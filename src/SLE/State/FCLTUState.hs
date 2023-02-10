{-# LANGUAGE TemplateHaskell #-}
module SLE.State.FCLTUState
    ( FCLTU
    , FCLTUVar
    , SleFcltuCmd(..)
    , newFCLTUVarIO
    , readFCLTUVar
    , readFCLTUVarIO
    , writeFCLTUVar
    , getFCLTUState
    , setFCLTUState
    , modifyFCLTUState
    , modifyFCLTU
    , sendSleFcltuCmd
    , sendSleFcltuPdu
    , setCltuID
    , fcltuResetState
    , fcltuSII
    , fcltuState
    , fcltuProdNotification
    , fcltuStartRadiationTime
    , fcltuVar
    , fcltuSleHandle
    , fcltuQueue
    , fcltuVarCfg
    , fcltuIdx
    , fcltuTMIdx
    , fcltuPeers
    , fcltuCltuID
    , fcltuLastProcessed
    , fcltuLastOK
    , fcltuProdStatus
    , fcltuCltusReceived
    , fcltuCltusProcessed
    , fcltuCltusRadiated
    , fcltuInitiator
    , fcltuSetInitiator
    , fcltuGetInitiator
    , fcltuGetPeer
    ) where

import           RIO
import qualified RIO.HashMap                   as HM

import           Control.Lens                   ( (+~)
                                                , makeLenses
                                                )

import           SLE.Data.Bind
import           SLE.Data.CCSDSTime
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.Data.WriteCmd

data FCLTU = FCLTU
    { _fcltuSII                :: !SII
    , _fcltuState              :: !ServiceState
    , _fcltuProdNotification   :: !SlduStatusNotification
    , _fcltuStartRadiationTime :: !CCSDSTime
    , _fcltuCltuID             :: !CltuIdentification
    , _fcltuLastProcessed      :: !CltuLastProcessed
    , _fcltuLastOK             :: !CltuLastOk
    , _fcltuProdStatus         :: !ProductionStatus
    , _fcltuCltusReceived      :: !Word64
    , _fcltuCltusProcessed     :: !Word64
    , _fcltuCltusRadiated      :: !Word64
    , _fcltuInitiator          :: !(Maybe Peer)
    }
makeLenses ''FCLTU


-- | Command to be sent to the FCLTU
data SleFcltuCmd = BindFcltu


data FCLTUVar = FCLTUVar
    { _fcltuVar       :: !(TVar FCLTU)
    , _fcltuQueue     :: !(TBQueue SleFcltuCmd)
    , _fcltuSleHandle :: !SleHandle
    , _fcltuVarCfg    :: !FCLTUConfig
    , _fcltuIdx       :: !FCLTUIdx
    , _fcltuTMIdx     :: !TMIdx
    , _fcltuPeers     :: !(HashMap AuthorityIdentifier Peer)
    }
makeLenses ''FCLTUVar


fcltuStartState :: FCLTUConfig -> FCLTU
fcltuStartState cfg = FCLTU { _fcltuSII                = cfg ^. cfgFCLTUSII
                            , _fcltuState              = ServiceInit
                            , _fcltuProdNotification = DoNotProduceNotification
                            , _fcltuStartRadiationTime = ccsdsNullTime
                            , _fcltuCltuID             = CltuIdentification 0
                            , _fcltuLastProcessed      = NoCltuProcessed
                            , _fcltuLastOK             = NoCltuOk
                            , _fcltuProdStatus         = ProdOperational
                            , _fcltuCltusReceived      = 0
                            , _fcltuCltusProcessed     = 0
                            , _fcltuCltusRadiated      = 0
                            , _fcltuInitiator          = Nothing
                            }


newFCLTUVarIO
    :: (MonadIO m)
    => CommonConfig
    -> FCLTUConfig
    -> FCLTUIdx
    -> TMIdx
    -> m FCLTUVar
newFCLTUVarIO commonCfg cfg idx tmIdx = do
    let fcltu = fcltuStartState cfg
    var <- newTVarIO fcltu
    q   <- newTBQueueIO 100
    hdl <- newSleHandle (TCFCLTU idx) 1 -- buffer size is 1 as we don't use a buffer
    return $! FCLTUVar var q hdl cfg idx tmIdx (mkPeerSet commonCfg)


readFCLTUVarIO :: (MonadIO m) => FCLTUVar -> m FCLTU
readFCLTUVarIO var = readTVarIO (_fcltuVar var)

readFCLTUVar :: FCLTUVar -> STM FCLTU
readFCLTUVar var = readTVar (_fcltuVar var)

writeFCLTUVar :: FCLTUVar -> FCLTU -> STM ()
writeFCLTUVar var fcltu = writeTVar (_fcltuVar var) fcltu

getFCLTUState :: (MonadIO m) => FCLTUVar -> m ServiceState
getFCLTUState var = _fcltuState <$> readTVarIO (_fcltuVar var)

setFCLTUState :: (MonadIO m) => FCLTUVar -> ServiceState -> m ()
setFCLTUState var st = atomically $ do
    fcltu <- readTVar (_fcltuVar var)
    writeTVar (_fcltuVar var) (fcltu & fcltuState .~ st)


modifyFCLTU :: (MonadIO m) => FCLTUVar -> (FCLTU -> FCLTU) -> m ()
modifyFCLTU var f = atomically $ do
    fcltu <- readTVar (_fcltuVar var)
    let !newst = f fcltu
    writeTVar (_fcltuVar var) newst

modifyFCLTUState :: (MonadIO m) => FCLTUVar -> (FCLTU -> FCLTU) -> m FCLTU
modifyFCLTUState var f = atomically $ do
    fcltu <- readTVar (_fcltuVar var)
    let !newst = f fcltu
    writeTVar (_fcltuVar var) newst
    return newst


sendSleFcltuCmd :: (MonadIO m) => FCLTUVar -> SleFcltuCmd -> m ()
sendSleFcltuCmd var cmd = atomically $ writeTBQueue (_fcltuQueue var) cmd

sendSleFcltuPdu :: (MonadIO m) => FCLTUVar -> SleWrite -> m ()
sendSleFcltuPdu var input = writeSLE (_fcltuSleHandle var) input


setCltuID
    :: (MonadIO m) => FCLTUVar -> CltuIdentification -> m SlduStatusNotification
setCltuID var cltuID = _fcltuProdNotification <$> modifyFCLTUState var update
    where update st = st & fcltuCltuID .~ cltuID & fcltuCltusReceived +~ 1

fcltuResetState :: (MonadIO m) => FCLTUVar -> m ()
fcltuResetState var =
    atomically $ writeFCLTUVar var (fcltuStartState (var ^. fcltuVarCfg))

fcltuSetInitiator :: (MonadIO m) => FCLTUVar -> Maybe Peer -> m ()
fcltuSetInitiator var newAuthority = modifyFCLTU var f
    where f state = state & fcltuInitiator .~ newAuthority

fcltuGetInitiator :: (MonadIO m) => FCLTUVar -> m (Maybe Peer)
fcltuGetInitiator var = _fcltuInitiator <$> readFCLTUVarIO var

fcltuGetPeer :: FCLTUVar -> AuthorityIdentifier -> Maybe Peer
fcltuGetPeer var authority = HM.lookup authority (_fcltuPeers var)
