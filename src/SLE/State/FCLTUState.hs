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
    , modifyFCLTU
    , sendSleFcltuCmd
    , sendSleFcltuPdu
    , fcltuSII
    , fcltuState
    , fcltuProdNotification
    , fcltuStartRadiationTime
    , fcltuVar
    , fcltuSleHandle
    , fcltuQueue
    , fcltuVarCfg
    , fcltuIdx
    , fcltuPeers
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )

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
    , _fcltuPeers     :: !(HashMap AuthorityIdentifier Peer)
    }
makeLenses ''FCLTUVar


fcltuStartState :: FCLTUConfig -> FCLTU
fcltuStartState cfg = FCLTU { _fcltuSII                = cfg ^. cfgFCLTUSII
                            , _fcltuState              = ServiceInit
                            , _fcltuProdNotification = DoNotProduceNotification
                            , _fcltuStartRadiationTime = ccsdsNullTime
                            }


newFCLTUVarIO
    :: (MonadIO m) => CommonConfig -> FCLTUConfig -> FCLTUIdx -> m FCLTUVar
newFCLTUVarIO commonCfg cfg idx = do
    let fcltu = fcltuStartState cfg
    var <- newTVarIO fcltu
    q   <- newTBQueueIO 100
    hdl <- newSleHandle (TCFCLTU idx) 1 -- buffer size is 1 as we don't use a buffer
    return $! FCLTUVar var q hdl cfg idx (mkPeerSet commonCfg)


readFCLTUVarIO :: (MonadIO m) => FCLTUVar -> m FCLTU
readFCLTUVarIO var = readTVarIO (_fcltuVar var)

readFCLTUVar :: FCLTUVar -> STM FCLTU
readFCLTUVar var = readTVar (_fcltuVar var)

writeFCLTUVar :: FCLTUVar -> FCLTU -> STM ()
writeFCLTUVar var raf = writeTVar (_fcltuVar var) raf

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

sendSleFcltuCmd :: (MonadIO m) => FCLTUVar -> SleFcltuCmd -> m ()
sendSleFcltuCmd var cmd = atomically $ writeTBQueue (_fcltuQueue var) cmd

sendSleFcltuPdu :: (MonadIO m) => FCLTUVar -> SleWrite -> m ()
sendSleFcltuPdu var input = writeSLE (_fcltuSleHandle var) input
