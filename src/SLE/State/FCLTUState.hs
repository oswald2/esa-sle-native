{-# LANGUAGE TemplateHaskell #-}
module SLE.State.FCLTUState
    ( FCLTU
    , ConfigFromApp(..)
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
    , fcltuSchedule
    , fcltuCltuID
    , fcltuLastProcessed
    , fcltuLastOK
    , fcltuProdStatus
    , fcltuUplinkStatus
    , fcltuCltusReceived
    , fcltuCltusProcessed
    , fcltuCltusRadiated
    , fcltuInitiator
    , fcltuBitLock
    , fcltuSetInitiator
    , fcltuGetInitiator
    , fcltuGetPeer
    , fcltuSendStatusReport
    , fcltuStopSchedule
    , fcltuStartSchedule
    , fcltuGetReportSchedule
    , fcltuSetBitLock
    , fcltuSCID
    , fcltuVCID
    , fcltuSetVCID
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
import           SLE.Data.PDU
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
    , _fcltuUplinkStatus       :: !UplinkStatus
    , _fcltuCltusReceived      :: !Word64
    , _fcltuCltusProcessed     :: !Word64
    , _fcltuCltusRadiated      :: !Word64
    , _fcltuInitiator          :: !(Maybe Peer)
    , _fcltuBitLock            :: !Bool
    , _fcltuSCID               :: !Word16
    , _fcltuVCID               :: !Word8
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
    , _fcltuSchedule  :: !(TVar (Maybe (Async (), Word16)))
    }
makeLenses ''FCLTUVar


fcltuStartState :: FCLTUConfig -> ConfigFromApp -> FCLTU
fcltuStartState cfg appCfg = FCLTU
    { _fcltuSII                = cfg ^. cfgFCLTUSII
    , _fcltuState              = ServiceInit
    , _fcltuProdNotification   = ProduceNotification
    , _fcltuStartRadiationTime = ccsdsNullTime
    , _fcltuCltuID             = CltuIdentification 0
    , _fcltuLastProcessed      = NoCltuProcessed
    , _fcltuLastOK             = NoCltuOk
    , _fcltuProdStatus         = ProdOperational
    , _fcltuUplinkStatus       = UplinkNominal
    , _fcltuCltusReceived      = 0
    , _fcltuCltusProcessed     = 0
    , _fcltuCltusRadiated      = 0
    , _fcltuInitiator          = Nothing
    , _fcltuBitLock            = True
    , _fcltuSCID               = appSCID appCfg
    , _fcltuVCID               = appVCID appCfg
    }


newFCLTUVarIO
    :: (MonadIO m)
    => CommonConfig
    -> FCLTUConfig
    -> FCLTUIdx
    -> TMIdx
    -> ConfigFromApp
    -> m FCLTUVar
newFCLTUVarIO commonCfg cfg idx tmIdx appCfg = do
    let fcltu = fcltuStartState cfg appCfg
    var   <- newTVarIO fcltu
    q     <- newTBQueueIO 100
    hdl   <- newSleHandle (TCFCLTU idx) 1 -- buffer size is 1 as we don't use a buffer
    sched <- newTVarIO Nothing
    return $! FCLTUVar var q hdl cfg idx tmIdx (mkPeerSet commonCfg) sched


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

fcltuResetState :: (MonadIO m) => FCLTUVar -> ConfigFromApp -> m ()
fcltuResetState var appCfg =
    atomically $ writeFCLTUVar var (fcltuStartState (var ^. fcltuVarCfg) appCfg)

fcltuSetInitiator :: (MonadIO m) => FCLTUVar -> Maybe Peer -> m ()
fcltuSetInitiator var newAuthority = modifyFCLTU var f
    where f state = state & fcltuInitiator .~ newAuthority

fcltuGetInitiator :: (MonadIO m) => FCLTUVar -> m (Maybe Peer)
fcltuGetInitiator var = _fcltuInitiator <$> readFCLTUVarIO var

fcltuGetPeer :: FCLTUVar -> AuthorityIdentifier -> Maybe Peer
fcltuGetPeer var authority = HM.lookup authority (_fcltuPeers var)


fcltuSendStatusReport :: (MonadIO m) => FCLTUVar -> m ()
fcltuSendStatusReport var = do
    fcltu <- readFCLTUVarIO var
    let
        pdu = SLEPdu $ SlePduFcltuStatusReport CltuStatusReport
            { _fcltuStatusCredentials      = Nothing
            , _fcltuStatusLastProcessed    = fcltu ^. fcltuLastProcessed
            , _fcltuStatusLastOk           = fcltu ^. fcltuLastOK
            , _fcltuStatusProductionStatus = fcltu ^. fcltuProdStatus
            , _fcltuStatusUplinkStatus     = fcltu ^. fcltuUplinkStatus
            , _fcltuStatusNumReceived      = fcltu ^. fcltuCltusReceived
            , _fcltuStatusNumProcessed     = fcltu ^. fcltuCltusProcessed
            , _fcltuStatusNumRadiated      = fcltu ^. fcltuCltusRadiated
            , _fcltuStatusBufferAvailable  = fromIntegral (maxBound :: Int32)
            }
    sendSleFcltuPdu var pdu


fcltuStopSchedule :: (MonadIO m) => FCLTUVar -> m Bool
fcltuStopSchedule var = do
    thr <- readTVarIO (_fcltuSchedule var)
    case thr of
        Nothing          -> return False
        Just (thread, _) -> do
            cancel thread
            atomically $ writeTVar (_fcltuSchedule var) Nothing
            return True

fcltuStartSchedule :: (MonadUnliftIO m) => FCLTUVar -> Word16 -> m ()
fcltuStartSchedule var secs = do
    void $ fcltuStopSchedule var
    thr <- async thread
    atomically $ writeTVar (_fcltuSchedule var) (Just (thr, secs))
  where
    thread = do
        fcltuSendStatusReport var
        threadDelay (fromIntegral secs * 1_000_000)
        thread

fcltuGetReportSchedule :: (MonadIO m) => FCLTUVar -> m (Maybe Word16)
fcltuGetReportSchedule var = do
    val <- readTVarIO (_fcltuSchedule var)
    case val of
        Nothing         -> return Nothing
        Just (_, sched) -> return (Just sched)


fcltuSetBitLock :: (MonadIO m) => FCLTUVar -> Bool -> m ()
fcltuSetBitLock var val = do
    modifyFCLTU var (\s -> s & fcltuBitLock .~ val)

fcltuSetVCID :: (MonadIO m) => FCLTUVar -> Word8 -> m ()
fcltuSetVCID var val = do
    modifyFCLTU var (\s -> s & fcltuVCID .~ val)
