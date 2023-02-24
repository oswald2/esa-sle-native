{-# LANGUAGE TemplateHaskell #-}
module SLE.State.RAFState
    ( SleRafCmd(..)
    , RAFVar
    , RAF
    , rafSII
    , rafState
    , rafStateStartTime
    , rafStateStopTime
    , rafStateRequestedQuality
    , rafInitiator
    , rafErrorFreeFrames
    , rafDeliveredFrames
    , rafFrameSyncLockStatus
    , rafSymbolSyncLockStatus
    , rafSubCarrierLockStatus
    , rafCarrierLockStatus
    , rafProductionStatus
    , newRAFVarIO
    , readRAFVarIO
    , readRAFVar
    , writeRAFVar
    , modifyRAF
    , getRAFState
    , setRAFState
    , rafResetState
    , rafVar
    , rafQueue
    , rafSleHandle
    , rafVarCfg
    , rafIdx
    , rafContinuity
    , rafPeers
    , rafSchedule
    , sendSleRafCmd
    , sendSlePdu
    , sendFrameOrNotification
    , rafSetInitiator
    , rafGetInitiator
    , rafGetPeer
    , rafSendStatusReport
    , rafStartSchedule
    , rafStopSchedule
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )
import qualified RIO.HashMap                   as HM

import           Control.Lens

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.PDU                   ( SlePdu(SlePduRafStatusReport)
                                                )
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps
import           SLE.Data.WriteCmd


data RAF = RAF
    { _rafSII                   :: !SII
    , _rafState                 :: !ServiceState
    , _rafStateStartTime        :: !ConditionalTime
    , _rafStateStopTime         :: !ConditionalTime
    , _rafStateRequestedQuality :: !ReqFrameQuality
    , _rafInitiator             :: !(Maybe Peer)
    , _rafErrorFreeFrames       :: !Word32
    , _rafDeliveredFrames       :: !Word32
    , _rafFrameSyncLockStatus   :: !LockStatus
    , _rafSymbolSyncLockStatus  :: !LockStatus
    , _rafSubCarrierLockStatus  :: !LockStatus
    , _rafCarrierLockStatus     :: !LockStatus
    , _rafProductionStatus      :: !RafProductionStatus
    }
makeLenses ''RAF

-- | Command to be sent to the RAF
data SleRafCmd = BindRaf

data RAFVar = RAFVar
    { _rafVar        :: !(TVar RAF)
    , _rafQueue      :: !(TBQueue SleRafCmd)
    , _rafSleHandle  :: !SleHandle
    , _rafVarCfg     :: !RAFConfig
    , _rafIdx        :: !RAFIdx
    , _rafContinuity :: !(TVar Int32)
    , _rafPeers      :: !(HashMap AuthorityIdentifier Peer)
    , _rafSchedule   :: !(TVar (Maybe (Async ())))
    }
makeLenses ''RAFVar

instance Show RAFVar where
    show x =
        "RAFVar { _rafIdx="
            ++ show (_rafIdx x)
            ++ ", _rafVarCfg="
            ++ show (_rafVarCfg x)
            ++ "}"

rafStartState :: RAFConfig -> RAF
rafStartState cfg = RAF { _rafSII                   = cfg ^. cfgRAFSII
                        , _rafState                 = ServiceInit
                        , _rafStateStartTime        = Nothing
                        , _rafStateStopTime         = Nothing
                        , _rafStateRequestedQuality = AllFrames
                        , _rafInitiator             = Nothing
                        , _rafErrorFreeFrames       = 0
                        , _rafDeliveredFrames       = 0
                        , _rafFrameSyncLockStatus   = InLock
                        , _rafSymbolSyncLockStatus  = InLock
                        , _rafSubCarrierLockStatus  = InLock
                        , _rafCarrierLockStatus     = InLock
                        , _rafProductionStatus      = ProdRunning
                        }


newRAFVarIO :: (MonadIO m) => CommonConfig -> RAFConfig -> RAFIdx -> m RAFVar
newRAFVarIO commonCfg cfg idx = do
    let raf = rafStartState cfg
    var   <- newTVarIO raf
    q     <- newTBQueueIO 100
    cont  <- newTVarIO (-1)
    hdl   <- newSleHandle (TMRAF idx) (cfg ^. cfgRAFBufferSize)
    sched <- newTVarIO Nothing
    return $! RAFVar var q hdl cfg idx cont (mkPeerSet commonCfg) sched


readRAFVarIO :: (MonadIO m) => RAFVar -> m RAF
readRAFVarIO var = readTVarIO (_rafVar var)

readRAFVar :: RAFVar -> STM RAF
readRAFVar var = readTVar (_rafVar var)

writeRAFVar :: RAFVar -> RAF -> STM ()
writeRAFVar var raf = writeTVar (_rafVar var) raf

getRAFState :: (MonadIO m) => RAFVar -> m ServiceState
getRAFState var = _rafState <$> readTVarIO (_rafVar var)

setRAFState :: (MonadIO m) => RAFVar -> ServiceState -> m ()
setRAFState var st = atomically $ do
    raf <- readTVar (_rafVar var)
    writeTVar (_rafVar var) (raf & rafState .~ st)


modifyRAF :: (MonadIO m) => RAFVar -> (RAF -> RAF) -> m ()
modifyRAF var f = atomically $ modifyRAFSTM var f

modifyRAFSTM :: RAFVar -> (RAF -> RAF) -> STM ()
modifyRAFSTM var f = do
    raf <- readTVar (_rafVar var)
    let !newst = f raf
    writeTVar (_rafVar var) newst


sendSleRafCmd :: (MonadIO m) => RAFVar -> SleRafCmd -> m ()
sendSleRafCmd var cmd = atomically $ writeTBQueue (_rafQueue var) cmd

sendSlePdu :: (MonadIO m) => RAFVar -> SleWrite -> m ()
sendSlePdu var input = writeSLE (_rafSleHandle var) input


sendFrameOrNotification :: (MonadIO m) => RAFVar -> FrameOrNotification -> m ()
sendFrameOrNotification var value = do
    let hdl = var ^. rafSleHandle
    case isFrameBad value of
        NoFrame    -> writeFrameOrNotification hdl value
        GoodFrame -> atomically $ do
            modifyRAFSTM var setPositive
            writeFrameOrNotificationSTM hdl value
        BadFrame -> atomically $ do
            modifyRAFSTM var setNegative
            writeFrameOrNotificationSTM hdl value
  where
    setPositive raf = raf & rafErrorFreeFrames +~ 1 & rafDeliveredFrames +~ 1
    setNegative raf = raf & rafDeliveredFrames +~ 1


rafResetState :: (MonadIO m) => RAFVar -> m ()
rafResetState var = do
    void $ rafStopSchedule var
    atomically $ do
        writeRAFVar var (rafStartState (var ^. rafVarCfg))

rafSetInitiator :: (MonadIO m) => RAFVar -> Maybe Peer -> m ()
rafSetInitiator var newAuthority = modifyRAF var f
    where f state = state & rafInitiator .~ newAuthority

rafGetInitiator :: (MonadIO m) => RAFVar -> m (Maybe Peer)
rafGetInitiator var = _rafInitiator <$> readRAFVarIO var

rafGetPeer :: RAFVar -> AuthorityIdentifier -> Maybe Peer
rafGetPeer var authority = HM.lookup authority (_rafPeers var)


rafSendStatusReport :: (MonadIO m) => RAFVar -> m ()
rafSendStatusReport var = do
    raf <- readRAFVarIO var
    let pdu = SLEPdu $ SlePduRafStatusReport RafStatusReport
            { _rstrCredentials          = Nothing
            , _rstrErrorFreeFrameNumber = raf ^. rafErrorFreeFrames
            , _rstrDeliveredFrameNumber = raf ^. rafDeliveredFrames
            , _rstrFrameSyncLockStatus  = raf ^. rafFrameSyncLockStatus
            , _rstrSymbolSyncLockStatus = raf ^. rafSymbolSyncLockStatus
            , _rstrSubcarrierLockStatus = raf ^. rafSubCarrierLockStatus
            , _rstrCarrierLockStatus    = raf ^. rafCarrierLockStatus
            , _rstrProductionStatus     = raf ^. rafProductionStatus
            }
    sendSlePdu var pdu


rafStopSchedule :: (MonadIO m) => RAFVar -> m Bool
rafStopSchedule var = do
    thr <- readTVarIO (_rafSchedule var)
    case thr of
        Nothing     -> return False
        Just thread -> do
            cancel thread
            atomically $ writeTVar (_rafSchedule var) Nothing
            return True

rafStartSchedule :: (MonadUnliftIO m) => RAFVar -> Word16 -> m ()
rafStartSchedule var secs = do
    void $ rafStopSchedule var
    thr <- async thread
    atomically $ writeTVar (_rafSchedule var) (Just thr)
  where
    thread = do
        rafSendStatusReport var
        threadDelay (fromIntegral secs * 1_000_000)
        thread
