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
    , newRAFVarIO
    , readRAFVarIO
    , readRAFVar
    , writeRAFVar
    , getRAFState
    , setRAFState
    , rafVar
    , rafQueue
    , rafSleHandle
    , rafVarCfg
    , rafIdx
    , rafContinuity
    , rafPeers
    , sendSleRafCmd
    , sendSlePdu
    , sendFrameOrNotification
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )

import           Control.Lens

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps
import           SLE.Data.WriteCmd

data RAF = RAF
    { _rafSII                   :: !SII
    , _rafState                 :: !ServiceState
    , _rafStateStartTime        :: !ConditionalTime
    , _rafStateStopTime         :: !ConditionalTime
    , _rafStateRequestedQuality :: !ReqFrameQuality
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
    }
makeLenses ''RAFVar

rafStartState :: RAFConfig -> RAF
rafStartState cfg = RAF { _rafSII                   = cfg ^. cfgRAFSII
                        , _rafState                 = ServiceInit
                        , _rafStateStartTime        = Nothing
                        , _rafStateStopTime         = Nothing
                        , _rafStateRequestedQuality = AllFrames
                        }


newRAFVarIO :: (MonadIO m) => CommonConfig -> RAFConfig -> RAFIdx -> m RAFVar
newRAFVarIO commonCfg cfg idx = do
    let raf = rafStartState cfg
    var  <- newTVarIO raf
    q    <- newTBQueueIO 100
    cont <- newTVarIO (-1)
    hdl  <- newSleHandle (fromIntegral (cfg ^. cfgRAFPort))
                         (cfg ^. cfgRAFBufferSize)
    return $! RAFVar var q hdl cfg idx cont (mkPeerSet commonCfg)


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

sendSleRafCmd :: (MonadIO m) => RAFVar -> SleRafCmd -> m ()
sendSleRafCmd var cmd = atomically $ writeTBQueue (_rafQueue var) cmd

sendSlePdu :: (MonadIO m) => RAFVar -> SleWrite -> m ()
sendSlePdu var input = writeSLE (_rafSleHandle var) input


sendFrameOrNotification :: (MonadIO m) => RAFVar -> FrameOrNotification -> m ()
sendFrameOrNotification var value =
    writeFrameOrNotification (var ^. rafSleHandle) value
