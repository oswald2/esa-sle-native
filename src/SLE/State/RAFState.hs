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
    , sendSleRafCmd
    , sendSlePdu
    , sendFrameOrNotification
    , rafSetInitiator
    , rafGetInitiator
    , rafGetPeer
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
                        , _rafInitiator             = Nothing
                        }


newRAFVarIO :: (MonadIO m) => CommonConfig -> RAFConfig -> RAFIdx -> m RAFVar
newRAFVarIO commonCfg cfg idx = do
    let raf = rafStartState cfg
    var  <- newTVarIO raf
    q    <- newTBQueueIO 100
    cont <- newTVarIO (-1)
    hdl  <- newSleHandle (TMRAF idx) (cfg ^. cfgRAFBufferSize)
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


modifyRAF :: (MonadIO m) => RAFVar -> (RAF -> RAF) -> m ()
modifyRAF var f = atomically $ do
    raf <- readTVar (_rafVar var)
    let !newst = f raf
    writeTVar (_rafVar var) newst

sendSleRafCmd :: (MonadIO m) => RAFVar -> SleRafCmd -> m ()
sendSleRafCmd var cmd = atomically $ writeTBQueue (_rafQueue var) cmd

sendSlePdu :: (MonadIO m) => RAFVar -> SleWrite -> m ()
sendSlePdu var input = writeSLE (_rafSleHandle var) input


sendFrameOrNotification :: (MonadIO m) => RAFVar -> FrameOrNotification -> m ()
sendFrameOrNotification var value =
    writeFrameOrNotification (var ^. rafSleHandle) value


rafResetState :: (MonadIO m) => RAFVar -> m ()
rafResetState var =
    atomically $ writeRAFVar var (rafStartState (var ^. rafVarCfg))

rafSetInitiator :: (MonadIO m) => RAFVar -> Maybe Peer -> m ()
rafSetInitiator var newAuthority = modifyRAF var f
    where f state = state & rafInitiator .~ newAuthority

rafGetInitiator :: (MonadIO m) => RAFVar -> m (Maybe Peer)
rafGetInitiator var = _rafInitiator <$> readRAFVarIO var

rafGetPeer :: RAFVar -> AuthorityIdentifier -> Maybe Peer
rafGetPeer var authority = HM.lookup authority (_rafPeers var)
