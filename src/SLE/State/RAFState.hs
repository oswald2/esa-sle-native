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
    , sendSleRafCmd
    , sendSlePdu
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )

import           Control.Lens

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.Data.WriteCmd
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps


data RAF = RAF
    { _rafSII                   :: !SII
    , _rafState                 :: !ServiceState
    , _rafStateStartTime        :: !ConditionalTime
    , _rafStateStopTime         :: !ConditionalTime
    , _rafStateRequestedQuality :: !FrameQuality
    }
makeLenses ''RAF

-- | Command to be sent to the RAF
data SleRafCmd = BindRaf

data RAFVar = RAFVar
    { _rafVar       :: !(TVar RAF)
    , _rafQueue     :: !(TBQueue SleRafCmd)
    , _rafSleHandle :: !SleHandle
    , _rafVarCfg    :: !RAFConfig
    }
makeLenses ''RAFVar

rafStartState :: RAFConfig -> RAF
rafStartState cfg = RAF { _rafSII                   = cfg ^. cfgRAFSII
                        , _rafState                 = ServiceInit
                        , _rafStateStartTime        = Nothing
                        , _rafStateStopTime         = Nothing
                        , _rafStateRequestedQuality = AllFrames
                        }


newRAFVarIO :: (MonadIO m) => RAFConfig -> m RAFVar
newRAFVarIO cfg = do
    let raf = rafStartState cfg
    var <- newTVarIO raf
    q   <- newTBQueueIO 100
    hdl <- newSleHandle (fromIntegral (cfg ^. cfgRAFPort))
    return $! (RAFVar var q hdl cfg)


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
