module State.AppState
    ( AppState
    , initialState
    , sleRaiseEvent
    ) where

import           RIO
--import           Control.Lens
import           System.Timer.Updatable

import           State.Classes
import           State.Events

import           Data.SLE.Config
--import           Data.SLE.Input
import           Data.SLE.Handle
import           Data.SLE.TMLConfig





data AppState = AppState
    { _appTimerHBT         :: TVar (Maybe (Updatable ()))
    , _appTimerHBR         :: TVar (Maybe (Updatable ()))
    , _appConfig           :: !Config
    , _appHeartBeat        :: TVar Int64
    , _appHeartBeatReceive :: TVar Int64
    , _appLogFunc          :: !LogFunc
    , _appEventHandler     :: !SleEventHandler
    , _appSleHandle        :: SleHandle
    }


initialState
    :: (MonadIO m)
    => Config
    -> LogFunc
    -> SleEventHandler
    -> SleHandle
    -> m AppState
initialState cfg logFunc eventHandler hdl = do
    var  <- liftIO $ newTVarIO Nothing
    var1 <- liftIO $ newTVarIO Nothing
    let tmlCfg = cfg ^. cfgTML
    hbrec <- liftIO $ newTVarIO
        ( fromIntegral (cfgHeartbeat tmlCfg)
        * fromIntegral (cfgDeadFactor tmlCfg)
        * 1_000_000
        )
    hbtr <- liftIO $ newTVarIO (fromIntegral (cfgHeartbeat tmlCfg) * 1_000_000)
    return $! AppState { _appTimerHBT         = var
                       , _appTimerHBR         = var1
                       , _appConfig           = cfg
                       , _appLogFunc          = logFunc
                       , _appEventHandler     = eventHandler
                       , _appSleHandle        = hdl
                       , _appHeartBeat        = hbtr
                       , _appHeartBeatReceive = hbrec
                       }


instance HasTimer AppState where
    getTimerHBT = lens _appTimerHBT (\c v -> c { _appTimerHBT = v })
    getTimerHBR = lens _appTimerHBR (\c v -> c { _appTimerHBR = v })
    hbr = lens _appHeartBeatReceive (\c v -> c { _appHeartBeatReceive = v })
    hbt         = lens _appHeartBeat (\c v -> c { _appHeartBeat = v })

-- | Instance of the logging function for the global state
instance HasLogFunc AppState where
    logFuncL = lens _appLogFunc (\c lf -> c { _appLogFunc = lf })


instance HasEventHandler AppState where
    sleRaiseEventIO = _appEventHandler


instance HasConfig AppState where
    getConfig = lens _appConfig (\c cfg -> c { _appConfig = cfg })


instance HasSleHandle AppState where
    getHandle = lens _appSleHandle (\c inp -> c { _appSleHandle = inp })
