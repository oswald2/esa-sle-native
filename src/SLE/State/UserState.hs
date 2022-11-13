module SLE.State.UserState
    ( UserState
    , initialState
    , sleRaiseEvent
    ) where

import           RIO

import           System.Timer.Updatable

import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig
import           SLE.State.Classes
import           SLE.State.Events




data UserState = UserState
    { _appTimerHBT         :: TVar (Maybe (Updatable ()))
    , _appTimerHBR         :: TVar (Maybe (Updatable ()))
    , _appConfig           :: !UserConfig
    , _appHeartBeat        :: TVar Int64
    , _appHeartBeatReceive :: TVar Int64
    , _appLogFunc          :: !LogFunc
    , _appEventHandler     :: !SleEventHandler
    , _appSleHandle        :: SleHandle
    }


initialState
    :: (MonadIO m)
    => UserConfig
    -> LogFunc
    -> SleEventHandler
    -> SleHandle
    -> m UserState
initialState cfg logFunc eventHandler hdl = do
    var  <- liftIO $ newTVarIO Nothing
    var1 <- liftIO $ newTVarIO Nothing
    let tmlCfg = cfg ^. cfgCommon . cfgTML
    hbrec <- liftIO $ newTVarIO
        ( fromIntegral (cfgHeartbeat tmlCfg)
        * fromIntegral (cfgDeadFactor tmlCfg)
        * 1_000_000
        )
    hbtr <- liftIO $ newTVarIO (fromIntegral (cfgHeartbeat tmlCfg) * 1_000_000)
    return $! UserState { _appTimerHBT         = var
                        , _appTimerHBR         = var1
                        , _appConfig           = cfg
                        , _appLogFunc          = logFunc
                        , _appEventHandler     = eventHandler
                        , _appSleHandle        = hdl
                        , _appHeartBeat        = hbtr
                        , _appHeartBeatReceive = hbrec
                        }


instance HasTimer UserState where
    getTimerHBT = lens _appTimerHBT (\c v -> c { _appTimerHBT = v })
    getTimerHBR = lens _appTimerHBR (\c v -> c { _appTimerHBR = v })
    hbr = lens _appHeartBeatReceive (\c v -> c { _appHeartBeatReceive = v })
    hbt         = lens _appHeartBeat (\c v -> c { _appHeartBeat = v })

-- | Instance of the logging function for the global state
instance HasLogFunc UserState where
    logFuncL = lens _appLogFunc (\c lf -> c { _appLogFunc = lf })


instance HasEventHandler UserState where
    sleRaiseEventIO = _appEventHandler


instance HasCommonConfig UserState where
    commonCfg = userCfg . cfgCommon

instance HasUserConfig UserState where
    userCfg = lens _appConfig (\c cfg -> c { _appConfig = cfg })


instance HasSleHandle UserState where
    getHandle = lens _appSleHandle (\c inp -> c { _appSleHandle = inp })
