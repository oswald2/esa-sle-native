module State.AppState
  ( AppState
  , initialState
  )
where

import           RIO
--import           Control.Lens
import           System.Timer.Updatable

import           State.Classes
import           State.SLEEvents

import           Data.SLE.TMLConfig
import           Data.SLE.SLEInput


data AppState = AppState {
  _appTimerHBT :: TVar (Maybe (Updatable ()))
  , _appTimerHBR :: TVar (Maybe (Updatable ()))
  , _appTMLConfig :: !TMLConfig
  , _appHeartBeat :: TVar Int64 
  , _appHeartBeatReceive :: TVar Int64
  , _appLogFunc :: !LogFunc
  , _appEventHandler :: !SleEventHandler
  , _appSleInput :: TBQueue SLEInput
  }


initialState
  :: (MonadIO m) 
  => TMLConfig 
  -> LogFunc 
  -> SleEventHandler 
  -> TBQueue SLEInput 
  -> m AppState
initialState cfg logFunc eventHandler queue = do
  var   <- liftIO $ newTVarIO Nothing
  var1  <- liftIO $ newTVarIO Nothing
  hbrec <- liftIO $ newTVarIO (fromIntegral (cfgHeartbeat cfg) 
                        * fromIntegral (cfgDeadFactor  cfg) * 1_000_000)
  hbtr <- liftIO $ newTVarIO (fromIntegral (cfgHeartbeat cfg) * 1_000_000)
  return $! AppState { _appTimerHBT     = var
                     , _appTimerHBR     = var1
                     , _appTMLConfig    = cfg
                     , _appLogFunc      = logFunc
                     , _appEventHandler = eventHandler
                     , _appSleInput     = queue
                     , _appHeartBeat    = hbtr
                     , _appHeartBeatReceive = hbrec
                     }


instance HasTimer AppState where
  getTimerHBT = lens _appTimerHBT (\c v -> c { _appTimerHBT = v })
  getTimerHBR = lens _appTimerHBR (\c v -> c { _appTimerHBR = v })
  hbr = lens _appHeartBeatReceive (\c v -> c { _appHeartBeatReceive = v })
  hbt = lens _appHeartBeat (\c v -> c { _appHeartBeat = v })

-- | Instance of the logging function for the global state
instance HasLogFunc AppState where
  logFuncL = lens _appLogFunc (\c lf -> c { _appLogFunc = lf })


instance HasEventHandler AppState where
  sleRaiseEvent = _appEventHandler


instance HasConfig AppState where
  getTMLConfig = lens _appTMLConfig (\c cfg -> c { _appTMLConfig = cfg })


instance HasSleInput AppState where 
  getInput = lens _appSleInput (\c inp -> c { _appSleInput = inp })