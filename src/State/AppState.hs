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



data AppState = AppState {
  _appTimerHBT :: TVar (Maybe (Updatable ()))
  , _appTimerHBR :: TVar (Maybe (Updatable ()))
  , _appTMLConfig :: !TMLConfig
  , _appLogFunc :: !LogFunc
  , _appEventHandler :: !SleEventHandler
  }


initialState :: (MonadIO m) => TMLConfig -> LogFunc -> SleEventHandler -> m AppState
initialState cfg logFunc eventHandler = do
  var <- liftIO $ newTVarIO Nothing
  var1 <- liftIO $ newTVarIO Nothing
  return $! AppState {
    _appTimerHBT = var 
    , _appTimerHBR = var1 
    , _appTMLConfig = cfg
    , _appLogFunc = logFunc 
    , _appEventHandler = eventHandler
    }


instance HasTimer AppState where
  getTimerHBT = lens _appTimerHBT (\c v -> c { _appTimerHBT = v })
  getTimerHBR = lens _appTimerHBR (\c v -> c { _appTimerHBR = v })


-- | Instance of the logging function for the global state
instance HasLogFunc AppState where
  logFuncL = lens _appLogFunc (\c lf -> c { _appLogFunc = lf })


instance HasEventHandler AppState where
  sleRaiseEvent = _appEventHandler


instance HasConfig AppState where 
  getTMLConfig = lens _appTMLConfig (\c cfg -> c { _appTMLConfig = cfg })