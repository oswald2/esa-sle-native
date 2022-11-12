module State.Classes
    ( HasTimer(..)
    , HasEventHandler(..)
    , HasConfig(..)
    , HasSleHandle(..)
    , sleRaiseEvent
    ) where

import           Control.Lens
import           RIO                     hiding ( Lens' )
import           System.Timer.Updatable

import           Data.SLE.Config
import           Data.SLE.Handle
import           State.Events


class HasConfig env where
  getConfig :: Getter env Config

class HasTimer env where
  getTimerHBT :: Getter env (TVar (Maybe (Updatable ())))
  getTimerHBR :: Getter env (TVar (Maybe (Updatable ())))
  hbr :: Lens' env (TVar Int64)
  hbt :: Lens' env (TVar Int64)

class HasEventHandler env where
  sleRaiseEventIO :: env -> SleEvent -> IO ()

class HasSleHandle env where
  getHandle :: Getter env SleHandle


sleRaiseEvent :: (MonadIO m, HasEventHandler env) => env -> SleEvent -> m ()
sleRaiseEvent env event = liftIO $ sleRaiseEventIO env event


