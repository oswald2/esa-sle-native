module SLE.State.Classes
    ( HasTimer(..)
    , HasEventHandler(..)
    , HasCommonConfig(..)
    , HasUserConfig(..)
    , HasProviderConfig(..)
    , HasSleHandle(..)
    , sleRaiseEvent
    ) where

import           Control.Lens
import           RIO                     hiding ( Lens' )
import           System.Timer.Updatable

import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.Data.UserConfig
import           SLE.State.Events


class HasCommonConfig env where
  commonCfg :: Getter env CommonConfig

class HasCommonConfig env => HasProviderConfig env where
  providerCfg :: Getter env ProviderConfig

class HasCommonConfig env => HasUserConfig env where
  userCfg :: Getter env UserConfig


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


