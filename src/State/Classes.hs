module State.Classes
  ( HasTimer(..)
  , HasEventHandler(..)
  , HasConfig(..)
  , HasSleInput(..)
  )
where

import           RIO
import           Control.Lens
import           System.Timer.Updatable

import           State.SLEEvents
import           Data.SLE.TMLConfig
import           Data.SLE.SLEInput              ( SLEInput )

class HasConfig env where
  getTMLConfig :: Getter env TMLConfig

class HasTimer env where
  getTimerHBT :: Getter env (TVar (Maybe (Updatable ())))
  getTimerHBR :: Getter env (TVar (Maybe (Updatable ())))

class HasEventHandler env where
  sleRaiseEvent :: env -> SleEvent -> IO ()

class HasSleInput env where
  getInput :: Getter env (TBQueue SLEInput)
