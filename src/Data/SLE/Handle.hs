{-# LANGUAGE
  TemplateHaskell
#-}
module Data.SLE.Handle
  ( SleHandle
  , newSleHandle
  , queueSize
  , writeSLEInput
  , sleInput
  , slePort
  )
where


import           RIO
import           Control.Lens

import           Data.SLE.Input
import           Network.Socket         ( PortNumber )

data SleHandle = SleHandle {
  _sleInput :: TBQueue SleInput
  , _slePort :: !PortNumber
  }
makeLenses ''SleHandle

queueSize :: Natural
queueSize = 5000


newSleHandle :: PortNumber -> IO SleHandle
newSleHandle port = do
  inp <- newTBQueueIO queueSize
  return $ SleHandle { 
      _sleInput = inp 
      , _slePort = port
      }



writeSLEInput :: (MonadIO m) => SleHandle -> SleInput -> m ()
writeSLEInput hdl inp = do
  atomically $ do
    writeTBQueue (_sleInput hdl) inp
