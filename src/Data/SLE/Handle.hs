{-# LANGUAGE
  TemplateHaskell
#-}
module Data.SLE.Handle
(
  SleHandle 
  , newSleHandle 
  , queueSize
  , sleInput
  , writeSLEInput
)
where


import RIO
import Control.Lens 

import Data.SLE.Input


newtype SleHandle = SleHandle {
  _sleInput :: TBQueue SleInput 
  }
makeLenses ''SleHandle

queueSize :: Natural 
queueSize = 5000


newSleHandle :: IO SleHandle 
newSleHandle = do 
  inp <- newTBQueueIO queueSize 
  return $ SleHandle {
    _sleInput = inp
    }



writeSLEInput :: (MonadIO m) => SleHandle -> SleInput -> m () 
writeSLEInput hdl inp = do 
  atomically $ do 
    writeTBQueue (_sleInput hdl) inp