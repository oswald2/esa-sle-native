{-# LANGUAGE
  TemplateHaskell
#-}
module SLE.Data.Handle
    ( SleHandle
    , newSleHandle
    , withSleHandle
    , queueSize
    , writeSLEInput
    , readSLEHandle
    , sleInput
    , slePort
    ) where


import           RIO

import           Control.Lens

import           Network.Socket                 ( PortNumber )

import           SLE.Data.Input



data SleHandle = SleHandle
    { _sleInput :: TBQueue SleInput
    , _slePort  :: !PortNumber
    }
makeLenses ''SleHandle

queueSize :: Natural
queueSize = 5000


newSleHandle :: (MonadIO m) => PortNumber -> m SleHandle
newSleHandle port = do
    inp <- newTBQueueIO queueSize
    return $ SleHandle { _sleInput = inp, _slePort = port }

withSleHandle :: (MonadUnliftIO m) => PortNumber -> (SleHandle -> m a) -> m a
withSleHandle port process = do
    bracket (newSleHandle port) (\_hdl -> return ()) process

writeSLEInput :: (MonadIO m) => SleHandle -> SleInput -> m ()
writeSLEInput hdl inp = do
    atomically $ do
        writeTBQueue (_sleInput hdl) inp


readSLEHandle :: SleHandle -> STM SleInput
readSLEHandle hdl = readTBQueue (_sleInput hdl)
