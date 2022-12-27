{-# LANGUAGE
  TemplateHaskell
#-}
module SLE.Data.Handle
    ( SleHandle
    , newSleHandle
    , withSleHandle
    , queueSize
    , writeSLE
    , readSLEHandle
    , sleInput
    , slePort
    ) where


import           RIO

import           Control.Lens

import           Network.Socket                 ( PortNumber )

import           SLE.Data.WriteCmd



data SleHandle = SleHandle
    { _sleInput :: TBQueue SleWrite
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

writeSLE :: (MonadIO m) => SleHandle -> SleWrite -> m ()
writeSLE hdl inp = do
    atomically $ do
        writeTBQueue (_sleInput hdl) inp


readSLEHandle :: SleHandle -> STM SleWrite
readSLEHandle hdl = readTBQueue (_sleInput hdl)
