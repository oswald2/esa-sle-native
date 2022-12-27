{-# LANGUAGE
  TemplateHaskell
#-}
module SLE.Data.Handle
    ( SleHandle
    , newSleHandle
    , withSleHandle
    , queueSize
    , writeSLE
    , writeFrameOrNotification
    , writeFrameOrNotificationSTM
    , readFrameOrNotifications
    , readSLEHandle
    , sleInput
    , slePort
    , sleBuffer
    ) where


import           RIO

import           Control.Lens

import           Network.Socket                 ( PortNumber )

import           SLE.Data.RAFOps
import           SLE.Data.TimedBuffer
import           SLE.Data.WriteCmd

data SleHandle = SleHandle
    { _sleInput  :: !(TBQueue SleWrite)
    , _sleBuffer :: !(TimedBuffer FrameOrNotification)
    , _slePort   :: !PortNumber
    }
makeLenses ''SleHandle

queueSize :: Natural
queueSize = 5000


newSleHandle :: (MonadIO m) => PortNumber -> Word32 -> m SleHandle
newSleHandle port bufSize = do
    inp <- newTBQueueIO queueSize
    buf <- liftIO $ newTimedBufferIO bufSize
    return $ SleHandle { _sleInput = inp, _sleBuffer = buf, _slePort = port }

withSleHandle
    :: (MonadUnliftIO m) => PortNumber -> Word32 -> (SleHandle -> m a) -> m a
withSleHandle port bufferSize process = do
    bracket (newSleHandle port bufferSize) (\_hdl -> return ()) process

writeSLE :: (MonadIO m) => SleHandle -> SleWrite -> m ()
writeSLE hdl inp = do
    atomically $ do
        writeTBQueue (_sleInput hdl) inp


readSLEHandle :: SleHandle -> STM SleWrite
readSLEHandle hdl = readTBQueue (_sleInput hdl)

writeFrameOrNotificationSTM :: SleHandle -> FrameOrNotification -> STM ()
writeFrameOrNotificationSTM hdl dat = writeTimedBufferSTM (_sleBuffer hdl) dat


writeFrameOrNotification
    :: (MonadIO m) => SleHandle -> FrameOrNotification -> m ()
writeFrameOrNotification hdl dat = writeTimedBuffer (_sleBuffer hdl) dat

readFrameOrNotifications
    :: (MonadIO m) => SleHandle -> Timeout -> m [FrameOrNotification]
readFrameOrNotifications hdl latency = readTimedBuffer latency (_sleBuffer hdl)
