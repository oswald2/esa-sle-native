{-# LANGUAGE
  TemplateHaskell
#-}
module SLE.Data.Handle
    ( SleHandle
    , newSleHandle
    , newSleHandleBuf
    , withSleHandle
    , queueSize
    , writeSLE
    , readSLE
    , readSLEHandle
    , writeFrameOrNotification
    , writeFrameOrNotificationSTM
    , readFrameOrNotifications
    , sleInput
    , sleBuffer
    , sleIdx
    ) where


import           RIO                     hiding ( (^.) )

import           Control.Lens

import           Data.STM.TimedBuffer

import           SLE.Data.Common
import           SLE.Data.RAFOps
import           SLE.Data.WriteCmd
import           SLE.State.Classes


-- | An SleHandle. This contains the data structures (queues, timed buffer) to 
-- send SLE PDUs
data SleHandle = SleHandle
    { _sleInput  :: !(TBQueue SleWrite)
    , _sleBuffer :: !(TimedBuffer FrameOrNotification)
    , _sleIdx    :: !TMIdx
    }
makeLenses ''SleHandle

-- | Default queue size is 5000
queueSize :: Natural
queueSize = 5000


-- | Creates a new SLE Handle 
newSleHandle :: (MonadIO m) => TMIdx -> Word16 -> m SleHandle
newSleHandle idx bufSize = do
    inp <- newTBQueueIO queueSize
    buf <- liftIO $ newTimedBufferIO (fromIntegral bufSize)
    return $ SleHandle { _sleInput = inp, _sleBuffer = buf, _sleIdx = idx }

newSleHandleBuf
    :: (MonadIO m) => TMIdx -> TimedBuffer FrameOrNotification -> m SleHandle
newSleHandleBuf idx buf = do
    inp <- newTBQueueIO queueSize
    return $ SleHandle { _sleInput = inp, _sleBuffer = buf, _sleIdx = idx }


withSleHandle
    :: (MonadUnliftIO m) => TMIdx -> Word16 -> (SleHandle -> m a) -> m a
withSleHandle idx bufferSize process = do
    bracket (newSleHandle idx bufferSize) (\_hdl -> return ()) process

writeSLE :: (MonadIO m) => SleHandle -> SleWrite -> m ()
writeSLE hdl inp = do
    atomically $ do
        writeTBQueue (_sleInput hdl) inp

-- | Read input from the queue from the 'SleHandle'. If there is nothing received 
-- within the configured timeout (send heartbeat), returns 'Nothing'. This indicates
-- that a heartbeat message should be sent
readSLE
    :: (MonadIO m, MonadReader env m, HasTimer env)
    => SleHandle
    -> m (Maybe SleWrite)
readSLE hdl = do
    env   <- ask
    val   <- liftIO $ readTVarIO (env ^. hbt)
    delay <- registerDelay (fromIntegral val)
    atomically
        $   Just
        <$> readSLEHandle hdl
        <|> (readTVar delay >>= checkSTM >> pure Nothing)


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
