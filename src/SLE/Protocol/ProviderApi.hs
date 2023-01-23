module SLE.Protocol.ProviderApi
    ( rafSendFrame
    , rafSendFrameIdx
    , rcfSendFrame
    ) where

import           RIO

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps
import           SLE.State.RAFClasses
import           SLE.State.RAFState

import           Data.STM.TimedBuffer


rafSendFrameIdx
    :: (MonadIO m, MonadReader env m, HasRAF env)
    => RAFIdx
    -> Time
    -> FrameQuality
    -> ByteString
    -> m ()
rafSendFrameIdx idx ert quality frame = do
    var' <- getRAFVar idx

    forM_ var' $ \var -> do
        let cfg     = var ^. rafVarCfg
            antenna = cfg ^. cfgRAFAntennaID

        (_raf, cont) <- atomically $ do
            raf <- readTVar (var ^. rafVar)
            val <- readTVar (var ^. rafContinuity)
            writeTVar (var ^. rafContinuity) 0
            return (raf, val)

        let pdu = force $ TransFrame RafTransferDataInvocation
                { _rafTransCredentials       = Nothing
                , _rafTransERT               = ert
                , _rafTransAntennaID         = antenna
                , _rafTransDataContinuity    = cont
                , _rafTransFrameQuality      = quality
                , _rafTransPrivateAnnotation = Nothing
                , _rafTransData              = frame
                }

        sendFrameOrNotification var pdu

rafSendFrame
    :: (MonadIO m) => RAFVar -> Time -> FrameQuality -> ByteString -> m ()
rafSendFrame var ert quality frame = do
    let cfg     = var ^. rafVarCfg
        antenna = cfg ^. cfgRAFAntennaID

    (_raf, cont) <- atomically $ do
        raf <- readTVar (var ^. rafVar)
        val <- readTVar (var ^. rafContinuity)
        writeTVar (var ^. rafContinuity) 0
        return (raf, val)

    let pdu = force $ TransFrame RafTransferDataInvocation
            { _rafTransCredentials       = Nothing
            , _rafTransERT               = ert
            , _rafTransAntennaID         = antenna
            , _rafTransDataContinuity    = cont
            , _rafTransFrameQuality      = quality
            , _rafTransPrivateAnnotation = Nothing
            , _rafTransData              = frame
            }

    writeTimedBuffer (var ^. rafSleHandle . sleBuffer) pdu



rcfSendFrame
    :: (MonadIO m, MonadReader env m, HasRAF env)
    => TimedBuffer FrameOrNotification
    -> RAFIdx
    -> Time
    -> FrameQuality
    -> Word8
    -> ByteString
    -> m ()
rcfSendFrame _buffer _idx _ert _quality _vcid _frame = return ()
