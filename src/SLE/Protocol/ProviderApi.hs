module SLE.Protocol.ProviderApi
    ( rafSendFrame
    , rcfSendFrame
    ) where

import           RIO

import           SLE.Data.Common
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps
import           SLE.State.RAFClasses
import           SLE.State.RAFState




rafSendFrame
    :: (MonadIO m, MonadReader env m, HasRAF env)
    => RAFIdx
    -> Time
    -> FrameQuality
    -> ByteString
    -> m ()
rafSendFrame idx ert quality frame = do
    env <- ask

    let var     = getRAFVar env idx
        cfg     = var ^. rafVarCfg
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


rcfSendFrame
    :: (MonadIO m, MonadReader env m, HasRAF env)
    => RCFIdx
    -> Time
    -> FrameQuality
    -> ByteString
    -> Word8
    -> m ()
rcfSendFrame _idx _ert _quality _frame _vcid = return ()
