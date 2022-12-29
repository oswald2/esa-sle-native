module SLE.Protocol.ProviderApi
    ( rafSendFrame
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
    -> ByteString
    -> m ()
rafSendFrame idx ert frame = do
    env <- ask

    let var     = getRAFVar env idx
        cfg     = var ^. rafVarCfg
        antenna = cfg ^. cfgRAFAntennaID

    (raf, cont) <- atomically $ do
        raf <- readTVar (var ^. rafVar)
        val <- readTVar (var ^. rafContinuity) 
        writeTVar (var ^. rafContinuity) 0 
        return (raf, val)

    let pdu = force $ TransFrame RafTransferDataInvocation
            { _rafTransCredentials       = Nothing
            , _rafTransERT               = ert
            , _rafTransAntennaID         = antenna
            , _rafTransDataContinuity    = cont
            , _rafTransFrameQuality      = raf ^. rafStateRequestedQuality
            , _rafTransPrivateAnnotation = Nothing
            , _rafTransData              = frame
            }

    sendFrameOrNotification var pdu
