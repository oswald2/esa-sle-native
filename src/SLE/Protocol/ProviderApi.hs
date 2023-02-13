module SLE.Protocol.ProviderApi
    ( rafSendFrame
    , rafSendFrameIdx
    -- , rcfSendFrame
    , fcltuStartRadiation
    , fcltuRadiationSuccess
    , fcltuRadiationFailure
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )

import           Control.Lens

import           SLE.Data.Common
import           SLE.Data.FCLTUOps
import           SLE.Data.Handle
import           SLE.Data.PDU
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps
import           SLE.Data.WriteCmd
import           SLE.State.FCLTUState
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



-- rcfSendFrame
--     :: (MonadIO m, MonadReader env m, HasRAF env)
--     => TimedBuffer FrameOrNotification
--     -> RAFIdx
--     -> Time
--     -> FrameQuality
--     -> Word8
--     -> ByteString
--     -> m ()
-- rcfSendFrame _buffer _idx _ert _quality _vcid _frame = return ()



fcltuStartRadiation
    :: (MonadIO m) => FCLTUVar -> CltuIdentification -> Time -> m ()
fcltuStartRadiation var cltuID radStart = do
    void $ modifyFCLTUState var update
  where
    update st =
        st
            &  fcltuLastProcessed
            .~ CltuProcessed cltuID (Just radStart) FwDUProductionStarted
            &  fcltuCltusProcessed
            +~ 1


fcltuRadiationSuccess
    :: (MonadIO m)
    => FCLTUVar
    -> CltuIdentification
    -> Time
    -> Time
    -> UplinkStatus
    -> m ()
fcltuRadiationSuccess var cltuID radStart radStop uplinkStatus = do
    state <- modifyFCLTUState var update

    case state ^. fcltuProdNotification of
        ProduceNotification ->
            fcltuSendAsync var state CltuRadiated uplinkStatus
        DoNotProduceNotification -> return ()

  where
    update st =
        st
            &  fcltuLastProcessed
            .~ CltuProcessed cltuID (Just radStart) FwDURadiated
            &  fcltuLastOK
            .~ CltuOk cltuID radStop
            &  fcltuCltusRadiated
            +~ 1

fcltuRadiationFailure
    :: (MonadIO m)
    => FCLTUVar
    -> CltuIdentification
    -> ConditionalTime
    -> CltuNotification
    -> ForwardDuStatus
    -> UplinkStatus
    -> m ()
fcltuRadiationFailure var cltuID tim notif duStatus uplinkStatus = do
    state <- modifyFCLTUState var update
    fcltuSendAsync var state notif uplinkStatus
  where
    update st =
        st
            &  fcltuLastProcessed
            .~ CltuProcessed cltuID tim duStatus
            &  fcltuCltusProcessed
            +~ 1


fcltuSendAsync
    :: (MonadIO m)
    => FCLTUVar
    -> FCLTU
    -> CltuNotification
    -> UplinkStatus
    -> m ()
fcltuSendAsync var state notif uplinkStatus = do
    let pdu = FcltuAsyncNotify
            { _fcltuAsyncCredentials      = Nothing
            , _fcltuAsyncNotification     = notif
            , _fcltuAsyncLastProcessed    = state ^. fcltuLastProcessed
            , _fcltuAsyncLastOK           = state ^. fcltuLastOK
            , _fcltuAsyncProductionStatus = state ^. fcltuProdStatus
            , _fcltuAsyncUplinkStatus     = uplinkStatus
            }
    sendSleFcltuPdu var (SLEPdu (SlePduFcltuAsync pdu))
