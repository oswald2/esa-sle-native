module SLE.Data.Api
    ( SleHandle
    , withSleHandle
    , bind
    , unbind
    , startRAF
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.Input
import           SLE.Data.PDU
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID

-- import           Text.Builder





bind
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> ApplicationIdentifier
    -> PortID
    -> [ServiceInstanceAttribute]
    -> m ()
bind cfg hdl appID port attrs = do
  -- create an SLE Bind Invocation
    let bnd = mkSleBindInvocation (cfg ^. cfgInitiator)
                                  port
                                  appID
                                  (cfg ^. cfgVersion)
                                  (ServiceInstanceIdentifier attrs)
    -- send it to the lower layers
    writeSLEInput hdl (SLEPdu (SlePduBind bnd))



unbind :: (MonadIO m) => CommonConfig -> SleHandle -> UnbindReason -> m ()
unbind _cfg hdl reason = do
    let pdu = mkSleUnbindBindInvocation reason
    writeSLEInput hdl (SLEPdu (SlePduUnbind pdu))


startRAF
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Word16
    -> ConditionalTime
    -> ConditionalTime
    -> FrameQuality
    -> m ()
startRAF _cfg hdl invokeID start stop quality = do
    let pdu = RafStartInvocation { _rafStartCredentials       = Nothing
                                 , _rafStartInvokeID          = invokeID
                                 , _rafStartTime              = start
                                 , _rafStopTime               = stop
                                 , _rafStartRequestedTimeQual = quality
                                 }
    writeSLEInput hdl (SLEPdu (SlePduRafStart pdu))


-- sendFrame :: (Monad m) => SleHandle -> ByteString -> m ()
-- sendFrame = undefined

-- sendOCF :: (Monad m) => SleHandle -> Word32 -> m ()
-- sendOCF = undefined


-- receiveFrame :: (Monad m) => SleHandle -> m ByteString
-- receiveFrame = undefined

-- receiveOCF :: (Monad m) => SleHandle -> m Word32
-- receiveOCF = undefined



