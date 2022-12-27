module SLE.Data.Api
    ( SleHandle
    , withSleHandle
    , bind
    , unbind
    , startRAF
    , stopRAF
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.WriteCmd
import           SLE.Data.PDU
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID

-- import           Text.Builder





bind
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> ApplicationIdentifier
    -> PortID
    -> [ServiceInstanceAttribute]
    -> m ()
bind cfg hdl creds appID port attrs = do
  -- create an SLE Bind Invocation
    let bnd = mkSleBindInvocation creds 
                                  (cfg ^. cfgInitiator)
                                  port
                                  appID
                                  (cfg ^. cfgVersion)
                                  (ServiceInstanceIdentifier attrs)
    -- send it to the lower layers
    writeSLE hdl (SLEPdu (SlePduBind bnd))



unbind :: (MonadIO m) => CommonConfig -> SleHandle -> Credentials -> UnbindReason -> m ()
unbind _cfg hdl creds reason = do
    let pdu = mkSleUnbindBindInvocation creds reason
    writeSLE hdl (SLEPdu (SlePduUnbind pdu))


startRAF
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> Word16
    -> ConditionalTime
    -> ConditionalTime
    -> FrameQuality
    -> m ()
startRAF _cfg hdl creds invokeID start stop quality = do
    let pdu = RafStartInvocation { _rafStartCredentials       = creds
                                 , _rafStartInvokeID          = invokeID
                                 , _rafStartTime              = start
                                 , _rafStopTime               = stop
                                 , _rafStartRequestedTimeQual = quality
                                 }
    writeSLE hdl (SLEPdu (SlePduRafStart pdu))

stopRAF :: (MonadIO m) => CommonConfig -> SleHandle -> Credentials -> Word16 -> m () 
stopRAF _cfg hdl creds  invokeID = do 
    let pdu = SleStopInvocation { 
                    _sleStopCredentials = creds
                    , _sleStopInvokeID = invokeID
                    }
    writeSLE hdl (SLEPdu (SlePduStop pdu))

-- sendFrame :: (Monad m) => SleHandle -> ByteString -> m ()
-- sendFrame = undefined

-- sendOCF :: (Monad m) => SleHandle -> Word32 -> m ()
-- sendOCF = undefined


-- receiveFrame :: (Monad m) => SleHandle -> m ByteString
-- receiveFrame = undefined

-- receiveOCF :: (Monad m) => SleHandle -> m Word32
-- receiveOCF = undefined



