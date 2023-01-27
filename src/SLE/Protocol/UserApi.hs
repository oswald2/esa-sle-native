module SLE.Protocol.UserApi
    ( SleHandle
    , withSleHandle
    , bind
    , unbind
    , startRAF
    , stopRAF
    , startFCLTU
    , stopFCLTU
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.Handle
import           SLE.Data.PDU
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID
import           SLE.Data.WriteCmd

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
                                  (cfg ^. cfgLocal)
                                  port
                                  appID
                                  (cfg ^. cfgVersion)
                                  (ServiceInstanceIdentifier attrs)
    -- send it to the lower layers
    writeSLE hdl (SLEPdu (SlePduBind bnd))



unbind
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> UnbindReason
    -> m ()
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
    -> ReqFrameQuality
    -> m ()
startRAF _cfg hdl creds invokeID start stop quality = do
    let pdu = RafStartInvocation { _rafStartCredentials       = creds
                                 , _rafStartInvokeID          = invokeID
                                 , _rafStartTime              = start
                                 , _rafStopTime               = stop
                                 , _rafStartRequestedTimeQual = quality
                                 }
    writeSLE hdl (SLEPdu (SlePduRafStart pdu))

stopRAF
    :: (MonadIO m) => CommonConfig -> SleHandle -> Credentials -> Word16 -> m ()
stopRAF _cfg hdl creds invokeID = do
    let pdu = SleStopInvocation { _sleStopCredentials = creds
                                , _sleStopInvokeID    = invokeID
                                }
    writeSLE hdl (SLEPdu (SlePduStop pdu))



startFCLTU
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> Word16
    -> m ()
startFCLTU _cfg hdl creds invokeID = do
    let pdu = FcltuStartInvocation
            { _fcltuStartCredentials            = creds
            , _fcltuStartInvokeID               = invokeID
            , _fcluStartFirstCltuIdentification = CltuIdentification 0
            }
    writeSLE hdl (SLEPdu (SlePduFcltuStart pdu))

stopFCLTU
    :: (MonadIO m) => CommonConfig -> SleHandle -> Credentials -> Word16 -> m ()
stopFCLTU _cfg hdl creds invokeID = do
    let pdu = SleStopInvocation { _sleStopCredentials = creds
                                , _sleStopInvokeID    = invokeID
                                }
    writeSLE hdl (SLEPdu (SlePduStop pdu))

