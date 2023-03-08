module SLE.Protocol.UserApi
    ( SleHandle
    , withSleHandle
    , bind
    , unbind
    , scheduleReport
    , startRAF
    , stopRAF
    , startFCLTU
    , stopFCLTU
    , sendFCLTUData
    , rafGetParameter
    , fcltuGetParameter
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.Handle
import           SLE.Data.HexBytes
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


scheduleReport
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> Word16
    -> ReportRequestType
    -> m ()
scheduleReport _cfg hdl creds invokeID typ = do
    let pdu = SleScheduleStatusReport { _sleSchedCredentials = creds
                                      , _sleSchedInvokeID    = invokeID
                                      , _sleSchedRequestType = typ
                                      }
    writeSLE hdl (SLEPdu (SlePduScheduleStatusReport pdu))


rafGetParameter
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> Word16
    -> ParameterName
    -> m ()
rafGetParameter _cfg hdl creds invokeID paramName = do
    let pdu = GetParameterInvocation { _gpCredentials = creds
                                     , _gpInvokeID    = invokeID
                                     , _gpParameter   = paramName
                                     }
    writeSLE hdl (SLEPdu (SlePduGetParameter pdu))



startFCLTU
    :: (MonadIO m) => CommonConfig -> SleHandle -> Credentials -> Word16 -> m ()
startFCLTU _cfg hdl creds invokeID = do
    let pdu = FcltuStartInvocation
            { _fcltuStartCredentials            = creds
            , _fcltuStartInvokeID               = invokeID
            , _fcluStartFirstCltuIdentification = CltuIdentification 1
            }
    writeSLE hdl (SLEPdu (SlePduFcltuStart pdu))

stopFCLTU
    :: (MonadIO m) => CommonConfig -> SleHandle -> Credentials -> Word16 -> m ()
stopFCLTU _cfg hdl creds invokeID = do
    let pdu = SleStopInvocation { _sleStopCredentials = creds
                                , _sleStopInvokeID    = invokeID
                                }
    writeSLE hdl (SLEPdu (SlePduStop pdu))


sendFCLTUData
    :: MonadIO m
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> Word16
    -> CltuIdentification
    -> ConditionalTime
    -> ConditionalTime
    -> Duration
    -> SlduStatusNotification
    -> ByteString
    -> m ()
sendFCLTUData _cfg hdl creds invokeID cltuID earliest latest delay notif frame
    = do
        let pdu = FcltuTransDataInvocation
                { _fcltuDataCredentials           = creds
                , _fcltuDataInvokeID              = invokeID
                , _fcltuDataIdent                 = cltuID
                , _fcltuDataEarliestTransmission  = earliest
                , _fcltuDataLatestTransmission    = latest
                , _fcltuDataDelayTime             = delay
                , _fcltuDataRadiationNotification = notif
                , _fcltuData                      = bsToHex frame
                }
        writeSLE hdl (SLEPdu (SlePduFcltuTransferData pdu))


fcltuGetParameter
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> Credentials
    -> Word16
    -> ParameterName
    -> m ()
fcltuGetParameter _cfg hdl creds invokeID paramName = do
    let pdu = GetParameterInvocation { _gpCredentials = creds
                                     , _gpInvokeID    = invokeID
                                     , _gpParameter   = paramName
                                     }
    writeSLE hdl (SLEPdu (SlePduGetParameter pdu))
