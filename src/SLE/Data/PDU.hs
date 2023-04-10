module SLE.Data.PDU
    ( SlePdu(..)
    , setCredentials
    , isBind
    , isBindOrReturn
    , isTransfer
    , checkPermission
    , encodeSlePdu
    ) where

import           RIO                     hiding ( (^.) )
import qualified RIO.HashMap                   as HM

import           Control.Lens

import           SLE.Data.AUL
import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.RAFOps


data SlePdu =
  SlePduBind !SleBindInvocation
  | SlePduBindReturn !SleBindReturn
  | SlePduUnbind !SleUnbind
  | SlePduUnbindReturn !SleUnbindReturn
  | SlePduRafStart !RafStartInvocation
  | SlePduRafStartReturn !RafStartReturn
  | SlePduStop !SleStopInvocation
  | SlePduAck !SleAcknowledgement
  | SlePduRafTransferBuffer !RafTransferBuffer
  | SlePduRafStatusReport !RafStatusReport
  | SlePduScheduleStatusReport !SleScheduleStatusReport
  | SlePduScheduleStatusReturn !SleScheduleStatusReportReturn
  | SlePduPeerAbort !SlePeerAbort
  | SlePduFcltuStart !FcltuStartInvocation
  | SlePduFcltuStartReturn !FcltuStartReturn
  | SlePduFcltuThrowEvent !FcltuThrowEventInvocation
  | SlePduFcltuTransferData !FcltuTransDataInvocation
  | SlePduFcltuTransReturn !FcltuTransferDataReturn
  | SlePduFcltuAsync !FcltuAsyncNotify
  | SlePduGetParameter !GetParameterInvocation
  | SlePduRafParameterReturn !RafGetParameterReturn
  | SlePduFcltuStatusReport !CltuStatusReport
  | SlePduFcltuParameterReturn !FcltuGetParameterReturn
  deriving (Show, Generic)


isBind :: SlePdu -> Bool
isBind (SlePduBind _) = True
isBind _              = False

isBindOrReturn :: SlePdu -> Bool
isBindOrReturn (SlePduBind       _) = True
isBindOrReturn (SlePduBindReturn _) = True
isBindOrReturn _                    = False


isTransfer :: SlePdu -> Bool
isTransfer (SlePduRafTransferBuffer _) = True
isTransfer _                           = False

setCredentials :: SlePdu -> ISP1Credentials -> SlePdu
setCredentials (SlePduBind val) creds =
    SlePduBind $ val & sleBindCredentials ?~ creds
setCredentials (SlePduBindReturn val) creds =
    SlePduBindReturn $ val & sleBindRetCredentials ?~ creds
setCredentials (SlePduUnbind val) creds =
    SlePduUnbind $ val & sleUnbindCredentials ?~ creds
setCredentials (SlePduUnbindReturn val) creds =
    SlePduUnbindReturn $ val & sleUnbindRetCredentials ?~ creds
setCredentials (SlePduRafStart val) creds =
    SlePduRafStart $ val & rafStartCredentials ?~ creds
setCredentials (SlePduRafStartReturn val) creds =
    SlePduRafStartReturn $ val & rafStartRetCredentials ?~ creds
setCredentials (SlePduStop val) creds =
    SlePduStop $ val & sleStopCredentials ?~ creds
setCredentials (SlePduAck val) creds =
    SlePduAck $ val & sleAckCredentials ?~ creds
setCredentials (SlePduRafTransferBuffer val) creds = SlePduRafTransferBuffer
    (map setCreds val)
  where
    setCreds (TransFrame v) = TransFrame (v & rafTransCredentials ?~ creds)
    setCreds (TransNotification v) =
        TransNotification (v & rafSyncNCredentials ?~ creds)

setCredentials v@(SlePduPeerAbort _) _ = v
setCredentials (SlePduFcltuStart val) creds =
    SlePduFcltuStart $ val & fcltuStartCredentials ?~ creds
setCredentials (SlePduFcltuStartReturn val) creds =
    SlePduFcltuStartReturn $ val & fcltuStartRetCredentials ?~ creds
setCredentials (SlePduFcltuThrowEvent val) creds =
    SlePduFcltuThrowEvent $ val & fcltuThrowCredentials ?~ creds
setCredentials (SlePduFcltuTransferData val) creds =
    SlePduFcltuTransferData $ val & fcltuDataCredentials ?~ creds
setCredentials (SlePduFcltuTransReturn val) creds =
    SlePduFcltuTransReturn $ val & fcltuTransRetCredentials ?~ creds
setCredentials (SlePduFcltuAsync val) creds =
    SlePduFcltuAsync $ val & fcltuAsyncCredentials ?~ creds
setCredentials (SlePduRafStatusReport val) creds =
    SlePduRafStatusReport $ val & rstrCredentials ?~ creds
setCredentials (SlePduScheduleStatusReport val) creds =
    SlePduScheduleStatusReport $ val & sleSchedCredentials ?~ creds
setCredentials (SlePduScheduleStatusReturn val) creds =
    SlePduScheduleStatusReturn $ val & sleSchedRetCredentials ?~ creds
setCredentials (SlePduGetParameter val) creds =
    SlePduGetParameter $ val & gpCredentials ?~ creds
setCredentials (SlePduRafParameterReturn val) creds =
    SlePduRafParameterReturn $ val & rgpCredentials ?~ creds
setCredentials (SlePduFcltuStatusReport val) creds =
    SlePduFcltuStatusReport $ val & fcltuStatusCredentials ?~ creds
setCredentials (SlePduFcltuParameterReturn val) creds =
    SlePduFcltuParameterReturn $ val & fgpCredentials ?~ creds



getCredentials :: SlePdu -> Credentials
getCredentials (SlePduBind                 pdu ) = pdu ^. sleBindCredentials
getCredentials (SlePduBindReturn           pdu ) = pdu ^. sleBindRetCredentials
getCredentials (SlePduUnbind               pdu ) = pdu ^. sleUnbindCredentials
getCredentials (SlePduUnbindReturn pdu) = pdu ^. sleUnbindRetCredentials
getCredentials (SlePduRafStart             pdu ) = pdu ^. rafStartCredentials
getCredentials (SlePduRafStartReturn       pdu ) = pdu ^. rafStartRetCredentials
getCredentials (SlePduStop                 pdu ) = pdu ^. sleStopCredentials
getCredentials (SlePduAck                  pdu ) = pdu ^. sleAckCredentials
getCredentials (SlePduRafTransferBuffer    _pdu) = Nothing
getCredentials (SlePduPeerAbort            _pdu) = Nothing
getCredentials (SlePduFcltuStart           pdu ) = pdu ^. fcltuStartCredentials
getCredentials (SlePduFcltuStartReturn pdu) = pdu ^. fcltuStartRetCredentials
getCredentials (SlePduFcltuThrowEvent      pdu ) = pdu ^. fcltuThrowCredentials
getCredentials (SlePduFcltuTransferData    pdu ) = pdu ^. fcltuDataCredentials
getCredentials (SlePduFcltuTransReturn pdu) = pdu ^. fcltuTransRetCredentials
getCredentials (SlePduFcltuAsync           pdu ) = pdu ^. fcltuAsyncCredentials
getCredentials (SlePduGetParameter         pdu ) = pdu ^. gpCredentials
getCredentials (SlePduRafParameterReturn   pdu ) = pdu ^. rgpCredentials
getCredentials (SlePduRafStatusReport      pdu ) = pdu ^. rstrCredentials
getCredentials (SlePduScheduleStatusReport pdu ) = pdu ^. sleSchedCredentials
getCredentials (SlePduScheduleStatusReturn pdu ) = pdu ^. sleSchedRetCredentials
getCredentials (SlePduFcltuStatusReport    pdu ) = pdu ^. fcltuStatusCredentials
getCredentials (SlePduFcltuParameterReturn pdu ) = pdu ^. fgpCredentials


checkPermission
    :: SleAuthType
    -> Maybe Peer
    -> HashMap AuthorityIdentifier Peer
    -> SlePdu
    -> Bool
checkPermission _authType _initiator _authSet (SlePduPeerAbort _) = True
checkPermission authType initiator _authSet (SlePduRafTransferBuffer buf) =
    case authType of
        AuthNone -> True
        AuthBind -> True
        AuthAll  -> all (== True) $ map chk buf
  where
    chk (TransFrame        frame) = transChk initiator frame
    chk (TransNotification notif) = notifChk initiator notif

checkPermission authType initiator authSet pdu =
    checkPermission' authType initiator authSet pdu


checkPermission'
    :: SleAuthType
    -> Maybe Peer
    -> HashMap AuthorityIdentifier Peer
    -> SlePdu
    -> Bool
checkPermission' authType initiator authSet pdu = case authType of
    AuthNone -> True
    AuthBind -> case pdu of
        SlePduBind bind ->
            case HM.lookup (bind ^. sleBindInitiatorID) authSet of
                Nothing   -> False
                Just peer -> case bind ^. sleBindCredentials of
                    Nothing   -> False
                    Just isp1 -> checkCredentials
                        isp1
                        (bind ^. sleBindInitiatorID)
                        (passFromHex (cfgPeerPassword peer))
        _ -> True
    AuthAll -> case initiator of
        Nothing -> False
        Just authority ->
            let creds = getCredentials pdu
            in  case creds of
                    Nothing   -> False
                    Just isp1 -> checkCredentials
                        isp1
                        (cfgPeerAuthorityID authority)
                        (Password (cfgPeerPassword authority))

transChk :: Maybe Peer -> RafTransferDataInvocation -> Bool
transChk Nothing          _     = False
transChk (Just authority) frame = case frame ^. rafTransCredentials of
    Nothing   -> False
    Just isp1 -> checkCredentials isp1
                                  (cfgPeerAuthorityID authority)
                                  (Password (cfgPeerPassword authority))

notifChk :: Maybe Peer -> RafSyncNotifyInvocation -> Bool
notifChk Nothing          _     = False
notifChk (Just authority) notif = case notif ^. rafSyncNCredentials of
    Nothing   -> False
    Just isp1 -> checkCredentials isp1
                                  (cfgPeerAuthorityID authority)
                                  (Password (cfgPeerPassword authority))


encodeSlePdu :: SleVersion -> SlePdu -> ByteString
encodeSlePdu _version (SlePduBind                 val) = encode val
encodeSlePdu _version (SlePduBindReturn           val) = encode val
encodeSlePdu _version (SlePduUnbind               val) = encode val
encodeSlePdu _version (SlePduUnbindReturn         val) = encode val
encodeSlePdu _version (SlePduRafStart             val) = encode val
encodeSlePdu _version (SlePduRafStartReturn       val) = encode val
encodeSlePdu _version (SlePduStop                 val) = encode val
encodeSlePdu _version (SlePduAck                  val) = encode val
encodeSlePdu _version (SlePduRafTransferBuffer    val) = encode val
encodeSlePdu _version (SlePduPeerAbort            val) = encode val
encodeSlePdu _version (SlePduFcltuStart           val) = encode val
encodeSlePdu _version (SlePduFcltuStartReturn     val) = encode val
encodeSlePdu _version (SlePduFcltuThrowEvent      val) = encode val
encodeSlePdu _version (SlePduFcltuTransferData    val) = encode val
encodeSlePdu _version (SlePduFcltuTransReturn     val) = encode val
encodeSlePdu _version (SlePduFcltuAsync           val) = encode val
encodeSlePdu _version (SlePduGetParameter         val) = encode val
encodeSlePdu _version (SlePduRafParameterReturn   val) = encode val
encodeSlePdu _version (SlePduRafStatusReport      val) = encode val
encodeSlePdu _version (SlePduScheduleStatusReport val) = encode val
encodeSlePdu _version (SlePduScheduleStatusReturn val) = encode val
encodeSlePdu _version (SlePduFcltuStatusReport    val) = encode val
encodeSlePdu version (SlePduFcltuParameterReturn val) =
    encodeFcltuGetParameterReturn version val

