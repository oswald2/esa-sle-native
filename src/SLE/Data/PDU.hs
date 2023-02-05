module SLE.Data.PDU
    ( SlePdu(..)
    , setCredentials
    , isBind
    , isBindOrReturn
    , isTransfer
    ) where

import           Control.Lens
import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
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
  | SlePduPeerAbort !SlePeerAbort
  | SlePduFcltuStart !FcltuStartInvocation
  | SlePduFcltuStartReturn !FcltuStartReturn
  | SlePduFcltuThrowEvent !FcltuThrowEventInvocation
  | SlePduFcltuTransferData !FcltuTransDataInvocation
  | SlePduFcltuTransReturn !FcltuTransferDataReturn
  | SlePduFcltuAsync !FcltuAsyncNotify
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
setCredentials v@(SlePduRafTransferBuffer _) _ = v
setCredentials v@(SlePduPeerAbort         _) _ = v
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


instance EncodeASN1 SlePdu where
    encode (SlePduBind              val) = encode val
    encode (SlePduBindReturn        val) = encode val
    encode (SlePduUnbind            val) = encode val
    encode (SlePduUnbindReturn      val) = encode val
    encode (SlePduRafStart          val) = encode val
    encode (SlePduRafStartReturn    val) = encode val
    encode (SlePduStop              val) = encode val
    encode (SlePduAck               val) = encode val
    encode (SlePduRafTransferBuffer val) = encode val
    encode (SlePduPeerAbort         val) = encode val
    encode (SlePduFcltuStart        val) = encode val
    encode (SlePduFcltuStartReturn  val) = encode val
    encode (SlePduFcltuThrowEvent   val) = encode val
    encode (SlePduFcltuTransferData val) = encode val
    encode (SlePduFcltuTransReturn  val) = encode val
    encode (SlePduFcltuAsync        val) = encode val
