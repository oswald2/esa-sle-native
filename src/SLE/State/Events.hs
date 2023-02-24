module SLE.State.Events
    ( SleEvent(..)
    , SleEventHandler
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.FCLTUOps
import           SLE.Data.RAFOps


data SleEvent =
  TMLConnect
  | TMLCouldNotConnect
  | TMLDisconnect
  | TMLProtocolAbort
  | TMLParseError !Text
  | TMLError !Text
  | SLEWarning !Text
  | SLEError !Text
  | TMLPeerAbort
  | SLEBindReceived !SleBindInvocation
  | SLEBindSucceed !SII
  | SLEBindFailed !SII !BindDiagnostic
  | SLEUnbindReceived !SleUnbind
  | SLEUnbindSucceed !SII
  | SLEUnbindFailed !SII
  | SLERafStartReceived  !RafStartInvocation
  | SLERafStartFailed !SII
  | SLERafStartSucceed  !SII
  | SLERafStopReceived !SleStopInvocation
  | SLERafStopFailed !SII
  | SLERafStopSucceed !SII
  | SLERafStatus !RAFIdx !ServiceState
  | SLERafScheduleStatusReceived !SII !SleScheduleStatusReport
  | SLERafScheduleStatusSuccess !SII
  | SLERafScheduleStatusFailed !SII !Text 
  | SLEFcltuStatus !FCLTUIdx !ServiceState
  | SLEFcltuStartReceived !FcltuStartInvocation
  | SLEFcltuStartFailed !SII
  | SLEFcltuStartSucceed !SII
  | SLEFcltuStopReceived !SleStopInvocation
  | SLEFcltuStopFailed !SII
  | SLEFcltuStopSucceed !SII
  | SLEFcltuTransferData !SII !FCLTUIdx !TMIdx !FcltuTransDataInvocation
  | SLEFcltuTransDataFailed !SII
  deriving (Show, Generic)

type SleEventHandler = SleEvent -> IO ()
