module SLE.State.Events
    ( SleEvent(..)
    , SleEventHandler
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.RAFOps
import           SLE.Data.FCLTUOps


data SleEvent =
  TMLConnect
  | TMLCouldNotConnect
  | TMLDisconnect
  | TMLProtocolAbort
  | TMLParseError !Text
  | TMLError !Text
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
  | SLEFcltuStatus !FCLTUIdx !ServiceState
  | SLEFcltuStartReceived !FcltuStartInvocation
  | SLEFcltuStartFailed !SII
  | SLEFcltuStartSucceed !SII
  | SLEFcltuStopReceived !SleStopInvocation
  | SLEFcltuStopFailed !SII
  | SLEFcltuStopSucceed !SII
  deriving (Show, Generic)

type SleEventHandler = SleEvent -> IO ()
