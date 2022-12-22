module SLE.State.Events
    ( SleEvent(..)
    , SleEventHandler
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.RAFOps
import           SLE.Data.Types.Common

data SleEvent =
  TMLConnect
  | TMLCouldNotConnect
  | TMLDisconnect
  | TMLProtocolAbort
  | TMLParseError Text
  | TMLError Text
  | TMLPeerAbort
  | SLEBindReceived SleBindInvocation
  | SLEBindSucceed SII
  | SLEBindFailed SII BindDiagnostic
  | SLEUnbindReceived SleUnbind
  | SLEUnbindSucceed SII
  | SLEUnbindFailed SII
  | SLERafStartReceived  RafStartInvocation
  | SLERafStartFailed SII
  | SLERafStartSucceed  SII
  deriving (Show, Generic)

type SleEventHandler = SleEvent -> IO ()
