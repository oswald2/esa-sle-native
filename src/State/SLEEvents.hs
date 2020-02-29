module State.SLEEvents
(
  SleEvent(..)
  , SleEventHandler
)
where

import RIO

data SleEvent = 
  TMLConnect 
  | TMLCouldNotConnect
  | TMLDisconnect
  | TMLProtocolAbort
  | TMLParseError Text 
  | TMLPeerAbort

type SleEventHandler = SleEvent -> IO ()
