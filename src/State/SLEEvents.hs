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

type SleEventHandler = SleEvent -> IO ()
