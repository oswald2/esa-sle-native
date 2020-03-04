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
  | TMLError Text 
  | TMLPeerAbort
  deriving (Show, Generic)

type SleEventHandler = SleEvent -> IO ()
