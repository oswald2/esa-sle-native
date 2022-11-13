module SLE.State.Events
(
  SleEvent(..)
  , SleEventHandler
)
where

import RIO

import SLE.Data.Bind 


data SleEvent = 
  TMLConnect 
  | TMLCouldNotConnect
  | TMLDisconnect
  | TMLProtocolAbort
  | TMLParseError Text 
  | TMLError Text 
  | TMLPeerAbort
  | SLEBindReceived SleBindInvocation
  deriving (Show, Generic)

type SleEventHandler = SleEvent -> IO ()
