module State.SLEEvents
(
  SleEvent(..)
  , SleEventHandler
)
where

import RIO

import Data.SLE.Bind 


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
