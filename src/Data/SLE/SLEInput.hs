module Data.SLE.SLEInput
  ( SLEInput(..)
  )
where

--import           RIO

import           Data.SLE.TMLMessage


data SLEInput =
  SLEAbort
  | SLEAbortPeer
  | SLEStopListen
  | SLEMsg TMLMessage
