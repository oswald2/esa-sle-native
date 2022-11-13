{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.Input
  ( SleInput(..)
  )
where

import           RIO
import           Control.Lens

import           SLE.Data.TMLMessage
import           SLE.Data.PDU


data SleInput =
  SLEAbort
  | SLEAbortPeer
  | SLEStopListen
  | SLEMsg TMLMessage
  | SLEPdu SlePdu
  deriving (Show, Generic)
makePrisms ''SleInput
