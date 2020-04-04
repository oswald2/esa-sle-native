{-# LANGUAGE 
  TemplateHaskell
#-}
module Data.SLE.Input
  ( SleInput(..)
  )
where

import           RIO
import           Control.Lens

import           Data.SLE.TMLMessage
import           Data.SLE.PDU


data SleInput =
  SLEAbort
  | SLEAbortPeer
  | SLEStopListen
  | SLEMsg TMLMessage
  | SLEPdu SlePdu
  deriving (Show, Generic)
makePrisms ''SleInput
