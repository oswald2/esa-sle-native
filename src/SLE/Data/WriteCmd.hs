{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.WriteCmd
  ( SleWrite(..)
  )
where

import           RIO
import           Control.Lens

import           SLE.Data.TMLMessage
import           SLE.Data.PDU


data SleWrite =
  SLEAbort
  | SLEAbortPeer
  | SLEStopListen
  | SLEMsg TMLMessage
  | SLEPdu SlePdu
  deriving (Show, Generic)
makePrisms ''SleWrite
