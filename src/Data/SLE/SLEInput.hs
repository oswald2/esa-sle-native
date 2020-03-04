module Data.SLE.SLEInput
  ( SLEInput(..)
  )
where

import           RIO

import           Data.SLE.TMLMessage


data SLEInput =
  SLEAbort
  | SLEAbortPeer
  | SLEMsg TMLMessage
