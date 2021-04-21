module SLE.Types
    ( SleBindRole(..)
    , sleBindRoleC
    ) where

import           RIO

import           Foreign.C.Types

data SleBindRole =
  SleBindRoleInitiator
  | SleBindRoleResponder
  | SleBindRoleBoth


sleBindRoleC :: SleBindRole -> CInt
sleBindRoleC SleBindRoleInitiator = 0
sleBindRoleC SleBindRoleResponder = 1
sleBindRoleC SleBindRoleBoth      = 2
