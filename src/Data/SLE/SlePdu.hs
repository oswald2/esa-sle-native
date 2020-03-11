module Data.SLE.SlePdu
  ( SlePdu(..)
  , setCredentials
  )
where

import           RIO
import           Control.Lens
import           Data.SLE.RAF
import           Data.SLE.Common

newtype SlePdu =
  SlePduBind SleBindInvocation



setCredentials :: SlePdu -> ByteString -> SlePdu
setCredentials (SlePduBind val) creds =
  SlePduBind $ val & sleBindCredentials ?~ creds


instance EncodeASN1 SlePdu where
  encode (SlePduBind val) = encode val
