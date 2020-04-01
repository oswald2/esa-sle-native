module Data.SLE.SlePdu
  ( SlePdu(..)
  , setCredentials
  )
where

import           RIO
import           Control.Lens
import           Data.SLE.RAF
import           Data.SLE.Common

data SlePdu =
  SlePduBind SleBindInvocation
  | SlePduBindReturn SleBindReturn


setCredentials :: SlePdu -> ByteString -> SlePdu
setCredentials (SlePduBind val) creds =
  SlePduBind $ val & sleBindCredentials ?~ creds
setCredentials (SlePduBindReturn val) creds =
  SlePduBindReturn $ val & sleBindRetCredentials ?~ creds

instance EncodeASN1 SlePdu where
  encode (SlePduBind val) = encode val
  encode (SlePduBindReturn val) = encode val 

