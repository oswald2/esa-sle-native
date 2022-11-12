module Data.SLE.PDU
  ( SlePdu(..)
  , setCredentials
  , isBind
  )
where

import           RIO
import           Control.Lens

import           Data.SLE.Common
import           Data.SLE.Bind

data SlePdu =
  SlePduBind SleBindInvocation
  | SlePduBindReturn SleBindReturn
  deriving (Show, Generic)


isBind :: SlePdu -> Bool
isBind (SlePduBind _) = True 
isBind _ = False 


setCredentials :: SlePdu -> ByteString -> SlePdu
setCredentials (SlePduBind val) creds =
  SlePduBind $ val & sleBindCredentials ?~ creds
setCredentials (SlePduBindReturn val) creds =
  SlePduBindReturn $ val & sleBindRetCredentials ?~ creds

instance EncodeASN1 SlePdu where
  encode (SlePduBind val) = encode val
  encode (SlePduBindReturn val) = encode val 

