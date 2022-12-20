module SLE.Data.PDU
    ( SlePdu(..)
    , setCredentials
    , isBind
    ) where

import           Control.Lens
import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common

data SlePdu =
  SlePduBind SleBindInvocation
  | SlePduBindReturn SleBindReturn
  | SlePduUnbind SleUnbind
  | SlePduUnbindReturn SleUnbindReturn
  deriving (Show, Generic)


isBind :: SlePdu -> Bool
isBind (SlePduBind _) = True
isBind _              = False


setCredentials :: SlePdu -> ByteString -> SlePdu
setCredentials (SlePduBind val) creds =
    SlePduBind $ val & sleBindCredentials ?~ creds
setCredentials (SlePduBindReturn val) creds =
    SlePduBindReturn $ val & sleBindRetCredentials ?~ creds
setCredentials (SlePduUnbind val) creds =
    SlePduUnbind $ val & sleUnbindCredentials ?~ creds
setCredentials (SlePduUnbindReturn val) creds =
    SlePduUnbindReturn $ val & sleUnbindRetCredentials ?~ creds


instance EncodeASN1 SlePdu where
    encode (SlePduBind         val) = encode val
    encode (SlePduBindReturn   val) = encode val
    encode (SlePduUnbind       val) = encode val
    encode (SlePduUnbindReturn val) = encode val
