module SLE.Data.PDU
    ( SlePdu(..)
    , setCredentials
    , isBind
    ) where

import           Control.Lens
import           RIO

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.RAFOps

data SlePdu =
  SlePduBind SleBindInvocation
  | SlePduBindReturn SleBindReturn
  | SlePduUnbind SleUnbind
  | SlePduUnbindReturn SleUnbindReturn
  | SlePduRafStart RafStartInvocation
  | SlePduRafStartReturn RafStartReturn
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
setCredentials (SlePduRafStart val) creds =
    SlePduRafStart $ val & rafStartCredentials ?~ creds
setCredentials (SlePduRafStartReturn val) creds =
    SlePduRafStartReturn $ val & rafStartRetCredentials ?~ creds


instance EncodeASN1 SlePdu where
    encode (SlePduBind           val) = encode val
    encode (SlePduBindReturn     val) = encode val
    encode (SlePduUnbind         val) = encode val
    encode (SlePduUnbindReturn   val) = encode val
    encode (SlePduRafStart       val) = encode val
    encode (SlePduRafStartReturn val) = encode val
