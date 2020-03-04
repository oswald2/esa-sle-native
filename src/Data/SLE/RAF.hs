module Data.SLE.RAF
  (Credentials)
where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy as BL
import           Language.Asn.Encoding
import Language.Asn.Types


newtype IntPosLong = IntPosLong Word32


type Credentials = Maybe ByteString

credentials :: AsnEncoding Credentials
credentials = choice [Nothing, Just B.empty] $ 
  \case 
    Nothing -> option 0 "unused" Nothing Language.Asn.Encoding.null'  
    Just bs -> option 1 "used" bs octetString


encode :: Credentials -> BL.ByteString
encode = der credentials

