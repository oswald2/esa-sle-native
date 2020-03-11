module Data.SLE.Common
  (IntPosShort(..)
  , intPosShort
  , Credentials
  , credentials
  , Time(..)
  , time 
  , visibleString
  , EncodeASN1(..)
  )
where

import           RIO
import           ByteString.StrictBuilder

import           Data.ASN1.Types

import           Data.SLE.CCSDSTime



class EncodeASN1 a where 
  encode :: a -> ByteString 


newtype IntPosShort = IntPosShort { getIntPosShort :: Word16 }

intPosShort :: IntPosShort -> ASN1
intPosShort (IntPosShort x) = IntVal (fromIntegral x)


type Credentials = Maybe ByteString

credentials :: Credentials -> ASN1
credentials Nothing   = Null
credentials (Just bs) = OctetString bs


data Time =
  Time CCSDSTime
  | TimePico CCSDSTimePico


time :: Time -> ASN1
time (Time     t) = OctetString . builderBytes . ccsdsTimeBuilder $ t
time (TimePico t) = OctetString . builderBytes . ccsdsTimePicoBuilder $ t


visibleString :: Text -> ASN1
visibleString t = ASN1String (ASN1CharacterString Visible (encodeUtf8 t))
