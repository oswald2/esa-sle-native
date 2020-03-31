module Data.SLE.Common
  ( IntPosShort(..)
  , intPosShort
  , Credentials
  , credentials
  , getCredentials
  , Time(..)
  , time
  , visibleString
  , getVisibleString
  , EncodeASN1(..)
  , DecodeASN1(..)
  )
where

import           RIO
import qualified RIO.ByteString                as B
import           ByteString.StrictBuilder

import           Data.ASN1.Types

import           Data.SLE.CCSDSTime



class EncodeASN1 a where
  encode :: a -> ByteString

class DecodeASN1 a where
  decode :: ByteString -> Maybe a


newtype IntPosShort = IntPosShort { getIntPosShort :: Word16 }

intPosShort :: IntPosShort -> ASN1
intPosShort (IntPosShort x) = IntVal (fromIntegral x)


type Credentials = Maybe ByteString

credentials :: Credentials -> ASN1
credentials Nothing   = Other Context 0 ""
credentials (Just bs) = Other Context 1 bs

getCredentials :: ASN1 -> Maybe Credentials
getCredentials (Other Context 0 bs) = if B.null bs then Just Nothing else Just (Just bs)
getCredentials _                    = Nothing


data Time =
  Time CCSDSTime
  | TimePico CCSDSTimePico


time :: Time -> ASN1
time (Time     t) = OctetString . builderBytes . ccsdsTimeBuilder $ t
time (TimePico t) = OctetString . builderBytes . ccsdsTimePicoBuilder $ t


visibleString :: Text -> ASN1
visibleString t = ASN1String (ASN1CharacterString Visible (encodeUtf8 t))

getVisibleString :: ASN1 -> Maybe Text 
getVisibleString (ASN1String (ASN1CharacterString Visible t)) = 
  case decodeUtf8' t of 
    Left _err -> Nothing 
    Right dec -> Just dec 
getVisibleString _ = Nothing