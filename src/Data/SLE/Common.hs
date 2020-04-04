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
  , Parser 
  , parseASN1
  , parseBasicASN1
  , parseStartSequence
  , parseEndSequence
  , between
  , manyA
  , parseSequence
  , parseSet
  , parseStartSet
  , parseEndSet
  , parseVisibleString
  , parseCredentials
  , parseIntVal
  , parseIntPosShort
  , parseTime
  , parseOctetString
  )
where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import           RIO.State
import           ByteString.StrictBuilder
import           Control.Monad.Except
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.ASN1.Types

import           Data.SLE.CCSDSTime



class EncodeASN1 a where
  encode :: a -> ByteString

class DecodeASN1 a where
  decode :: ByteString -> Maybe a


newtype IntPosShort = IntPosShort { getIntPosShort :: Word16 }

intPosShort :: IntPosShort -> ASN1
intPosShort (IntPosShort x) = IntVal (fromIntegral x)


parseIntPosShort :: Parser IntPosShort 
parseIntPosShort = do 
  IntPosShort . fromIntegral <$> parseIntVal 


parseIntVal :: Parser Integer 
parseIntVal = do 
  x <- get 
  case x of 
    (IntVal v : rest) -> do 
      put rest 
      return v 
    _ -> throwError "parseIntVal: no IntVal"


type Credentials = Maybe ByteString

credentials :: Credentials -> ASN1
credentials Nothing   = Other Context 0 ""
credentials (Just bs) = Other Context 1 bs

getCredentials :: ASN1 -> Maybe Credentials
getCredentials (Other Context 0 bs) =
  if B.null bs then Just Nothing else Just (Just bs)
getCredentials _ = Nothing

parseCredentials :: Parser Credentials 
parseCredentials = do 
  x <- get 
  case x of 
    ((Other Context 0 bs) : rest) -> do 
      put rest 
      if B.null bs then return Nothing else return (Just bs)
    ((Other Context 1 _) : rest) -> do 
      put rest 
      return Nothing 
    _ -> throwError "parseCredentials: no credentials found"

data Time =
  Time CCSDSTime
  | TimePico CCSDSTimePico


time :: Time -> ASN1
time (Time     t) = OctetString . builderBytes . ccsdsTimeBuilder $ t
time (TimePico t) = OctetString . builderBytes . ccsdsTimePicoBuilder $ t


parseTime :: Parser Time 
parseTime = do 
  x <- get 
  case x of 
    OctetString bs : rest -> do 
      put rest 
      if 
        | B.length bs == 8 -> ccsdsTime bs
        | B.length bs == 10 -> ccsdsTimePico bs
        | otherwise -> throwError $ "parseTime: illegal time length: " <> T.pack (show (B.length bs)) <> ", should be 8 or 10"
    _ -> throwError "parseTime: no time found"
  where 
    ccsdsTime bs = do 
      case parseOnly ccsdsTimeParser bs of 
        Left err -> throwError $ "parseTime: cannot parse CCSDS time: " <> T.pack err
        Right t -> return (Time t)

    ccsdsTimePico bs = do 
      case parseOnly ccsdsTimePicoParser bs of 
        Left err -> throwError $ "parseTime: cannot parse CCSDS pico time: " <> T.pack err
        Right t -> return (TimePico t)

visibleString :: Text -> ASN1
visibleString t = ASN1String (ASN1CharacterString Visible (encodeUtf8 t))

getVisibleString :: ASN1 -> Maybe Text
getVisibleString (ASN1String (ASN1CharacterString Visible t)) =
  case decodeUtf8' t of
    Left  _err -> Nothing
    Right dec  -> Just dec
getVisibleString _ = Nothing

parseVisibleString :: Parser Text
parseVisibleString = do
  x <- get
  case x of
    (ASN1String (ASN1CharacterString Visible t) : rest) -> do
      case decodeUtf8' t of
        Left err ->
          throwError $ "parseVisibleString: error decoding UTF8 value: "
            <> T.pack (show err) <> ": " <> T.pack (show t)
        Right text -> do
          put rest
          return text
    _ -> throwError "parseVisibleString: no visible string"


parseOctetString :: Parser ByteString 
parseOctetString = do 
  x <- get 
  case x of 
    OctetString bs : rest -> do 
      put rest 
      return bs 
    _ -> throwError "parseOctetString: no octet string found"


parseASN1 :: Parser a -> [ASN1] -> Either Text a
parseASN1 p = evalState (runExceptT p)


type Parser a = ExceptT Text (State [ASN1]) a


parseBasicASN1 :: (ASN1 -> Bool) -> (ASN1 -> a) -> Parser a
parseBasicASN1 p f = do
  x <- get
  case x of
    (val : rest) -> do
      --traceM $ "Val: " <> T.pack (show val)
      if p val
        then do
          --traceM "Match."
          put rest
          return (f val)
        else do 
          --traceM "No Match."
          throwError "parseASN1: Predicate did not match"
    _ -> throwError "parseASN1: list empty, could not parse value"


parseStartSequence :: Parser ()
parseStartSequence = parseBasicASN1 (== Start Sequence) (const ())

parseEndSequence :: Parser ()
parseEndSequence = parseBasicASN1 (== End Sequence) (const ())

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end bet = do
  void begin
  val <- bet
  void end
  return val

manyA :: Parser a -> Parser [a]
manyA p = loop []
 where
  loop acc = do
    action acc `catchError` const (return (reverse acc))

  action acc = do
    val <- p
    loop (val : acc)

parseSequence :: Parser e -> Parser e
parseSequence p = do
  between parseStartSequence parseEndSequence p


parseStartSet :: Parser ()
parseStartSet = parseBasicASN1 (== Start Set) (const ())

parseEndSet :: Parser ()
parseEndSet = parseBasicASN1 (== End Set) (const ())


parseSet :: Parser e -> Parser [e]
parseSet p = do 
  between parseStartSet parseEndSet (manyA p)