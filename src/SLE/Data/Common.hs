{-# LANGUAGE TemplateHaskell 
#-}
module SLE.Data.Common
    ( SII(..)
    , mkSII
    , ServiceState(..)
    , IntPosShort(..)
    , intPosShort
    , Credentials
    , credentials
    , getCredentials
    , Time(..)
    , time
    , ConditionalTime
    , conditionalTime
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
    , parseEitherASN1
    , parseSequence
    , parseSet
    , parseStartSet
    , parseEndSet
    , parseVisibleString
    , parseCredentials
    , parseIntVal
    , parseIntPosShort
    , parseTime
    , parseConditionalTime
    , parseOctetString
    , parseChoice
    , Diagnostics(..)
    , diagnostics
    , parseDiagnostics
    , diagnosticsFromInt
    , SleStopInvocation(..)
    , sleStopCredentials
    , sleStopInvokeID
    , parseStopInvocation
    , SleAcknowledgement(..)
    , sleAckCredentials
    , sleAckInvokeID
    , sleResult
    , parseSleAcknowledgement
    , AntennaID(..)
    , antennaID
    , parseAntennaID
    , PrivateAnnotation
    , privateAnnotation
    , parsePrivateAnnotation
    , RAFIdx(..)
    , InvokeID(..)
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL
import           RIO.State
import qualified RIO.Text                      as T

import           Control.Lens            hiding ( Context )
import           Control.Monad.Except

import           ByteString.StrictBuilder

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim
import           Data.ASN1.Types
import           Data.Aeson
import           Data.Attoparsec.ByteString     ( parseOnly )

import           SLE.Data.CCSDSTime

import           Text.Builder                  as TB

-- import           Text.Show.Pretty        hiding ( Time )

newtype SII = SII Text
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

mkSII :: Text -> SII
mkSII s = SII s

instance Display SII where
    textDisplay (SII sii) = sii


data ServiceState = ServiceInit | ServiceBound | ServiceActive
    deriving stock (Eq, Ord, Enum, Show, Read, Generic)



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
    parseChoice
        (\bs -> if B.null bs then return Nothing else return (Just bs))
        (const (return Nothing))
        "parseCredentials: no credentials found"

parseChoice
    :: (MonadError Text m, MonadState [ASN1] m)
    => (ByteString -> m b)
    -> (ByteString -> m b)
    -> Text
    -> m b
parseChoice choice0 choice1 errTxt = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            choice0 bs
        ((Other Context 1 bs) : rest) -> do
            put rest
            choice1 bs
        (o : _) ->
            throwError $ "parseChoice: expected choice, got " <> fromString
                (show o)
        _ -> throwError errTxt

data Time =
  Time CCSDSTime
  | TimePico CCSDSTimePico
  deriving (Show, Generic)

time :: Time -> ASN1
time (Time     t) = OctetString . builderBytes . ccsdsTimeBuilder $ t
time (TimePico t) = OctetString . builderBytes . ccsdsTimePicoBuilder $ t


parseTime :: Parser Time
parseTime = do
    x <- get
    case x of
        OctetString bs : rest -> do
            put rest
            case timeFromBS bs of
                Left  err -> throwError err
                Right t   -> return t
        Other Context 0 bs : rest -> do
            put rest
            case timeFromBS bs of
                Left  err -> throwError err
                Right t   -> return t
        _ -> throwError "parseTime: no time found"


timeFromBS :: ByteString -> Either Text Time
timeFromBS bs
    | B.length bs == 8
    = ccsdsTime
    | B.length bs == 10
    = ccsdsTimePico
    | otherwise
    = Left
        $  TB.run
        $  TB.text "parseTime: illegal time length: "
        <> TB.decimal (B.length bs)
        <> TB.text ", should be 8 or 10"
  where
    ccsdsTime = case parseOnly ccsdsTimeParser bs of
        Left err ->
            Left $ "parseTime: cannot parse CCSDS time: " <> fromString err
        Right t -> Right (Time t)

    ccsdsTimePico = case parseOnly ccsdsTimePicoParser bs of
        Left err ->
            Left $ "parseTime: cannot parse CCSDS pico time: " <> fromString err
        Right t -> Right (TimePico t)


type ConditionalTime = Maybe Time

conditionalTime :: ConditionalTime -> ASN1
conditionalTime Nothing = Other Context 0 B.empty
conditionalTime (Just t) =
    Other Context 1 (BL.toStrict (encodeASN1 DER ([time t])))


parseConditionalTime :: Parser ConditionalTime
parseConditionalTime = do
    parseChoice
        (const (return Nothing))
        tim
        "Could not parse conditional time, no conditional time found"
  where
    tim t = do
        case timeFromBS t of
            Left  err -> throwError err
            Right tm  -> return (Just tm)


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
                    throwError
                        $  "parseVisibleString: error decoding UTF8 value: "
                        <> T.pack (show err)
                        <> ": "
                        <> T.pack (show t)
                Right txt -> do
                    put rest
                    return txt
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
            -- traceM $ "Val: " <> T.pack (show val) <> "\nrest:\n" <> T.pack
            --     (ppShow rest)
            if p val
                then do
                    -- traceM "Match."
                    put rest
                    return (f val)
                else do
                    -- traceM "No Match."
                    throwError "parseASN1: Predicate did not match"
        _ -> throwError "parseASN1: list empty, could not parse value"


parseEitherASN1 :: Parser a -> Parser b -> Parser (Either a b)
parseEitherASN1 leftp rightp = do
    x <- get
    case x of
        (Other Context 0 dat : rest) -> do
            put rest
            case decodeASN1 DER (BL.fromStrict dat) of
                Left err ->
                    throwError
                        $  "parseEitherASN1: could not decode ASN1 choice: "
                        <> T.pack (show err)
                Right val -> do
                    case parseASN1 leftp val of
                        Left err ->
                            throwError
                                $ "parseEitherASN1: error on parsing LEFT value: "
                                <> err
                        Right l -> return (Left l)
        (Other Context 1 dat : rest) -> do
            put rest
            case decodeASN1 DER (BL.fromStrict dat) of
                Left err ->
                    throwError
                        $  "parseEitherASN1: could not decode ASN1 choice: "
                        <> T.pack (show err)
                Right val -> do
                    case parseASN1 rightp val of
                        Left err ->
                            throwError
                                $ "parseEitherASN1: error on parsing RIGHT value: "
                                <> err
                        Right l -> return (Right l)
        (Other Context n _ : _) ->
            throwError $ "parseEitherASN1: illegal value for choice: " <> T.pack
                (show n)
        (o : _) ->
            throwError $ "parseEitherASN1: expected CHOICE, got: " <> T.pack
                (show o)
        [] -> throwError "parseEitherASN1: expected CHOICE, got nothing"



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


data Diagnostics = DuplicateInvokeID | DiagOtherReason
    deriving(Show, Generic)


diagnostics :: Diagnostics -> ASN1
diagnostics DuplicateInvokeID = IntVal 100
diagnostics DiagOtherReason   = IntVal 127

parseDiagnostics :: Parser Diagnostics
parseDiagnostics = do
    x <- parseIntVal
    case x of
        100 -> return DuplicateInvokeID
        _   -> return DiagOtherReason

diagnosticsFromInt :: ASN1 -> Diagnostics
diagnosticsFromInt (IntVal 100) = DuplicateInvokeID
diagnosticsFromInt (IntVal _  ) = DiagOtherReason
diagnosticsFromInt _            = DiagOtherReason


type OptionalDiagnostics = Maybe Diagnostics


optionalDiagnostics :: OptionalDiagnostics -> ASN1
optionalDiagnostics Nothing = Other Context 0 B.empty
optionalDiagnostics (Just diag) =
    Other Context 1 (encodeASN1' DER [diagnostics diag])

parseOptionalDiagnostcs :: Parser OptionalDiagnostics
parseOptionalDiagnostcs = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return Nothing
        ((Other Context 1 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Optional Diagnostic RAF Start Parser: could not decode diagnostic: "
                        <> fromString (show err)
                Right v -> return (Just (diagnosticsFromInt v))
        _ -> throwError "Could not parse optional diagnostics"


data SleStopInvocation = SleStopInvocation
    { _sleStopCredentials :: !Credentials
    , _sleStopInvokeID    :: !Word16
    }
    deriving (Show, Generic)
makeLenses ''SleStopInvocation


stopInvocation :: SleStopInvocation -> [ASN1]
stopInvocation SleStopInvocation {..} =
    [ Start (Container Context 2)
    , credentials _sleStopCredentials
    , IntVal (fromIntegral _sleStopInvokeID)
    , End (Container Context 2)
    ]

instance EncodeASN1 SleStopInvocation where
    encode val = encodeASN1' DER (stopInvocation val)

parseStopInvocation :: Parser SleStopInvocation
parseStopInvocation = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 2)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        void endContainer
        return SleStopInvocation { _sleStopCredentials = creds
                                 , _sleStopInvokeID    = fromIntegral invokeID
                                 }



data SleAcknowledgement = SleAcknowledgement
    { _sleAckCredentials :: !Credentials
    , _sleAckInvokeID    :: !Word16
    , _sleResult         :: OptionalDiagnostics
    }
    deriving (Show, Generic)
makeLenses ''SleAcknowledgement


sleAcknowledgement :: SleAcknowledgement -> [ASN1]
sleAcknowledgement SleAcknowledgement {..} =
    [ Start (Container Context 3)
    , credentials _sleAckCredentials
    , IntVal (fromIntegral _sleAckInvokeID)
    , optionalDiagnostics _sleResult
    , End (Container Context 3)
    ]

instance EncodeASN1 SleAcknowledgement where
    encode val = encodeASN1' DER (sleAcknowledgement val)


parseSleAcknowledgement :: Parser SleAcknowledgement
parseSleAcknowledgement = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 3)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        result   <- parseOptionalDiagnostcs
        void endContainer
        return SleAcknowledgement { _sleAckCredentials = creds
                                  , _sleAckInvokeID    = fromIntegral invokeID
                                  , _sleResult         = result
                                  }


data AntennaID = GlobalForm OID | LocalForm ByteString
    deriving (Show, Generic)

antennaID :: AntennaID -> ASN1
antennaID (GlobalForm oid) = Other Context 0 (encodeASN1' DER [OID oid])
antennaID (LocalForm  bs ) = Other Context 1 bs

parseAntennaID :: Parser AntennaID
parseAntennaID = parseChoice global loc "Error parsing AntennaID"
  where
    global bs = case decodeASN1' DER bs of
        Left err ->
            throwError $ "Erro parsing global antenna ID: " <> fromString
                (show err)
        Right (OID o : rest) -> do
            put rest
            return $ GlobalForm o
        Right _ ->
            throwError $ "Error parsing global antenna ID, no OID provided"

    loc bs = return $ LocalForm bs


type PrivateAnnotation = Maybe ByteString

privateAnnotation :: PrivateAnnotation -> ASN1
privateAnnotation Nothing =
    Other Context 0 (encodeASN1' DER [Data.ASN1.Types.Null])
privateAnnotation (Just v) = Other Context 1 (encodeASN1' DER [OctetString v])

parsePrivateAnnotation :: Parser PrivateAnnotation
parsePrivateAnnotation = parseChoice (const (return Nothing))
                                     annot
                                     "Error parsing Private Annotation"
  where
    annot bs = do
        case decodeASN1' DER bs of
            Left err ->
                throwError $ "Error parsing Private Annotation: " <> fromString
                    (show err)
            Right (OctetString str : _) -> return (Just str)
            Right _ -> throwError "Error parsing Private Annotation"



newtype RAFIdx = RAFIdx Int
    deriving stock (Show, Generic)


newtype InvokeID =InvokeID Word16
    deriving stock (Show, Generic)
