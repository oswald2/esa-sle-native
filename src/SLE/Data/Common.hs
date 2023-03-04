{-# LANGUAGE TemplateHaskell 
#-}
module SLE.Data.Common
    ( SII(..)
    , mkSII
    , ServiceState(..)
    , IntPosShort(..)
    , intPosShort
    , encWord16
    , decWord16
    , Credentials
    , credentials
    , ISP1Credentials(..)
    , isp1Credentials
    , mkCredentials
    , isp1CredentialsParser
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
    , parseStartContainer
    , parseEndContainer
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
    , parseChoiceASN1
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
    , RCFIdx(..)
    , TMIdx(..)
    , FCLTUIdx(..)
    , InvokeID(..)
    , PeerAbortDiagnostic(..)
    , SlePeerAbort(..)
    , parseSlePeerAbort
    , ForwardDuStatus(..)
    , forwardDuStatus
    , parseForwardDuStatus
    , ReportRequestType(..)
    , SleScheduleStatusReport(..)
    , parseScheduleStatusReport
    , sleSchedCredentials
    , sleSchedInvokeID
    , sleSchedRequestType
    , DiagScheduleSpecificVal(..)
    , DiagScheduleStatus(..)
    , ParameterName(..)
    , parameterName
    , parseParameterName
    , GetParameterInvocation(..)
    , gpCredentials
    , gpInvokeID
    , gpParameter
    , parseGetParameterInvocation
    , DiagScheduleResult(..)
    , SleScheduleStatusReportReturn(..)
    , parseSleScheduleStatusReportReturn
    , sleSchedRetCredentials
    , sleSchedRetInvokeID
    , sleSchedRetResult
    , DeliveryMode(..)
    , parseDeliveryMode
    , deliveryMode
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL
import           RIO.State
import qualified RIO.Text                      as T

import           Control.Lens            hiding ( Context )
import           Control.Monad.Except

import           ByteString.StrictBuilder

import           Data.ASN1.BinaryEncoding       ( DER(DER) )
import           Data.ASN1.Encoding             ( ASN1Decoding(decodeASN1)
                                                , decodeASN1'
                                                , encodeASN1'
                                                )
import           Data.ASN1.Prim
import           Data.ASN1.Types
import           Data.Aeson
import           Data.Attoparsec.ByteString     ( parseOnly )
import           Data.Bits

import           SLE.Data.CCSDSTime             ( CCSDSTime
                                                , CCSDSTimePico
                                                , ccsdsTimeBuilder
                                                , ccsdsTimeParser
                                                , ccsdsTimePicoBuilder
                                                , ccsdsTimePicoParser
                                                , toPicoTime
                                                )
import           SLE.Data.HexBytes

import           Text.Builder                  as TB
                                                ( decimal
                                                , run
                                                , text
                                                )

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


data ISP1Credentials = ISP1Credentials
    { _isp1Time         :: CCSDSTime
    , _isp1RandomNumber :: Int32
    , _isp1TheProtected :: HexBytes
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass NFData

mkCredentials :: CCSDSTime -> Int32 -> ByteString -> ISP1Credentials
mkCredentials tim rand prot = ISP1Credentials
    { _isp1Time         = tim
    , _isp1RandomNumber = rand
    , _isp1TheProtected = bsToHex prot
    }

isp1Credentials :: ISP1Credentials -> [ASN1]
isp1Credentials ISP1Credentials {..} =
    [ Start Sequence
    , OctetString (builderBytes (ccsdsTimeBuilder _isp1Time))
    , IntVal (fromIntegral _isp1RandomNumber)
    , OctetString (hexToBS _isp1TheProtected)
    , End Sequence
    ]

instance EncodeASN1 ISP1Credentials where
    encode val = encodeASN1' DER (isp1Credentials val)



isp1CredentialsParser :: Parser ISP1Credentials
isp1CredentialsParser = do
    parseSequence isp1Parser
  where
    isp1Parser = do
        t    <- parseCredentialTime
        r    <- parseIntVal
        prot <- parseOctetString
        case t of
            Time cds -> return ISP1Credentials
                { _isp1Time         = cds
                , _isp1RandomNumber = fromIntegral r
                , _isp1TheProtected = bsToHex prot
                }
            TimePico _ ->
                throwError
                    "isp1CredentialsParser: expected CCSDS Time, got CCSDS Pico Time"




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
        a -> do
            put a
            throwError "parseIntVal: no IntVal"


type Credentials = Maybe ISP1Credentials

credentials :: Credentials -> ASN1
credentials Nothing = Other Context 0 ""
credentials (Just creds) =
    Other Context 1 (encodeASN1' DER (isp1Credentials creds))

-- getCredentials :: ASN1 -> Maybe Credentials
-- getCredentials (Other Context 0 bs) =
--     if B.null bs then Just Nothing else Just (Just bs)
-- getCredentials _ = Nothing

parseCredentials :: Parser Credentials
parseCredentials = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return Nothing
        ((Other Context 1 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError $ "Could not parse Credentials: " <> fromString
                        (show err)
                Right v -> do
                    case parseASN1 isp1CredentialsParser v of
                        Left err ->
                            throwError $ "Could not parse Credentials: " <> err
                        Right isp -> return (Just isp)
        asn1 -> do
            put asn1
            throwError
                $  "Could not parse Credentials: unexpected ASN1: "
                <> fromString (show asn1)


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
        a@(o : _) -> do
            put a
            throwError $ "parseChoice: expected choice, got " <> fromString
                (show o)
        [] -> throwError errTxt


parseChoiceASN1 :: Parser b -> Parser b -> Text -> Parser b
parseChoiceASN1 choice0 choice1 errTxt = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError $ "Error decoding choice 0: " <> fromString
                        (show err)
                Right asn1 -> case parseASN1 choice0 asn1 of
                    Left err ->
                        throwError $ "Error decoding choice 0: " <> fromString
                            (show err)
                    Right v -> return v
        ((Other Context 1 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError $ "Error decoding choice 1: " <> fromString
                        (show err)
                Right asn1 -> case parseASN1 choice1 asn1 of
                    Left err ->
                        throwError $ "Error decoding choice 0: " <> fromString
                            (show err)
                    Right v -> return v
        a@(o : _) -> do
            put a
            throwError $ "parseChoice: expected choice, got " <> fromString
                (show o)
        [] -> throwError errTxt

data Time =
  Time CCSDSTime
  | TimePico CCSDSTimePico
  deriving stock (Show, Generic)
  deriving anyclass (NFData)


instance Eq Time where
    Time     t1 == Time     t2 = t1 == t2
    TimePico t1 == TimePico t2 = t1 == t2
    Time     t1 == TimePico t2 = toPicoTime t1 == t2
    TimePico t1 == Time     t2 = t1 == toPicoTime t2

instance Ord Time where
    compare (Time     t1) (Time     t2) = compare t1 t2
    compare (TimePico t1) (TimePico t2) = compare t1 t2
    compare (Time     t1) (TimePico t2) = compare (toPicoTime t1) t2
    compare (TimePico t1) (Time     t2) = compare t1 (toPicoTime t2)

instance Display Time where
    display (Time     t) = display t
    display (TimePico t) = display t


time :: Time -> ASN1
time (Time     t) = Other Context 0 $ builderBytes . ccsdsTimeBuilder $ t
time (TimePico t) = Other Context 1 $ builderBytes . ccsdsTimePicoBuilder $ t


parseTime :: Parser Time
parseTime = do
    x <- get
    case x of
        Other Context _ bs : rest -> do
            put rest
            case timeFromBS bs of
                Left  err -> throwError err
                Right t   -> return t
        a -> do
            put a
            throwError "parseTime: no time found"


parseCredentialTime :: Parser Time
parseCredentialTime = do
    x <- get
    case x of
        (OctetString bs : rest) -> do
            put rest
            case timeFromBS bs of
                Left  err -> throwError err
                Right t   -> return t
        asn1 -> do
            put asn1
            throwError
                $  "Parsing ISP1 Credentials Time: unexpected ASN1: "
                <> fromString (show asn1)

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

conditionalTime :: ConditionalTime -> [ASN1]
conditionalTime Nothing = [Other Context 0 B.empty]
-- conditionalTime (Just t) =
--     Other Context 1 (BL.toStrict (encodeASN1 DER [time t]))
conditionalTime (Just t) =
    [Start (Container Context 1), time t, End (Container Context 1)]


parseConditionalTime :: Parser ConditionalTime
parseConditionalTime = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return Nothing
        ((Other Context 1 bs) : rest) -> do
            put rest
            tim bs
        ((Start (Container Context 1)) : Other Context _ bs : End (Container Context 1) : rest)
            -> do
                put rest
                tim bs
        a@(asn1 : _) -> do
            put a
            throwError
                $  "Conditional Time Parser: unexpected ASN1 value: "
                <> fromString (show asn1)
        [] -> throwError "Conditional Time: no value found."
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
            put rest
            case decodeUtf8' t of
                Left err ->
                    throwError
                        $  "parseVisibleString: error decoding UTF8 value: "
                        <> T.pack (show err)
                        <> ": "
                        <> T.pack (show t)
                Right txt -> do
                    return txt
        a -> do
            put a
            throwError "parseVisibleString: no visible string"


parseOctetString :: Parser ByteString
parseOctetString = do
    x <- get
    case x of
        OctetString bs : rest -> do
            put rest
            return bs
        a -> do
            put a
            throwError "parseOctetString: no octet string found"


parseASN1 :: Parser a -> [ASN1] -> Either Text a
parseASN1 p = evalState (runExceptT p)


type Parser a = ExceptT Text (State [ASN1]) a


parseBasicASN1 :: (ASN1 -> Bool) -> (ASN1 -> a) -> Parser a
parseBasicASN1 p f = do
    x <- get
    case x of
        a@(val : rest) -> do
            -- traceM $ "Val: " <> T.pack (show val) <> "\nrest:\n" <> T.pack
            --     (ppShow rest)
            if p val
                then do
                    put rest 
                    -- traceM "Match."
                    return (f val)
                else do
                    put a
                    -- traceM "No Match."
                    throwError "parseASN1: Predicate did not match"
        [] -> throwError "parseASN1: list empty, could not parse value"


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
        a@(Other Context n _ : _) -> do
            put a
            throwError $ "parseEitherASN1: illegal value for choice: " <> T.pack
                (show n)
        a@(o : _) -> do
            put a
            throwError $ "parseEitherASN1: expected CHOICE, got: " <> T.pack
                (show o)
        [] -> throwError "parseEitherASN1: expected CHOICE, got nothing"



parseStartSequence :: Parser ()
parseStartSequence = parseBasicASN1 (== Start Sequence) (const ())

parseEndSequence :: Parser ()
parseEndSequence = parseBasicASN1 (== End Sequence) (const ())

parseStartContainer :: Int -> Parser ()
parseStartContainer x =
    parseBasicASN1 (== Start (Container Context x)) (const ())

parseEndContainer :: Int -> Parser ()
parseEndContainer x = parseBasicASN1 (== End (Container Context x)) (const ())



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
        a -> do
            put a
            throwError "Could not parse optional diagnostics"


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


data AntennaID = GlobalForm OID | LocalForm Text
    deriving stock (Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

antennaID :: AntennaID -> ASN1
antennaID (GlobalForm oid) = Other Context 0 (encodeASN1' DER [OID oid])
antennaID (LocalForm  bs ) = Other Context 1 (encodeUtf8 bs)

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
        Right asn1 -> do
            put asn1
            throwError $ "Error parsing global antenna ID, no OID provided"

    loc bs = case decodeUtf8' bs of
        Left  _err -> return $ LocalForm "UNKNOWN"
        Right txt  -> return $ LocalForm txt


type PrivateAnnotation = Maybe ByteString

privateAnnotation :: PrivateAnnotation -> ASN1
privateAnnotation Nothing  = Other Context 0 B.empty
privateAnnotation (Just v) = Other Context 1 v

parsePrivateAnnotation :: Parser PrivateAnnotation
parsePrivateAnnotation = parseChoice (const (return Nothing))
                                     annot
                                     "Error parsing Private Annotation"
    where annot bs = return (Just bs)



newtype RAFIdx = RAFIdx Int
    deriving stock (Show, Generic)

newtype RCFIdx = RCFIdx Int
    deriving stock (Show, Generic)

data TMIdx = TMRAF !RAFIdx | TMRCF !RCFIdx | TMFirst !Word8 | TCFCLTU !FCLTUIdx
    deriving stock (Show, Generic)

newtype InvokeID =InvokeID Word16
    deriving stock (Show, Generic)

newtype FCLTUIdx = FCLTUIdx Int
    deriving stock (Show, Generic)


data PeerAbortDiagnostic =
    PAAccessDenied
    | PAUnexpectedResponderID
    | PAOperationalRequirement
    | PAProtocolError
    | PACommunicationsFailure
    | PAEncodingError
    | PAReturnTimeout
    | PAEndOfServiceProvisionPeriod
    | PAUnsolicitedInvokeID
    | PAOtherReason
    | PAOther !Word8
    deriving stock (Read, Show, Generic)


instance Display PeerAbortDiagnostic where
    display PAAccessDenied                = "Access Denied"
    display PAUnexpectedResponderID       = "Unexpected Responder ID"
    display PAOperationalRequirement      = "Operational Requirement"
    display PAProtocolError               = "Protocol Error"
    display PACommunicationsFailure       = "Communications Failure"
    display PAEncodingError               = "Encoding Error"
    display PAReturnTimeout               = "Return Timeout"
    display PAEndOfServiceProvisionPeriod = "End Of Service Provision Period"
    display PAUnsolicitedInvokeID         = "Unsolicited InvokeID"
    display PAOtherReason                 = "Other Reason"
    display (PAOther x)                   = "Specific Reason: " <> display x

parsePeerAbortDiagnostic :: Parser PeerAbortDiagnostic
parsePeerAbortDiagnostic = do
    x <- parseIntVal
    case x of
        0   -> return PAAccessDenied
        1   -> return PAUnexpectedResponderID
        2   -> return PAOperationalRequirement
        3   -> return PAProtocolError
        4   -> return PACommunicationsFailure
        5   -> return PAEncodingError
        6   -> return PAReturnTimeout
        7   -> return PAEndOfServiceProvisionPeriod
        8   -> return PAUnsolicitedInvokeID
        127 -> return PAOtherReason
        v   -> return $ PAOther (fromIntegral v)

peerAbortDiagnostic :: PeerAbortDiagnostic -> ASN1
peerAbortDiagnostic PAAccessDenied                = IntVal 0
peerAbortDiagnostic PAUnexpectedResponderID       = IntVal 1
peerAbortDiagnostic PAOperationalRequirement      = IntVal 2
peerAbortDiagnostic PAProtocolError               = IntVal 3
peerAbortDiagnostic PACommunicationsFailure       = IntVal 4
peerAbortDiagnostic PAEncodingError               = IntVal 5
peerAbortDiagnostic PAReturnTimeout               = IntVal 6
peerAbortDiagnostic PAEndOfServiceProvisionPeriod = IntVal 7
peerAbortDiagnostic PAUnsolicitedInvokeID         = IntVal 8
peerAbortDiagnostic PAOtherReason                 = IntVal 127
peerAbortDiagnostic (PAOther x)                   = IntVal (fromIntegral x)


newtype SlePeerAbort = SlePeerAbort PeerAbortDiagnostic
    deriving stock (Show, Read, Generic)


slePeerAbort :: SlePeerAbort -> [ASN1]
slePeerAbort (SlePeerAbort diag) =
    [ Start (Container Context 104)
    , peerAbortDiagnostic diag
    , End (Container Context 104)
    ]

instance EncodeASN1 SlePeerAbort where
    encode val = encodeASN1' DER (slePeerAbort val)

parseSlePeerAbort :: Parser SlePeerAbort
parseSlePeerAbort = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 104)) (const ())

    content      = do
        reason <- parsePeerAbortDiagnostic
        void endContainer
        return $ SlePeerAbort reason





data ForwardDuStatus =
    FwDURadiated
    | FwDUExpired
    | FwDUInterrupted
    | FwDUAcknowledged
    | FwDUProductionStarted
    | FwDUProductionNotStarted
    | FwDUUnsupportedTransmissionMode
    deriving (Show, Generic)

forwardDuStatus :: ForwardDuStatus -> ASN1
forwardDuStatus FwDURadiated                    = IntVal 0
forwardDuStatus FwDUExpired                     = IntVal 1
forwardDuStatus FwDUInterrupted                 = IntVal 2
forwardDuStatus FwDUAcknowledged                = IntVal 3
forwardDuStatus FwDUProductionStarted           = IntVal 4
forwardDuStatus FwDUProductionNotStarted        = IntVal 5
forwardDuStatus FwDUUnsupportedTransmissionMode = IntVal 6


parseForwardDuStatus :: Parser ForwardDuStatus
parseForwardDuStatus = do
    x <- parseIntVal
    case x of
        0 -> return FwDURadiated
        1 -> return FwDUExpired
        2 -> return FwDUInterrupted
        3 -> return FwDUAcknowledged
        4 -> return FwDUProductionStarted
        5 -> return FwDUProductionNotStarted
        6 -> return FwDUUnsupportedTransmissionMode
        _ -> return FwDUUnsupportedTransmissionMode



data ParameterName =
    ParAcquisitionSequenceLength
    | ParApidList
    | ParBitLockRequired
    | ParBlockingTimeoutPeriod
    | ParBlockingUsage
    | ParBufferSize
    | ParClcwGlobalVCID
    | ParClcwPhysicalChannel
    | ParCopCntrFramesRepetition
    | ParDeliveryMode
    | ParDirectiveInvocation
    | ParDirectiveInvocationOnline
    | ParExpectedDirectiveIdentification
    | ParExpectedEventInvocationIdentification
    | ParExpectedSludIdentification
    | ParFopSlidingWindow
    | ParFopState
    | ParLatencyLimit
    | ParMapList
    | ParMuxControl
    | ParMuxScheme
    | ParMaximumFrameLength
    | ParMaximumPacketLength
    | ParMaximumSlduLength
    | ParMinimumDelayTime
    | ParMinReportingCycle
    | ParModulationFrequency
    | ParModulationIndex
    | ParNotificationMode
    | ParPermittedControlWordTypeSet
    | ParPermittedFrameQuality
    | ParPermittedGvcidSet
    | ParPermittedTcVcidSet
    | ParPermittedTransmissionMode
    | ParPermittedUpdateModeSet
    | ParPlop1IdleSequenceLength
    | ParPlopInEffect
    | ParProtocolAbortMode
    | ParReportingCycle
    | ParRequestedControlWordType
    | ParRequestedFrameQuality
    | ParRequestedGvcid
    | ParRequestedTcVcid
    | ParRequestedUpdateMode
    | ParReturnTimeoutPeriod
    | ParRfAvailable
    | ParRfAvailableRequired
    | ParSegmentHeader
    | ParSequCntrFramesRepetition
    | ParSubcarrierToBitRateRatio
    | ParThrowEventOperation
    | ParTimeoutType
    | ParTimerInitial
    | ParTransmissionLimit
    | ParTransmitterFrameSequenceNumber
    | ParVcMuxControl
    | ParVcMuxScheme
    | ParVirtualChannel
    deriving(Read, Show, Eq, Ord, Enum, Generic)


parameterName :: ParameterName -> ASN1
parameterName ParAcquisitionSequenceLength             = IntVal 201
parameterName ParApidList                              = IntVal 2
parameterName ParBitLockRequired                       = IntVal 3
parameterName ParBlockingTimeoutPeriod                 = IntVal 0
parameterName ParBlockingUsage                         = IntVal 1
parameterName ParBufferSize                            = IntVal 4
parameterName ParClcwGlobalVCID                        = IntVal 202
parameterName ParClcwPhysicalChannel                   = IntVal 203
parameterName ParCopCntrFramesRepetition               = IntVal 300
parameterName ParDeliveryMode                          = IntVal 6
parameterName ParDirectiveInvocation                   = IntVal 7
parameterName ParDirectiveInvocationOnline             = IntVal 108
parameterName ParExpectedDirectiveIdentification       = IntVal 8
parameterName ParExpectedEventInvocationIdentification = IntVal 9
parameterName ParExpectedSludIdentification            = IntVal 10
parameterName ParFopSlidingWindow                      = IntVal 11
parameterName ParFopState                              = IntVal 12
parameterName ParLatencyLimit                          = IntVal 15
parameterName ParMapList                               = IntVal 16
parameterName ParMuxControl                            = IntVal 17
parameterName ParMuxScheme                             = IntVal 18
parameterName ParMaximumFrameLength                    = IntVal 19
parameterName ParMaximumPacketLength                   = IntVal 20
parameterName ParMaximumSlduLength                     = IntVal 21
parameterName ParMinimumDelayTime                      = IntVal 204
parameterName ParMinReportingCycle                     = IntVal 301
parameterName ParModulationFrequency                   = IntVal 22
parameterName ParModulationIndex                       = IntVal 23
parameterName ParNotificationMode                      = IntVal 205
parameterName ParPermittedControlWordTypeSet           = IntVal 101
parameterName ParPermittedFrameQuality                 = IntVal 302
parameterName ParPermittedGvcidSet                     = IntVal 24
parameterName ParPermittedTcVcidSet                    = IntVal 102
parameterName ParPermittedTransmissionMode             = IntVal 107
parameterName ParPermittedUpdateModeSet                = IntVal 103
parameterName ParPlop1IdleSequenceLength               = IntVal 206
parameterName ParPlopInEffect                          = IntVal 25
parameterName ParProtocolAbortMode                     = IntVal 207
parameterName ParReportingCycle                        = IntVal 26
parameterName ParRequestedControlWordType              = IntVal 104
parameterName ParRequestedFrameQuality                 = IntVal 27
parameterName ParRequestedGvcid                        = IntVal 28
parameterName ParRequestedTcVcid                       = IntVal 105
parameterName ParRequestedUpdateMode                   = IntVal 106
parameterName ParReturnTimeoutPeriod                   = IntVal 29
parameterName ParRfAvailable                           = IntVal 30
parameterName ParRfAvailableRequired                   = IntVal 31
parameterName ParSegmentHeader                         = IntVal 32
parameterName ParSequCntrFramesRepetition              = IntVal 303
parameterName ParSubcarrierToBitRateRatio              = IntVal 34
parameterName ParThrowEventOperation                   = IntVal 304
parameterName ParTimeoutType                           = IntVal 35
parameterName ParTimerInitial                          = IntVal 36
parameterName ParTransmissionLimit                     = IntVal 37
parameterName ParTransmitterFrameSequenceNumber        = IntVal 38
parameterName ParVcMuxControl                          = IntVal 39
parameterName ParVcMuxScheme                           = IntVal 40
parameterName ParVirtualChannel                        = IntVal 41


parseParameterName :: Parser ParameterName
parseParameterName = do
    x <- parseIntVal
    case x of
        201 -> return ParAcquisitionSequenceLength
        2   -> return ParApidList
        3   -> return ParBitLockRequired
        0   -> return ParBlockingTimeoutPeriod
        1   -> return ParBlockingUsage
        4   -> return ParBufferSize
        202 -> return ParClcwGlobalVCID
        203 -> return ParClcwPhysicalChannel
        300 -> return ParCopCntrFramesRepetition
        6   -> return ParDeliveryMode
        7   -> return ParDirectiveInvocation
        108 -> return ParDirectiveInvocationOnline
        8   -> return ParExpectedDirectiveIdentification
        9   -> return ParExpectedEventInvocationIdentification
        10  -> return ParExpectedSludIdentification
        11  -> return ParFopSlidingWindow
        12  -> return ParFopState
        15  -> return ParLatencyLimit
        16  -> return ParMapList
        17  -> return ParMuxControl
        18  -> return ParMuxScheme
        19  -> return ParMaximumFrameLength
        20  -> return ParMaximumPacketLength
        21  -> return ParMaximumSlduLength
        204 -> return ParMinimumDelayTime
        301 -> return ParMinReportingCycle
        22  -> return ParModulationFrequency
        23  -> return ParModulationIndex
        205 -> return ParNotificationMode
        101 -> return ParPermittedControlWordTypeSet
        302 -> return ParPermittedFrameQuality
        24  -> return ParPermittedGvcidSet
        102 -> return ParPermittedTcVcidSet
        107 -> return ParPermittedTransmissionMode
        103 -> return ParPermittedUpdateModeSet
        206 -> return ParPlop1IdleSequenceLength
        25  -> return ParPlopInEffect
        207 -> return ParProtocolAbortMode
        26  -> return ParReportingCycle
        104 -> return ParRequestedControlWordType
        27  -> return ParRequestedFrameQuality
        28  -> return ParRequestedGvcid
        105 -> return ParRequestedTcVcid
        106 -> return ParRequestedUpdateMode
        29  -> return ParReturnTimeoutPeriod
        30  -> return ParRfAvailable
        31  -> return ParRfAvailableRequired
        32  -> return ParSegmentHeader
        303 -> return ParSequCntrFramesRepetition
        34  -> return ParSubcarrierToBitRateRatio
        304 -> return ParThrowEventOperation
        35  -> return ParTimeoutType
        36  -> return ParTimerInitial
        37  -> return ParTransmissionLimit
        38  -> return ParTransmitterFrameSequenceNumber
        39  -> return ParVcMuxControl
        40  -> return ParVcMuxScheme
        41  -> return ParVirtualChannel
        v ->
            throwError $ "Invalid ParameterName value: " <> fromString (show v)


encWord16 :: Word16 -> ByteString
encWord16 v = val
  where
    byte0 = fromIntegral $ (v .&. 0xFF00) `shiftR` 8
    byte1 = fromIntegral $ v .&. 0xFF
    val   = if byte0 == 0 then B.singleton byte1 else B.pack [byte0, byte1]

decWord16 :: ByteString -> Maybe Word16
decWord16 bs = case B.length bs of
    1 -> let val = B.index bs 0 in Just (fromIntegral val)
    2 ->
        let b0  = B.index bs 0
            b1  = B.index bs 1
            val = (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1
        in  Just val
    _ -> Nothing


data ReportRequestType =
    ReportImmediately
    | ReportPeriodically !Word16
    | ReportStop
    deriving (Show, Generic)

reportRequestType :: ReportRequestType -> ASN1
reportRequestType ReportImmediately        = Other Context 0 B.empty
reportRequestType (ReportPeriodically tim) = Other Context 1 (encWord16 tim)
reportRequestType ReportStop               = Other Context 2 B.empty

parseReportRequestType :: Parser ReportRequestType
parseReportRequestType = do
    x <- get
    case x of
        Other Context 0 _ : rest -> do
            put rest
            return ReportImmediately
        Other Context 1 bs : rest -> do
            put rest
            case decWord16 bs of
                Just val -> return (ReportPeriodically val)
                Nothing  -> do
                    throwError
                        "Could not parse ReportPeriodically time value: illegal lengtth (max 16 bit)"
        Other Context 2 _ : rest -> do
            put rest
            return ReportStop
        a@(asn1 : _) -> do 
            put a
            throwError
                $  "parseReportRequestType: unexpected ASN1 sequence: "
                <> fromString (show asn1)
        asn1 -> do 
            put asn1
            throwError
                $  "parseReportRequestType: unexpected ASN1 sequence: "
                <> fromString (show asn1)

data SleScheduleStatusReport = SleScheduleStatusReport
    { _sleSchedCredentials :: !Credentials
    , _sleSchedInvokeID    :: !Word16
    , _sleSchedRequestType :: !ReportRequestType
    }
    deriving (Show, Generic)
makeLenses ''SleScheduleStatusReport


sleScheduleStatusReport :: SleScheduleStatusReport -> [ASN1]
sleScheduleStatusReport SleScheduleStatusReport {..} =
    [ Start (Container Context 4)
    , credentials _sleSchedCredentials
    , IntVal (fromIntegral _sleSchedInvokeID)
    , reportRequestType _sleSchedRequestType
    , End (Container Context 4)
    ]

instance EncodeASN1 SleScheduleStatusReport where
    encode val = encodeASN1' DER (sleScheduleStatusReport val)

parseScheduleStatusReport :: Parser SleScheduleStatusReport
parseScheduleStatusReport = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 4)) (const ())

    content      = do
        creds      <- parseCredentials
        invokeID   <- parseIntVal
        reportType <- parseReportRequestType
        void endContainer
        return SleScheduleStatusReport
            { _sleSchedCredentials = creds
            , _sleSchedInvokeID    = fromIntegral invokeID
            , _sleSchedRequestType = reportType
            }


data DiagScheduleSpecificVal = DiagSchedNotSupportedInThisDeliveryMode
    | AlreadyStopped
    | InvalidReportingCycle
    deriving (Show, Generic)


data DiagScheduleStatus = DiagScheduleCommon !Diagnostics | DiagScheduleSpecific !DiagScheduleSpecificVal
    deriving (Show, Generic)

diagScheduleStatus :: DiagScheduleStatus -> ASN1
diagScheduleStatus (DiagScheduleCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagScheduleStatus (DiagScheduleSpecific DiagSchedNotSupportedInThisDeliveryMode)
    = Other Context 1 (B.singleton 0)
diagScheduleStatus (DiagScheduleSpecific AlreadyStopped) =
    Other Context 1 (B.singleton 1)
diagScheduleStatus (DiagScheduleSpecific InvalidReportingCycle) =
    Other Context 1 (B.singleton 2)



data DiagScheduleResult = DiagScheduleStatusPositive | DiagScheduleStatusNegative DiagScheduleStatus
    deriving (Show, Generic)

diagScheduleResult :: DiagScheduleResult -> [ASN1]
diagScheduleResult DiagScheduleStatusPositive = [Other Context 0 B.empty]
diagScheduleResult (DiagScheduleStatusNegative diag) =
    [ Start (Container Context 1)
    , diagScheduleStatus diag
    , End (Container Context 1)
    ]

parseDiagScheduleResult :: Parser DiagScheduleResult
parseDiagScheduleResult = do
    x <- get
    case x of
        Other Context 0 _ : rest -> do
            put rest
            return DiagScheduleStatusPositive
        Start (Container Context 1) : Other Context 0 bs : End (Container Context 1) : rest
            -> do
                put rest
                case decodeASN1' DER bs of
                    Left err ->
                        throwError
                            $ "Error decoding ScheduleStatusReturn diagnostics: "
                            <> fromString (show err)
                    Right v -> case parseASN1 parseDiagnostics v of
                        Left err ->
                            throwError
                                $ "Error decoding ScheduleStatusReturn diagnostics: "
                                <> fromString (show err)
                        Right diag ->
                            return
                                (DiagScheduleStatusNegative
                                    (DiagScheduleCommon diag)
                                )
        Start (Container Context 1) : Other Context 1 bs : End (Container Context 1) : rest
            -> do
                put rest
                if B.length bs >= 1
                    then do
                        case B.index bs 0 of
                            0 -> return
                                (DiagScheduleStatusNegative
                                    (DiagScheduleSpecific
                                        DiagSchedNotSupportedInThisDeliveryMode
                                    )
                                )
                            1 ->
                                return
                                    (DiagScheduleStatusNegative
                                        (DiagScheduleSpecific AlreadyStopped)
                                    )
                            2 -> return
                                (DiagScheduleStatusNegative
                                    (DiagScheduleSpecific InvalidReportingCycle)
                                )
                            y ->
                                throwError
                                    $ "Error decoding ScheduleStatusReturn specific diagnostics, invalid value: "
                                    <> fromString (show y)
                    else do
                        throwError
                            $ "Error decoding ScheduleStatusReturn specific diagnostics: bytestring too short"
        a@(asn1 : _rest) -> do 
            put a 
            throwError
                $ "Error decoding ScheduleStatusReturn specific diagnostics: unexpected ASN1: "
                <> fromString (show asn1)
        asn1 -> do
            put asn1
            throwError
                $ "Error decoding ScheduleStatusReturn specific diagnostics: unexpected ASN1: "
                <> fromString (show asn1)


data SleScheduleStatusReportReturn = SleScheduleStatusReportReturn
    { _sleSchedRetCredentials :: !Credentials
    , _sleSchedRetInvokeID    :: !Word16
    , _sleSchedRetResult      :: !DiagScheduleResult
    }
    deriving (Show, Generic)
makeLenses ''SleScheduleStatusReportReturn

sleScheduleStatusReportReturn :: SleScheduleStatusReportReturn -> [ASN1]
sleScheduleStatusReportReturn SleScheduleStatusReportReturn {..} =
    [ Start (Container Context 5)
        , credentials _sleSchedRetCredentials
        , IntVal (fromIntegral _sleSchedRetInvokeID)
        ]
        ++ diagScheduleResult _sleSchedRetResult
        ++ [End (Container Context 5)]


instance EncodeASN1 SleScheduleStatusReportReturn where
    encode val = encodeASN1' DER (sleScheduleStatusReportReturn val)

parseSleScheduleStatusReportReturn :: Parser SleScheduleStatusReportReturn
parseSleScheduleStatusReportReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 5)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        diag     <- parseDiagScheduleResult
        void endContainer
        return SleScheduleStatusReportReturn
            { _sleSchedRetCredentials = creds
            , _sleSchedRetInvokeID    = fromIntegral invokeID
            , _sleSchedRetResult      = diag
            }

data GetParameterInvocation = GetParameterInvocation
    { _gpCredentials :: !Credentials
    , _gpInvokeID    :: !Word16
    , _gpParameter   :: !ParameterName
    }
    deriving (Show, Generic)
makeLenses ''GetParameterInvocation



getParameterInvocation :: GetParameterInvocation -> [ASN1]
getParameterInvocation GetParameterInvocation {..} =
    [ Start (Container Context 6)
    , credentials _gpCredentials
    , IntVal (fromIntegral _gpInvokeID)
    , parameterName _gpParameter
    , End (Container Context 6)
    ]

parseGetParameterInvocation :: Parser GetParameterInvocation
parseGetParameterInvocation = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 6)) (const ())

    content      = do
        creds     <- parseCredentials
        invokeID  <- parseIntVal
        parameter <- parseParameterName
        void endContainer
        return GetParameterInvocation { _gpCredentials = creds
                                      , _gpInvokeID    = fromIntegral invokeID
                                      , _gpParameter   = parameter
                                      }

instance EncodeASN1 GetParameterInvocation where
    encode val = encodeASN1' DER (getParameterInvocation val)



data DeliveryMode =
    DelRtnTimelyOnline
    | DelRtnCompleteOnline
    | DelRtnOffline
    | FwdOnline
    | FwdOffline
    deriving stock (Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

deliveryMode :: DeliveryMode -> ASN1
deliveryMode DelRtnTimelyOnline   = IntVal 0
deliveryMode DelRtnCompleteOnline = IntVal 1
deliveryMode DelRtnOffline        = IntVal 2
deliveryMode FwdOnline            = IntVal 3
deliveryMode FwdOffline           = IntVal 4

parseDeliveryMode :: Parser DeliveryMode
parseDeliveryMode = do
    x <- parseIntVal
    case x of
        0 -> return DelRtnTimelyOnline
        1 -> return DelRtnCompleteOnline
        2 -> return DelRtnOffline
        3 -> return FwdOnline
        4 -> return FwdOffline
        v ->
            throwError
                $  "Could not parse Deliver Mode, illegal value: "
                <> fromString (show v)

