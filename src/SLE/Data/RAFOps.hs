{-# LANGUAGE TemplateHaskell #-}
module SLE.Data.RAFOps
    ( FrameQuality(..)
    , ReqFrameQuality(..)
    , RafStartInvocation(..)
    , RafStartReturn(..)
    , DiagnosticRafStart(..)
    , RafStartSpecific(..)
    , rafStartCredentials
    , rafStartInvokeID
    , rafStartTime
    , rafStopTime
    , rafStartRequestedTimeQual
    , parseRafStart
    , parseRafStartReturn
    , rafStartRetCredentials
    , rafStartRetInvokeID
    , rafStartRetResult
    , RafTransferBuffer
    , RafTransferDataInvocation(..)
    , RafSyncNotifyInvocation(..)
    , FrameOrNotification(..)
    , BadType(..)
    , isFrameBad
    , LockStatus(..)
    , LockStatusReport(..)
    , lockStatRepTime
    , lockStatRepCarrierLockStatus
    , lockStatRepSubCarrierLockStatus
    , lockStatRepSymbolLockStatus
    , RafProductionStatus(..)
    , rafSyncNCredentials
    , rafSyncNNotification
    , parseTransferBuffer
    , parseFrameOrNotification
    , parseRafTransferDataInvocation
    , rafTransCredentials
    , rafTransERT
    , rafTransAntennaID
    , rafTransDataContinuity
    , rafTransFrameQuality
    , rafTransPrivateAnnotation
    , rafTransData
    , RafGetParameter(..)
    , RafGetParameterReturn(..)
    , rgpCredentials
    , rgpInvokeID
    , rgpResult
    , DiagnosticRafGet(..)
    , RafDiagRafGetSpecific(..)
    , parseRafGetParameterReturn
    , RafStatusReport(..)
    , parseRafStatusReport
    , rstrCredentials
    , rstrErrorFreeFrameNumber
    , rstrDeliveredFrameNumber
    , rstrFrameSyncLockStatus
    , rstrSymbolSyncLockStatus
    , rstrSubcarrierLockStatus
    , rstrCarrierLockStatus
    , rstrProductionStatus
    , reqFrameQualitySet
    , parseReqFrameQualitySet
    ) where

import           RIO
import qualified RIO.ByteString                as B
import           RIO.State

import           Control.Lens                   ( makeLenses )
import           Control.Monad.Except

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim
import           Data.ASN1.Types

import           SLE.Data.Common

--import           Text.Show.Pretty               ( ppShow )


data FrameQuality = FrameGood | FrameErred | FrameUndetermined
    deriving stock (Eq, Ord, Enum, Show, Generic)
    deriving anyclass (NFData)

frameQuality :: FrameQuality -> ASN1
frameQuality FrameGood  = IntVal 0
frameQuality FrameErred = IntVal 1
frameQuality _          = IntVal 2

parseFrameQuality :: Parser FrameQuality
parseFrameQuality = do
    v <- parseIntVal
    case v of
        0 -> return FrameGood
        1 -> return FrameErred
        2 -> return FrameUndetermined
        _ -> return FrameUndetermined



data ReqFrameQuality = GoodOnly | ErredOnly | AllFrames | FrameQualInvalid
    deriving stock (Eq, Ord, Enum, Show, Generic)
    deriving anyclass (NFData)

instance Display ReqFrameQuality where
    display GoodOnly         = "GOOD"
    display ErredOnly        = "ERRED"
    display AllFrames        = "ALL"
    display FrameQualInvalid = "INVALID"


reqFrameQuality :: ReqFrameQuality -> ASN1
reqFrameQuality GoodOnly         = IntVal 0
reqFrameQuality ErredOnly        = IntVal 1
reqFrameQuality AllFrames        = IntVal 2
reqFrameQuality FrameQualInvalid = IntVal 127

reqFrameQualitySet :: [ReqFrameQuality] -> [ASN1]
reqFrameQualitySet lst = Start Set : map reqFrameQuality lst ++ [End Set]


parseReqFrameQuality :: Parser ReqFrameQuality
parseReqFrameQuality = do
    v <- parseIntVal
    case v of
        0 -> return GoodOnly
        1 -> return ErredOnly
        2 -> return AllFrames
        _ -> return FrameQualInvalid

parseReqFrameQualitySet :: Parser [ReqFrameQuality]
parseReqFrameQualitySet = do
    parseSet parseReqFrameQuality


data RafStartInvocation = RafStartInvocation
    { _rafStartCredentials       :: !Credentials
    , _rafStartInvokeID          :: !Word16
    , _rafStartTime              :: !ConditionalTime
    , _rafStopTime               :: !ConditionalTime
    , _rafStartRequestedTimeQual :: !ReqFrameQuality
    }
    deriving (Show, Generic)
makeLenses ''RafStartInvocation



rafStartInvocation :: RafStartInvocation -> [ASN1]
rafStartInvocation RafStartInvocation {..} =
    [ Start (Container Context 0)
        , credentials _rafStartCredentials
        , IntVal (fromIntegral _rafStartInvokeID)
        ]
        ++ conditionalTime _rafStartTime
        ++ conditionalTime _rafStopTime
        ++ [ reqFrameQuality _rafStartRequestedTimeQual
           , End (Container Context 0)
           ]

parseRafStart :: Parser RafStartInvocation
parseRafStart = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 0)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        start    <- parseConditionalTime
        stop     <- parseConditionalTime
        qual     <- parseReqFrameQuality
        void endContainer
        return RafStartInvocation { _rafStartCredentials       = creds
                                  , _rafStartInvokeID = fromIntegral invokeID
                                  , _rafStartTime              = start
                                  , _rafStopTime               = stop
                                  , _rafStartRequestedTimeQual = qual
                                  }

instance EncodeASN1 RafStartInvocation where
    encode val = encodeASN1' DER (rafStartInvocation val)



data RafStartSpecific = RafStartOutOfService
        | RafStartUnableToComply
        | RafStartInvalidStartTime
        | RafStartInvalidStopTime
        | RafStartMissingTimeValue
        | RafStartInvalid
    deriving(Eq, Ord, Enum, Show, Generic)

rafStartSpecific :: RafStartSpecific -> ASN1
rafStartSpecific RafStartOutOfService     = IntVal 0
rafStartSpecific RafStartUnableToComply   = IntVal 1
rafStartSpecific RafStartInvalidStartTime = IntVal 2
rafStartSpecific RafStartInvalidStopTime  = IntVal 3
rafStartSpecific RafStartMissingTimeValue = IntVal 4
rafStartSpecific RafStartInvalid          = IntVal 127

rafStartSpecificFromInt :: ASN1 -> RafStartSpecific
rafStartSpecificFromInt (IntVal 0) = RafStartOutOfService
rafStartSpecificFromInt (IntVal 1) = RafStartUnableToComply
rafStartSpecificFromInt (IntVal 2) = RafStartInvalidStartTime
rafStartSpecificFromInt (IntVal 3) = RafStartInvalidStopTime
rafStartSpecificFromInt (IntVal 4) = RafStartMissingTimeValue
rafStartSpecificFromInt _          = RafStartInvalid


data DiagnosticRafStart = DiagRafStartCommon Diagnostics | DiagRafStartSpecific RafStartSpecific
    deriving(Show, Generic)

diagnosticRafStart :: DiagnosticRafStart -> ASN1
diagnosticRafStart (DiagRafStartCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagnosticRafStart (DiagRafStartSpecific diag) =
    Other Context 1 (encodeASN1' DER [rafStartSpecific diag])


optionalDiagRafStart :: Maybe DiagnosticRafStart -> ASN1
optionalDiagRafStart Nothing = Other Context 0 B.empty
optionalDiagRafStart (Just diag) =
    Other Context 1 (encodeASN1' DER [diagnosticRafStart diag])

parseOptionalRafStartDiag :: Parser (Maybe DiagnosticRafStart)
parseOptionalRafStartDiag = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return Nothing
        ((Other Context 1 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError
                        $ "Diagnostic RAF Start Parser: could not decode diagnostics: "
                        <> fromString (show err)
                Right v -> case parseASN1 parseRafStartDiag v of
                    Left err ->
                        throwError
                            $ "Diagnostic RAF Start Parser: could not decode diagnostics: "
                            <> fromString (show err)
                    Right diag -> return (Just diag)
        (asn1 : _) -> do
            throwError
                $  "Diagnostic RAF Start Parser: unexpected ASN1 value: "
                <> fromString (show asn1)
        [] -> do
            throwError "Diagnostic RAF Start Parser: no value found."

parseRafStartDiag :: Parser DiagnosticRafStart
parseRafStartDiag = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic RAF Start Parser: could not decode common diagnostic: "
                        <> fromString (show err)
                Right v -> return (DiagRafStartCommon (diagnosticsFromInt v))
        ((Other Context 1 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic RAF Start Parser: could not decode specific diagnostic"
                        <> fromString (show err)
                Right v ->
                    return (DiagRafStartSpecific (rafStartSpecificFromInt v))
        _ -> throwError
            "RAF Start diagnostics parser: could not parse diagnostics"


data RafStartReturn = RafStartReturn
    { _rafStartRetCredentials :: !Credentials
    , _rafStartRetInvokeID    :: !Word16
    , _rafStartRetResult      :: !(Maybe DiagnosticRafStart)
    }
    deriving (Show, Generic)
makeLenses ''RafStartReturn

rafStartReturn :: RafStartReturn -> [ASN1]
rafStartReturn RafStartReturn {..} =
    [ Start (Container Context 1)
    , credentials _rafStartRetCredentials
    , IntVal (fromIntegral _rafStartRetInvokeID)
    , optionalDiagRafStart _rafStartRetResult
    , End (Container Context 1)
    ]


parseRafStartReturn :: Parser RafStartReturn
parseRafStartReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 1)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        diag     <- parseOptionalRafStartDiag
        void endContainer
        return RafStartReturn { _rafStartRetCredentials = creds
                              , _rafStartRetInvokeID    = fromIntegral invokeID
                              , _rafStartRetResult      = diag
                              }

instance EncodeASN1 RafStartReturn where
    encode val = encodeASN1' DER (rafStartReturn val)


type CarrierLockStatus = LockStatus
type FrameSyncLockStatus = LockStatus

data LockStatus = InLock | OutOfLock | NotInUse | LockStatusUnknown
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

lockStatus :: LockStatus -> ASN1
lockStatus InLock            = IntVal 0
lockStatus OutOfLock         = IntVal 1
lockStatus NotInUse          = IntVal 2
lockStatus LockStatusUnknown = IntVal 3

parseLockStatus :: Parser LockStatus
parseLockStatus = do
    x <- parseIntVal
    case x of
        0 -> return InLock
        1 -> return OutOfLock
        2 -> return NotInUse
        _ -> return LockStatusUnknown



type SymbolLockStatus = LockStatus

data RafProductionStatus =
    ProdRunning
    | ProdInterrupted
    | ProdHalted
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

rafProductionStatus :: RafProductionStatus -> ASN1
rafProductionStatus ProdRunning     = IntVal 0
rafProductionStatus ProdInterrupted = IntVal 1
rafProductionStatus ProdHalted      = IntVal 2

intToRafProductionStatus :: Int -> Maybe RafProductionStatus
intToRafProductionStatus 0 = Just ProdRunning
intToRafProductionStatus 1 = Just ProdInterrupted
intToRafProductionStatus 2 = Just ProdHalted
intToRafProductionStatus _ = Nothing

parseRafProductionStatus :: Parser RafProductionStatus
parseRafProductionStatus = do
    x <- parseIntVal
    case x of
        0 -> return ProdRunning
        1 -> return ProdInterrupted
        2 -> return ProdHalted
        v ->
            throwError
                $  "Illegal value for RAF production status: "
                <> fromString (show v)


data LockStatusReport = LockStatusReport
    { _lockStatRepTime                 :: !Time
    , _lockStatRepCarrierLockStatus    :: !CarrierLockStatus
    , _lockStatRepSubCarrierLockStatus :: !LockStatus
    , _lockStatRepSymbolLockStatus     :: !SymbolLockStatus
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData
makeLenses ''LockStatusReport

lockStatusReport :: LockStatusReport -> [ASN1]
lockStatusReport LockStatusReport {..} =
    [ Start Sequence
    , time _lockStatRepTime
    , lockStatus _lockStatRepCarrierLockStatus
    , lockStatus _lockStatRepSubCarrierLockStatus
    , lockStatus _lockStatRepSymbolLockStatus
    , End Sequence
    ]

parseLockStatusReport :: Parser LockStatusReport
parseLockStatusReport = do
    parseSequence lockStatP
  where
    lockStatP = do
        t    <- parseTime
        cls  <- parseLockStatus
        scls <- parseLockStatus
        sls  <- parseLockStatus
        return $ LockStatusReport t cls scls sls


data Notification =
    LossFrameSync !LockStatusReport
    | ProductionStatusChange !RafProductionStatus
    | ExcessiveDataBacklog
    | EndOfData
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

notification :: Notification -> ASN1
notification (LossFrameSync v) =
    Other Context 0 (encodeASN1' DER (lockStatusReport v))
notification (ProductionStatusChange v) =
    Other Context 1 (encodeASN1' DER [rafProductionStatus v])
notification ExcessiveDataBacklog = Other Context 2 (encodeASN1' DER [Null])
notification EndOfData            = Other Context 3 (encodeASN1' DER [Null])


parseNotification :: Parser Notification
parseNotification = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError
                        $  "Error parsing LockStatusReport: "
                        <> fromString (show err)
                Right vals -> case parseASN1 parseLockStatusReport vals of
                    Left err ->
                        throwError
                            $  "Error parsing LockStatusReport: "
                            <> fromString (show err)
                    Right rep -> return $ LossFrameSync rep
        ((Other Context 1 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError
                        $  "Error parsing RAF Production Status: "
                        <> fromString (show err)
                Right (IntVal stat : _) -> maybe
                    (throwError
                        (  "Illegal value for RAF Production Status: "
                        <> fromString (show stat)
                        )
                    )
                    (return . ProductionStatusChange)
                    (intToRafProductionStatus (fromIntegral stat))
                _ -> throwError "Illegal value for RAF Production Status"
        ((Other Context 2 _) : rest) -> do
            put rest
            return ExcessiveDataBacklog
        ((Other Context 3 _) : rest) -> do
            put rest
            return EndOfData
        _ -> throwError "Could not parse Notification"




data RafSyncNotifyInvocation = RafSyncNotifyInvocation
    { _rafSyncNCredentials  :: !Credentials
    , _rafSyncNNotification :: !Notification
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData
makeLenses ''RafSyncNotifyInvocation


rafSyncNotification :: RafSyncNotifyInvocation -> [ASN1]
rafSyncNotification RafSyncNotifyInvocation {..} =
    [ Start Sequence
    , credentials _rafSyncNCredentials
    , notification _rafSyncNNotification
    , End Sequence
    ]

parseRafSyncNotification :: Parser RafSyncNotifyInvocation
parseRafSyncNotification = do
    parseSequence body
  where
    body = do
        creds <- parseCredentials
        notif <- parseNotification
        return $ RafSyncNotifyInvocation { _rafSyncNCredentials  = creds
                                         , _rafSyncNNotification = notif
                                         }


data RafTransferDataInvocation = RafTransferDataInvocation
    { _rafTransCredentials       :: !Credentials
    , _rafTransERT               :: !Time
    , _rafTransAntennaID         :: !AntennaID
    , _rafTransDataContinuity    :: !Int32
    , _rafTransFrameQuality      :: !FrameQuality
    , _rafTransPrivateAnnotation :: !PrivateAnnotation
    , _rafTransData              :: !ByteString
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData
makeLenses ''RafTransferDataInvocation


rafTransferDataInvocation :: RafTransferDataInvocation -> [ASN1]
rafTransferDataInvocation RafTransferDataInvocation {..} =
    let dat =
            [ credentials _rafTransCredentials
            , time _rafTransERT
            , antennaID _rafTransAntennaID
            , IntVal (fromIntegral _rafTransDataContinuity)
            , frameQuality _rafTransFrameQuality
            , privateAnnotation _rafTransPrivateAnnotation
            , OctetString _rafTransData
            ]
    in                                 {- trace ("TransferData: " <> fromString (ppShow dat))-}
        dat

parseRafTransferDataInvocation :: Parser RafTransferDataInvocation
parseRafTransferDataInvocation = do
    creds   <- parseCredentials
    ert     <- parseTime
    antenna <- parseAntennaID
    cont    <- parseIntVal
    qual    <- parseFrameQuality
    priv    <- parsePrivateAnnotation
    dat     <- parseOctetString
    return $ RafTransferDataInvocation
        { _rafTransCredentials       = creds
        , _rafTransERT               = ert
        , _rafTransAntennaID         = antenna
        , _rafTransDataContinuity    = fromIntegral cont
        , _rafTransFrameQuality      = qual
        , _rafTransPrivateAnnotation = priv
        , _rafTransData              = dat
        }

data FrameOrNotification = TransFrame !RafTransferDataInvocation | TransNotification !RafSyncNotifyInvocation
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data BadType = NoFrame | BadFrame | GoodFrame

isFrameBad :: FrameOrNotification -> BadType
isFrameBad (TransFrame frame) =
    if frame ^. rafTransFrameQuality /= FrameGood then BadFrame else GoodFrame
isFrameBad (TransNotification _) = NoFrame


frameOrNotification :: FrameOrNotification -> [ASN1]
frameOrNotification (TransFrame fr) =
    Start (Container Context 0)
        :  rafTransferDataInvocation fr
        ++ [End (Container Context 0)]
frameOrNotification (TransNotification n) =
    Start (Container Context 1)
        :  rafSyncNotification n
        ++ [End (Container Context 1)]

parseFrameOrNotification :: Parser FrameOrNotification
parseFrameOrNotification = do
    x <- get
    case x of
        (Start (Container Context 0) : rest) -> do
            put rest
            pdu <- parseRafTransferDataInvocation
            void $ endContainer 0
            return $ TransFrame pdu
        (Start (Container Context 1) : rest) -> do
            put rest
            pdu <- parseRafSyncNotification
            void $ endContainer 1
            return $ TransNotification pdu
        _ -> throwError "Could not parse FrameOrNotification"
  where
    endContainer x = parseBasicASN1 (== End (Container Context x)) (const ())


type RafTransferBuffer = [FrameOrNotification]

rafTransferBuffer :: RafTransferBuffer -> [ASN1]
rafTransferBuffer buf =
    let dat =
            Start (Container Context 8)
                :  concatMap frameOrNotification buf
                ++ [End (Container Context 8)]
    in                                 {- trace ("TransferBuffer: " <> fromString (ppShow dat)) -}
        dat


instance EncodeASN1 RafTransferBuffer where
    encode buf = encodeASN1' DER (rafTransferBuffer buf)

parseTransferBuffer :: Parser RafTransferBuffer
parseTransferBuffer = do
    buf <- manyA parseFrameOrNotification
    void endContainer
    return buf
  where
    endContainer = parseBasicASN1 (== End (Container Context 8)) (const ())



data RafStatusReport = RafStatusReport
    { _rstrCredentials          :: !Credentials
    , _rstrErrorFreeFrameNumber :: !Word32
    , _rstrDeliveredFrameNumber :: !Word32
    , _rstrFrameSyncLockStatus  :: !FrameSyncLockStatus
    , _rstrSymbolSyncLockStatus :: !SymbolLockStatus
    , _rstrSubcarrierLockStatus :: !LockStatus
    , _rstrCarrierLockStatus    :: !CarrierLockStatus
    , _rstrProductionStatus     :: !RafProductionStatus
    }
    deriving (Show, Generic)
makeLenses ''RafStatusReport

rafStatusReport :: RafStatusReport -> [ASN1]
rafStatusReport RafStatusReport {..} =
    [ Start (Container Context 9)
    , credentials _rstrCredentials
    , IntVal (fromIntegral _rstrErrorFreeFrameNumber)
    , IntVal (fromIntegral _rstrDeliveredFrameNumber)
    , lockStatus _rstrFrameSyncLockStatus
    , lockStatus _rstrSymbolSyncLockStatus
    , lockStatus _rstrSubcarrierLockStatus
    , lockStatus _rstrCarrierLockStatus
    , rafProductionStatus _rstrProductionStatus
    , End (Container Context 9)
    ]

instance EncodeASN1 RafStatusReport where
    encode val = encodeASN1' DER (rafStatusReport val)


parseRafStatusReport :: Parser RafStatusReport
parseRafStatusReport = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 9)) (const ())

    content      = do
        creds           <- parseCredentials
        errorFrames     <- parseIntVal
        deliveredFrames <- parseIntVal
        frameSync       <- parseLockStatus
        symbolSync      <- parseLockStatus
        subcarrierLock  <- parseLockStatus
        carrierLock     <- parseLockStatus
        prod            <- parseRafProductionStatus
        void endContainer
        return RafStatusReport
            { _rstrCredentials          = creds
            , _rstrErrorFreeFrameNumber = fromIntegral errorFrames
            , _rstrDeliveredFrameNumber = fromIntegral deliveredFrames
            , _rstrFrameSyncLockStatus  = frameSync
            , _rstrSymbolSyncLockStatus = symbolSync
            , _rstrSubcarrierLockStatus = subcarrierLock
            , _rstrCarrierLockStatus    = carrierLock
            , _rstrProductionStatus     = prod
            }




data RafGetParameter =
    RafParBufferSize !Word16
    | RafParDeliveryMode !DeliveryMode
    | RafParLatencyLimit !(Maybe Word16)
    | RafParReqFrameQuality !ReqFrameQuality
    | RafParMinReportingCycle !Word16
    | RafParReturnTimeout !Word16
    | RafParPermittedFrameQuality ![ReqFrameQuality]
    | RafParReportingCycle !(Maybe Word16)
    deriving (Show, Generic)


rafGetParameter :: RafGetParameter -> [ASN1]
rafGetParameter (RafParBufferSize size) =
    [ Start (Container Context 0)
    , parameterName ParBufferSize
    , IntVal (fromIntegral size)
    , End (Container Context 0)
    ]
rafGetParameter (RafParDeliveryMode mode) =
    [ Start (Container Context 1)
    , parameterName ParDeliveryMode
    , deliveryMode mode
    , End (Container Context 1)
    ]
rafGetParameter (RafParReqFrameQuality qual) =
    [ Start (Container Context 4)
    , parameterName ParRequestedFrameQuality
    , reqFrameQuality qual
    , End (Container Context 4)
    ]
rafGetParameter (RafParMinReportingCycle cycl) =
    [ Start (Container Context 7)
    , parameterName ParMinReportingCycle
    , IntVal (fromIntegral cycl)
    , End (Container Context 7)
    ]
rafGetParameter (RafParReturnTimeout val) =
    [ Start (Container Context 5)
    , parameterName ParReturnTimeoutPeriod
    , IntVal (fromIntegral val)
    , End (Container Context 5)
    ]
rafGetParameter (RafParPermittedFrameQuality val) =
    [Start (Container Context 6), parameterName ParPermittedFrameQuality]
        ++ reqFrameQualitySet val
        ++ [End (Container Context 6)]
rafGetParameter (RafParLatencyLimit (Just limit)) =
    [ Start (Container Context 2)
    , parameterName ParLatencyLimit
    , Other Context 0 (encWord16 limit)
    , End (Container Context 2)
    ]
rafGetParameter (RafParLatencyLimit Nothing) =
    [ Start (Container Context 2)
    , parameterName ParLatencyLimit
    , Other Context 1 B.empty
    , End (Container Context 2)
    ]
rafGetParameter (RafParReportingCycle (Just val)) =
    [ Start (Container Context 3)
    , parameterName ParReportingCycle
    , Other Context 1 (encWord16 val)
    , End (Container Context 3)
    ]
rafGetParameter (RafParReportingCycle Nothing) =
    [ Start (Container Context 3)
    , parameterName ParReportingCycle
    , Other Context 0 B.empty
    , End (Container Context 3)
    ]



parseRafGetParameter :: Parser RafGetParameter
parseRafGetParameter = do
    x <- get
    case x of
        Start (Container Context 0) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 0
            return (RafParBufferSize (fromIntegral val))
        Start (Container Context 1) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseDeliveryMode
            parseEndContainer 1
            return (RafParDeliveryMode val)
        Start (Container Context 2) : rest -> do
            put rest
            void $ parseParameterName
            latency <- parseChoice (return . decWord16)
                                   (\_ -> return Nothing)
                                   "Error parsing latency value"
            parseEndContainer 2
            return $ RafParLatencyLimit latency
        Start (Container Context 3) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseChoice (\_ -> return Nothing)
                               (return . decWord16)
                               "Error parsing reporting cycle"
            parseEndContainer 3
            return $ RafParReportingCycle val
        Start (Container Context 4) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseReqFrameQuality
            parseEndContainer 4
            return (RafParReqFrameQuality val)
        Start (Container Context 5) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 5
            return (RafParReturnTimeout (fromIntegral val))
        Start (Container Context 6) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseReqFrameQualitySet
            parseEndContainer 6
            return (RafParPermittedFrameQuality val)
        Start (Container Context 7) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 7
            return (RafParMinReportingCycle (fromIntegral val))
        asn1 -> do
            put asn1
            throwError
                $ "Error parsing returned parameter value: unexpected ASN1 value: "
                <> fromString (show asn1)



instance EncodeASN1 RafGetParameter where
    encode val = encodeASN1' DER (rafGetParameter val)


data RafDiagRafGetSpecific = RafUnknownParameter | RafDiagInvalid
    deriving(Eq, Ord, Enum, Show, Generic)

rafDiagRafGetSpecific :: RafDiagRafGetSpecific -> ASN1
rafDiagRafGetSpecific RafUnknownParameter = IntVal 0
rafDiagRafGetSpecific RafDiagInvalid      = IntVal 0

parseRafDiagRafGetSpecific :: Parser RafDiagRafGetSpecific
parseRafDiagRafGetSpecific = do
    x <- parseIntVal
    case x of
        0 -> return RafUnknownParameter
        _ -> return RafDiagInvalid

data DiagnosticRafGet = DiagRafGetCommon Diagnostics | DiagRafGetSpecific RafDiagRafGetSpecific
    deriving(Show, Generic)

diagnosticRafGet :: DiagnosticRafGet -> ASN1
diagnosticRafGet (DiagRafGetCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagnosticRafGet (DiagRafGetSpecific diag) =
    Other Context 1 (encodeASN1' DER [rafDiagRafGetSpecific diag])


parseDiagnosticRafGet :: Parser DiagnosticRafGet
parseDiagnosticRafGet = do
    res <- parseEitherASN1 parseDiagnostics parseRafDiagRafGetSpecific
    case res of
        Left  diag -> return $ DiagRafGetCommon diag
        Right diag -> return $ DiagRafGetSpecific diag

eitherRafGetResult :: Either DiagnosticRafGet RafGetParameter -> [ASN1]
eitherRafGetResult (Left diag) =
    [ Start (Container Context 1)
    , diagnosticRafGet diag
    , End (Container Context 1)
    ]
eitherRafGetResult (Right param) =
    Start (Container Context 0)
        :  rafGetParameter param
        ++ [End (Container Context 0)]


parseEitherRafGetResult :: Parser (Either DiagnosticRafGet RafGetParameter)
parseEitherRafGetResult = do
    x <- get
    case x of
        Start (Container Context 0) : rest -> do
            put rest
            param <- parseRafGetParameter
            parseEndContainer 0
            return (Right param)
        Start (Container Context 1) : rest -> do
            put rest
            diag <- parseDiagnosticRafGet
            parseEndContainer 1
            return (Left diag)
        asn1 ->
            throwError
                $ "Error parsing returned parameter value: unexpected ASN1 value: "
                <> fromString (show asn1)



data RafGetParameterReturn = RafGetParameterReturn
    { _rgpCredentials :: !Credentials
    , _rgpInvokeID    :: !Word16
    , _rgpResult      :: !(Either DiagnosticRafGet RafGetParameter)
    }
    deriving (Show, Generic)
makeLenses ''RafGetParameterReturn

rafGetParameterReturn :: RafGetParameterReturn -> [ASN1]
rafGetParameterReturn RafGetParameterReturn {..} =
    [ Start (Container Context 7)
        , credentials _rgpCredentials
        , IntVal (fromIntegral _rgpInvokeID)
        ]
        ++ eitherRafGetResult _rgpResult
        ++ [End (Container Context 7)]

instance EncodeASN1 RafGetParameterReturn where
    encode val = encodeASN1' DER (rafGetParameterReturn val)

parseRafGetParameterReturn :: Parser RafGetParameterReturn
parseRafGetParameterReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 7)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        result   <- parseEitherRafGetResult
        void endContainer
        return RafGetParameterReturn { _rgpCredentials = creds
                                     , _rgpInvokeID    = fromIntegral invokeID
                                     , _rgpResult      = result
                                     }

