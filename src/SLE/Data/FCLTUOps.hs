{-# LANGUAGE TemplateHaskell #-}
module SLE.Data.FCLTUOps
    ( CltuIdentification(..)
    , FcltuStartInvocation(..)
    , EventInvocationID(..)
    , SlduStatusNotification(..)
    , Duration(..)
    , parseFcltuStart
    , parseFcltuThrowEvent
    , parseFcltuTransDataInvocation
    , fcltuStartCredentials
    , fcltuStartInvokeID
    , fcluStartFirstCltuIdentification
    , fcltuThrowCredentials
    , fcltuThrowInvokeID
    , fcltuThrowEventInvocationIdent
    , fcltuThrowEventIdentifier
    , fcltuThrowEventQualifier
    , fcltuDataCredentials
    , fcltuDataInvokeID
    , fcltuDataIdent
    , fcltuDataEarliestTransmission
    , fcltuDataLatestTransmission
    , fcltuDataDelayTime
    , fcltuDataRadiationNotification
    , fcltuData
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim
import           Data.ASN1.Types

import           SLE.Data.Common


newtype CltuIdentification = CltuIdentification Word32
    deriving (Show, Generic)

cltuIdentification :: CltuIdentification -> ASN1
cltuIdentification (CltuIdentification x) = IntVal (fromIntegral x)

parseCltuIdentification :: Parser CltuIdentification
parseCltuIdentification = CltuIdentification . fromIntegral <$> parseIntVal

newtype EventInvocationID = EventInvocationID Word32
    deriving (Show, Generic)

eventInvocationID :: EventInvocationID -> ASN1
eventInvocationID (EventInvocationID x) = IntVal (fromIntegral x)

parseEventInvocationID :: Parser EventInvocationID
parseEventInvocationID = EventInvocationID . fromIntegral <$> parseIntVal


newtype Duration = Duration Word32
    deriving (Show, Generic)

duration :: Duration -> ASN1
duration (Duration x) = IntVal (fromIntegral x)

parseDuration :: Parser Duration
parseDuration = Duration . fromIntegral <$> parseIntVal



data FcltuStartInvocation = FcltuStartInvocation
    { _fcltuStartCredentials            :: !Credentials
    , _fcltuStartInvokeID               :: !Word16
    , _fcluStartFirstCltuIdentification :: !CltuIdentification
    }
    deriving (Show, Generic)
makeLenses ''FcltuStartInvocation


fcltuStartInvocation :: FcltuStartInvocation -> [ASN1]
fcltuStartInvocation FcltuStartInvocation {..} =
    [ Start (Container Context 0)
    , credentials _fcltuStartCredentials
    , IntVal (fromIntegral _fcltuStartInvokeID)
    , cltuIdentification _fcluStartFirstCltuIdentification
    , End (Container Context 0)
    ]


parseFcltuStart :: Parser FcltuStartInvocation
parseFcltuStart = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 0)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        start    <- parseCltuIdentification
        void endContainer
        return FcltuStartInvocation
            { _fcltuStartCredentials            = creds
            , _fcltuStartInvokeID               = fromIntegral invokeID
            , _fcluStartFirstCltuIdentification = start
            }

instance EncodeASN1 FcltuStartInvocation where
    encode val = encodeASN1' DER (fcltuStartInvocation val)


data FcltuThrowEventInvocation = FcltuThrowEventInvocation
    { _fcltuThrowCredentials          :: !Credentials
    , _fcltuThrowInvokeID             :: !Word16
    , _fcltuThrowEventInvocationIdent :: !EventInvocationID
    , _fcltuThrowEventIdentifier      :: !Word16
    , _fcltuThrowEventQualifier       :: !ByteString
    }
    deriving (Show, Generic)
makeLenses ''FcltuThrowEventInvocation

fcltuThrowEventInvocation :: FcltuThrowEventInvocation -> [ASN1]
fcltuThrowEventInvocation FcltuThrowEventInvocation {..} =
    [ Start (Container Context 8)
    , credentials _fcltuThrowCredentials
    , IntVal (fromIntegral _fcltuThrowInvokeID)
    , eventInvocationID _fcltuThrowEventInvocationIdent
    , IntVal (fromIntegral _fcltuThrowEventIdentifier)
    , OctetString _fcltuThrowEventQualifier
    , End (Container Context 8)
    ]


parseFcltuThrowEvent :: Parser FcltuThrowEventInvocation
parseFcltuThrowEvent = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 8)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        start    <- parseEventInvocationID
        eventID  <- parseIntVal
        dat      <- parseOctetString
        void endContainer
        return FcltuThrowEventInvocation
            { _fcltuThrowCredentials          = creds
            , _fcltuThrowInvokeID             = fromIntegral invokeID
            , _fcltuThrowEventInvocationIdent = start
            , _fcltuThrowEventIdentifier      = fromIntegral eventID
            , _fcltuThrowEventQualifier       = dat
            }

instance EncodeASN1 FcltuThrowEventInvocation where
    encode val = encodeASN1' DER (fcltuThrowEventInvocation val)


data SlduStatusNotification = ProduceNotification | DoNotProduceNotification
    deriving stock (Eq, Ord, Enum, Show, Generic)
    deriving anyclass (NFData)

slduStatusNotification :: SlduStatusNotification -> ASN1
slduStatusNotification ProduceNotification      = IntVal 0
slduStatusNotification DoNotProduceNotification = IntVal 1

parseSlduStatusNotification :: Parser SlduStatusNotification
parseSlduStatusNotification = do
    v <- parseIntVal
    case v of
        0 -> return ProduceNotification
        1 -> return DoNotProduceNotification
        _ -> return DoNotProduceNotification


data FcltuTransDataInvocation = FcltuTransDataInvocation
    { _fcltuDataCredentials           :: !Credentials
    , _fcltuDataInvokeID              :: !Word16
    , _fcltuDataIdent                 :: !CltuIdentification
    , _fcltuDataEarliestTransmission  :: !ConditionalTime
    , _fcltuDataLatestTransmission    :: !ConditionalTime
    , _fcltuDataDelayTime             :: !Duration
    , _fcltuDataRadiationNotification :: !SlduStatusNotification
    , _fcltuData                      :: !ByteString
    }
    deriving (Show, Generic)
makeLenses ''FcltuTransDataInvocation

fcltuTransDataInvocation :: FcltuTransDataInvocation -> [ASN1]
fcltuTransDataInvocation FcltuTransDataInvocation {..} =
    [ Start (Container Context 10)
    , credentials _fcltuDataCredentials
    , IntVal (fromIntegral _fcltuDataInvokeID)
    , cltuIdentification _fcltuDataIdent
    , conditionalTime _fcltuDataEarliestTransmission
    , conditionalTime _fcltuDataLatestTransmission
    , duration _fcltuDataDelayTime
    , slduStatusNotification _fcltuDataRadiationNotification
    , OctetString _fcltuData
    , End (Container Context 10)
    ]

parseFcltuTransDataInvocation :: Parser FcltuTransDataInvocation
parseFcltuTransDataInvocation = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 10)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        ident    <- parseCltuIdentification
        earliest <- parseConditionalTime
        latest   <- parseConditionalTime
        dur      <- parseDuration
        notif    <- parseSlduStatusNotification
        dat      <- parseOctetString
        void endContainer
        return FcltuTransDataInvocation
            { _fcltuDataCredentials           = creds
            , _fcltuDataInvokeID              = fromIntegral invokeID
            , _fcltuDataIdent                 = ident
            , _fcltuDataEarliestTransmission  = earliest
            , _fcltuDataLatestTransmission    = latest
            , _fcltuDataDelayTime             = dur
            , _fcltuDataRadiationNotification = notif
            , _fcltuData                      = dat
            }

instance EncodeASN1 FcltuTransDataInvocation where
    encode val = encodeASN1' DER (fcltuTransDataInvocation val)
