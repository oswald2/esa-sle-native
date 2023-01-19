{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.TMLMessage
    ( TMLTypeID(..)
    , TMLHeader(..)
    , TMLCtxtMessage(..)
    , TMLContextMsgRead(..)
    , TMLHeartBeatMessage(..)
    , TMLMessage(..)
    , TMLPDU(..)
    , CtxtMsgError(..)
    , tmlHeaderBuilder
    , tmlMessageBuilder
    , tmlContextMsgBuilder
    , tmlHeartBeatMsgBuilder
    , tmlHeaderParser
    , tmlPduParser
    , tmlType
    , tmlLength
    , tmlMsgHdr
    , tmlMsgData
    , tmlSleMsg
    , tmlCtxHdr
    , tmlCtxProtocolID
    , tmlCtxVersion
    , tmlCtxHeartbeatInterval
    , tmlCtxDeadFactor
    , chkContextMsg
    ) where

import           Control.Lens
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.ByteString.Char8         as BC
import           RIO                     hiding ( Builder
                                                , (^.)
                                                )

--import           ByteString.StrictBuilder       ( Builder )
import           ByteString.StrictBuilder      as B

import           SLE.Data.TMLConfig


data TMLTypeID =
  TMLSlePdu
  | TMLContextMsg
  | TMLHeartbeat
  deriving (Eq, Ord, Enum, Show, Generic)


data TMLHeader = TMLHeader
    { _tmlType   :: TMLTypeID
    , _tmlLength :: Word32
    }
    deriving (Show, Generic)
makeLenses ''TMLHeader


data TMLMessage = TMLMessage
    { _tmlMsgHdr  :: TMLHeader
    , _tmlMsgData :: ByteString
    }
    deriving (Show, Generic)
makeLenses ''TMLMessage

tmlSleMsg :: ByteString -> TMLMessage
tmlSleMsg = TMLMessage (TMLHeader TMLSlePdu 0)


data TMLContextMsgRead = TMLContextMsgRead
    { _tmlCtxHdr               :: TMLHeader
    , _tmlCtxProtocolID        :: Text
    , _tmlCtxVersion           :: Int
    , _tmlCtxHeartbeatInterval :: Word16
    , _tmlCtxDeadFactor        :: Word16
    }
    deriving (Show, Generic)
makeLenses ''TMLContextMsgRead



data CtxtMsgError =
  IllegalHeartBeat
  | IllegalDeadFactor
  | IllegalProtocol
  | IllegalVersion
  deriving (Eq, Ord, Enum, Show, Read, Generic)

chkContextMsg :: TMLConfig -> TMLContextMsgRead -> Either CtxtMsgError ()
chkContextMsg cfg TMLContextMsgRead {..} =
    let chkHB =
            (_tmlCtxHeartbeatInterval == 0)
                || (  cfgMinHeartBeat cfg
                   <= _tmlCtxHeartbeatInterval
                   && _tmlCtxHeartbeatInterval
                   <= cfgMaxHeartBeat cfg
                   )
        chkDF =
            (_tmlCtxDeadFactor == 0)
                || (  cfgMinDeadFactor cfg
                   <= _tmlCtxDeadFactor
                   && _tmlCtxDeadFactor
                   <= cfgMaxDeadFactor cfg
                   )
        chkProtocol = _tmlCtxProtocolID == "ISP1"
        chkVersion  = _tmlCtxVersion == 1
    in  if
            | not chkProtocol -> Left IllegalProtocol
            | not chkVersion  -> Left IllegalVersion
            | not chkHB       -> Left IllegalHeartBeat
            | not chkDF       -> Left IllegalDeadFactor
            | otherwise       -> Right ()

data TMLCtxtMessage = TMLCtxtMessage
    { _tmlCtxHbt   :: Word16
    , _tmlCtxDeadf :: Word16
    }
    deriving (Show, Generic)

data TMLHeartBeatMessage = TMLHeartBeatMessage
    deriving (Show, Generic)


data TMLPDU =
  TMLPDUMessage TMLMessage
  | TMLPDUCtxt TMLContextMsgRead
  | TMLPDUHeartBeat
  deriving (Show, Generic)



tmlHeaderBuilder :: TMLHeader -> Builder
tmlHeaderBuilder TMLHeader {..} =
    case _tmlType of
            TMLSlePdu     -> word8 1
            TMLContextMsg -> word8 2
            TMLHeartbeat  -> word8 3
        <> word8 0
        <> word8 0
        <> word8 0
        <> word32BE _tmlLength

tmlHeaderParser :: Parser TMLHeader
tmlHeaderParser = do
    v <- A.anyWord8
    void $ A.take 3
    len <- A.anyWord32be
    case v of
        1 -> return (TMLHeader TMLSlePdu len)
        2 -> return (TMLHeader TMLContextMsg len)
        3 -> return (TMLHeader TMLHeartbeat len)
        _ -> fail $ "TML Message Header: illegal type: " <> show v


tmlMessageBuilder :: TMLMessage -> Builder
tmlMessageBuilder TMLMessage {..} =
    let len    = BC.length _tmlMsgData
        newHdr = _tmlMsgHdr { _tmlLength = fromIntegral len }
    in  tmlHeaderBuilder newHdr <> bytes _tmlMsgData


tmlPduParser :: Parser TMLPDU
tmlPduParser = do
    hdr <- tmlHeaderParser
    case hdr ^. tmlType of
        TMLSlePdu -> do
            dat <- A.take (fromIntegral (hdr ^. tmlLength))
            return $ TMLPDUMessage $ TMLMessage hdr dat
        TMLHeartbeat -> do
            if hdr ^. tmlLength == 0
                then return TMLPDUHeartBeat
                else
                    fail
                        "TML PDU Parser: error for heartbeat message: length is not 0!"
        TMLContextMsg -> do
            when (hdr ^. tmlLength /= 12)
                $ fail
                      "TML PDU Parser: error for context message: length is not 12!"
            protocol <- A.take 4
            when (protocol /= "ISP1")
                $  fail
                $  "TML PDU Parser: error: illegal protocol: "
                <> BC.unpack protocol
            void $ A.take 3
            void $ A.word8 1
            hb <- A.anyWord16be
            df <- A.anyWord16be
            case decodeUtf8' protocol of
                Left err ->
                    fail
                        $ "TML PDU Parser: could not decode protocol from UTF8: "
                        <> show err
                Right p ->
                    return $ TMLPDUCtxt $ TMLContextMsgRead hdr p 1 hb df


tmlContextMsgBuilder :: TMLCtxtMessage -> Builder
tmlContextMsgBuilder TMLCtxtMessage {..} =
    let hdr = TMLHeader { _tmlType = TMLContextMsg, _tmlLength = 12 }
    in  tmlHeaderBuilder hdr
            <> bytes (BC.pack "ISP1")
            <> word8 0
            <> word8 0
            <> word8 0
            <> word8 1
            <> word16BE _tmlCtxHbt
            <> word16BE _tmlCtxDeadf

tmlHeartBeatMsgBuilder :: TMLHeartBeatMessage -> Builder
tmlHeartBeatMsgBuilder _ =
    let hdr = TMLHeader { _tmlType = TMLHeartbeat, _tmlLength = 0 }
    in  tmlHeaderBuilder hdr
