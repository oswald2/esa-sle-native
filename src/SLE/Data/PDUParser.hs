module SLE.Data.PDUParser
    ( slePduParser
    ) where

import           RIO
import           RIO.State

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.FCLTUOps
import           SLE.Data.PDU
import           SLE.Data.RAFOps

import           Data.ASN1.Types

import           Control.Monad.Except

import           Text.Show.Pretty

import           Text.Builder                  as TB




slePduParser :: ApplicationIdentifier -> Parser SlePdu
slePduParser app = do
    x <- get
    case x of
        (Start (Container Context c) : rest) -> do
            put rest
            parsePDU app c
        _ -> throwError $ "slePduParser: cannot parse PDU: " <> fromString
            (ppShow x)


-- | Parses the SLE PDU by Tag 
-- Tags:
-- 100 : SLE Bind 
-- 101 : SLE Bind Return
-- 102 : SLE Unbind 
-- 103 : SLE Unbind Return 
-- 0   : RAF Start Invocation 
-- 1   : RAF Start Return
-- 2   : SLE Stop Invocation 
-- 3   : RAF Stop Returng 
-- 4   : RAF Schedule Status Report Invocation 
-- 5   : RAF Schedule Status Report Return 
-- 6   : Get Parameter Invocation 
-- 7   : RAF Get Parameter Return 
-- 8   : RAF Transfer Buffer / FCLTU Throw Event
-- 9   : RAF Status Report Invocation 
-- 10  : FCLTU Transfer Data Invocation
-- 104 : RAF Peer Abort
parsePDU :: ApplicationIdentifier -> ASN1Tag -> Parser SlePdu
parsePDU _            100 = SlePduBind <$> parseSleBind
parsePDU _            101 = SlePduBindReturn <$> parseSleBindReturn
parsePDU _            102 = SlePduUnbind <$> parseSleUnbind
parsePDU _            103 = SlePduUnbindReturn <$> parseSleUnbindReturn
parsePDU _            104 = SlePduPeerAbort <$> parseSlePeerAbort
parsePDU RtnAllFrames 0   = SlePduRafStart <$> parseRafStart
parsePDU RtnAllFrames 1   = SlePduRafStartReturn <$> parseRafStartReturn
parsePDU _            2   = SlePduStop <$> parseStopInvocation
parsePDU _            3   = SlePduAck <$> parseSleAcknowledgement
parsePDU _ 4 = SlePduScheduleStatusReport <$> parseScheduleStatusReport
parsePDU _ 5 =
    SlePduScheduleStatusReturn <$> parseSleScheduleStatusReportReturn
parsePDU RtnAllFrames 8  = SlePduRafTransferBuffer <$> parseTransferBuffer
parsePDU RtnAllFrames 9  = SlePduRafStatusReport <$> parseRafStatusReport
parsePDU FwdCltu      8  = SlePduFcltuThrowEvent <$> parseFcltuThrowEvent
parsePDU FwdCltu      0  = SlePduFcltuStart <$> parseFcltuStart
parsePDU FwdCltu      1  = SlePduFcltuStartReturn <$> parseFcltuStartReturn
parsePDU FwdCltu 10 = SlePduFcltuTransferData <$> parseFcltuTransDataInvocation
parsePDU FwdCltu 11 = SlePduFcltuTransReturn <$> parseFcltuTransferDataReturn
parsePDU FwdCltu      12 = SlePduFcltuAsync <$> parseFcltuAsyncStatus
parsePDU FwdCltu      13 = SlePduFcltuStatusReport <$> parseCltuStatusReport
parsePDU _            6  = SlePduGetParameter <$> parseGetParameterInvocation
parsePDU RtnAllFrames 7 =
    SlePduRafParameterReturn <$> parseRafGetParameterReturn
parsePDU FwdCltu 7 =
    SlePduFcltuParameterReturn <$> parseFcltuGetParameterReturn
parsePDU t x =
    throwError
        $  TB.run
        $  "SLE PDU not implemented yet: type "
        <> string (show t)
        <> " ASN1 Tag "
        <> decimal x
