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




slePduParser :: SleVersion -> ApplicationIdentifier -> Parser SlePdu
slePduParser version app = do
    x <- get
    case x of
        (Start (Container Context c) : rest) -> do
            put rest
            parsePDU version app c
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
parsePDU :: SleVersion -> ApplicationIdentifier -> ASN1Tag -> Parser SlePdu
parsePDU _ _            100 = SlePduBind <$> parseSleBind
parsePDU _ _            101 = SlePduBindReturn <$> parseSleBindReturn
parsePDU _ _            102 = SlePduUnbind <$> parseSleUnbind
parsePDU _ _            103 = SlePduUnbindReturn <$> parseSleUnbindReturn
parsePDU _ _            104 = SlePduPeerAbort <$> parseSlePeerAbort
parsePDU _ RtnAllFrames 0   = SlePduRafStart <$> parseRafStart
parsePDU _ RtnAllFrames 1   = SlePduRafStartReturn <$> parseRafStartReturn
parsePDU _ _            2   = SlePduStop <$> parseStopInvocation
parsePDU _ _            3   = SlePduAck <$> parseSleAcknowledgement
parsePDU _ _ 4 = SlePduScheduleStatusReport <$> parseScheduleStatusReport
parsePDU _ _ 5 =
    SlePduScheduleStatusReturn <$> parseSleScheduleStatusReportReturn
parsePDU _ RtnAllFrames 8 = SlePduRafTransferBuffer <$> parseTransferBuffer
parsePDU _ RtnAllFrames 9 = SlePduRafStatusReport <$> parseRafStatusReport
parsePDU _ FwdCltu      8 = SlePduFcltuThrowEvent <$> parseFcltuThrowEvent
parsePDU _ FwdCltu      0 = SlePduFcltuStart <$> parseFcltuStart
parsePDU _ FwdCltu      1 = SlePduFcltuStartReturn <$> parseFcltuStartReturn
parsePDU _ FwdCltu 10 =
    SlePduFcltuTransferData <$> parseFcltuTransDataInvocation
parsePDU _ FwdCltu 11 = SlePduFcltuTransReturn <$> parseFcltuTransferDataReturn
parsePDU _ FwdCltu 12 = SlePduFcltuAsync <$> parseFcltuAsyncStatus
parsePDU _ FwdCltu 13 = SlePduFcltuStatusReport <$> parseCltuStatusReport
parsePDU _ _       6  = SlePduGetParameter <$> parseGetParameterInvocation
parsePDU _ RtnAllFrames 7 =
    SlePduRafParameterReturn <$> parseRafGetParameterReturn
parsePDU version FwdCltu 7 =
    SlePduFcltuParameterReturn <$> parseFcltuGetParameterReturn version
parsePDU _ t x =
    throwError
        $  TB.run
        $  "SLE PDU not implemented yet: type "
        <> string (show t)
        <> " ASN1 Tag "
        <> decimal x
