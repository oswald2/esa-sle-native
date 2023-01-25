module SLE.Data.PDUParser
    ( slePduParser
    ) where

import           RIO
import           RIO.State

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.PDU
import           SLE.Data.RAFOps

import           Data.ASN1.Types

import           Control.Monad.Except

import           Text.Show.Pretty

import           Text.Builder                  as TB


slePduParser :: Parser SlePdu
slePduParser = do
    x <- get
    case x of
        (Start (Container Context c) : rest) -> do
            put rest
            parsePDU c
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
-- 6   : RAF Get Parameter Invocation 
-- 7   : RAF Get Parameter Return 
-- 8   : RAF Transfer Buffer / FCLTU Throw Event
-- 9   : RAF Status Report Invocation 
-- 10  : FCLTU Transfer Data Invocation
-- 104 : RAF Peer Abort
parsePDU :: ASN1Tag -> Parser SlePdu
parsePDU 100 = SlePduBind <$> parseSleBind
parsePDU 101 = SlePduBindReturn <$> parseSleBindReturn
parsePDU 102 = SlePduUnbind <$> parseSleUnbind
parsePDU 103 = SlePduUnbindReturn <$> parseSleUnbindReturn
parsePDU 104 = SlePduPeerAbort <$> parseSlePeerAbort
parsePDU 0   = SlePduRafStart <$> parseRafStart
parsePDU 1   = SlePduRafStartReturn <$> parseRafStartReturn
parsePDU 2   = SlePduStop <$> parseStopInvocation
parsePDU 3   = SlePduAck <$> parseSleAcknowledgement
parsePDU 8   = SlePduRafTransferBuffer <$> parseTransferBuffer
parsePDU x =
    throwError $ TB.run $ "SLE PDU not implemented yet: ASN1 Tag " <> decimal x
