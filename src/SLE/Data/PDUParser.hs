module SLE.Data.PDUParser
    ( slePduParser
    ) where

import           RIO
import           RIO.State

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.PDU

import           Data.ASN1.Types

import           Control.Monad.Except

import           Text.Show.Pretty

import           Text.Builder                  as TB


slePduParser :: Parser SlePdu
slePduParser = do
    x <- get
    case x of
        (Start (Container Context x) : rest) -> do
            put rest
            parsePDU x
        _ -> throwError $ "slePduParser: cannot parse PUD: " <> fromString
            (ppShow x)


-- | Parses the SLE PDU by Tag 
-- Tags:
-- 100 : SLE Bind 
-- 101 : SLE Bind Return
parsePDU :: ASN1Tag -> Parser SlePdu
parsePDU 100 = SlePduBind <$> parseSleBind
parsePDU 101 = SlePduBindReturn <$> parseSleBindReturn
parsePDU x =
    throwError $ TB.run $ "SLE PDU not implemented yet: ASN1 Tag " <> decimal x
