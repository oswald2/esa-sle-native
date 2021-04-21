{-# LANGUAGE TemplateHaskell
  , QuasiQuotes
#-}
module SLE.ServiceElement
    ( ServiceElement
    , seCreate
    , initSLE
    ) where

import           RIO
import qualified RIO.ByteString                as B

import qualified Language.C.Inline.Cpp         as C
import qualified Language.C.Inline.Context     as CC ()
import qualified Language.C.Types              as CT ()
import           Foreign.Ptr

import SLE.Types

-- | The Service Element itself 
data SLEse

-- | Haskell Type for the Service Element
newtype ServiceElement = ServiceElement (Ptr SLEse)


C.context $ C.cppCtx <> C.cppTypePairs [
  ("SLEse", [t|SLEse|])
  ] <> C.bsCtx

C.include "<iostream>"
C.include "ISLE_Concurrent.h"
C.include "reporterImpl.H"
C.include "SLEse.H"


seCreate :: IO ServiceElement
seCreate = do
    ServiceElement <$> [C.block| SLEse* {
      SLEse* result = SLEse::instance();
      return result;
    } |]


initSLE :: ServiceElement -> Text -> Text -> SleBindRole -> Text -> IO (Either Text ())
initSLE (ServiceElement se) seConfig proxyConfig role logFile = do
    let sec = encodeUtf8 seConfig `B.snoc` 0
        pc  = encodeUtf8 proxyConfig `B.snoc` 0
        irole = sleBindRoleC role 
        logF = encodeUtf8 logFile `B.snoc` 0
    res <- [C.block| int {
        return $(SLEse* se)->initSLE($bs-ptr:sec, $bs-ptr:pc, (SLE_BindRole)$(int irole), $bs-ptr:logF);
      }
    |]
    if res < 0
        then return $ Left "Could not initialise SLE"
        else return $ Right ()
