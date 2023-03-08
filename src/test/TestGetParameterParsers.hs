{-# LANGUAGE 
  OverloadedStrings
  , NoImplicitPrelude
#-}
module Main where

import           RIO
import qualified RIO.ByteString                as B

import           Data.ASN1.BinaryEncoding
--import qualified RIO.Text                      as T
--import qualified Data.Text.IO                  as T
import           Data.ASN1.Encoding
import           Data.ASN1.Types


import qualified Data.Text.IO                  as T

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.FCLTUOps
import           SLE.Data.RAF
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID

import qualified RIO.Text                      as T
import           Test.Hspec


getParameterAcquSeqReturn :: [ASN1]
getParameterAcquSeqReturn =
    [ {- Start (Container Context 7)
    , -}
      Other Context 0 ""
    , IntVal 1
    , Start (Container Context 0)
    , Start (Container Context 12)
    , IntVal 201
    , IntVal 65535
    , End (Container Context 12)
    , End (Container Context 0)
    , End (Container Context 7)
    ]

getParameterClcwGlobalVCID :: [ASN1]
getParameterClcwGlobalVCID =
    [ {-Start (Container Context 7)
    , -}
      Other Context 0 ""
    , IntVal 1
    , Start (Container Context 0)
    , Start (Container Context 13)
    , IntVal 202
    , Start Sequence
    , IntVal 16
    , IntVal 1
    , Other Context 1 "\SOH"
    , End Sequence
    , End (Container Context 13)
    , End (Container Context 0)
    , End (Container Context 7)
    ]

getParameterClcwGlobalMC :: [ASN1]
getParameterClcwGlobalMC =
    [ {-Start (Container Context 7)
    , -}
      Other Context 0 ""
    , IntVal 1
    , Start (Container Context 0)
    , Start (Container Context 13)
    , IntVal 202
    , Start Sequence
    , IntVal 16
    , IntVal 1
    , Other Context 0 ""
    , End Sequence
    , End (Container Context 13)
    , End (Container Context 0)
    , End (Container Context 7)
    ]


main :: IO ()
main = hspec $ do
    describe "FCLTU Get Parameter Parsers" $ do
        it "Acquisition Sequence Length" $ do
            let
                result = parseASN1 parseFcltuGetParameterReturn
                                   getParameterAcquSeqReturn
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight

        it "CLCW Global GVCID VC" $ do
            let
                result = parseASN1 parseFcltuGetParameterReturn
                                   getParameterClcwGlobalVCID
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight

        it "CLCW Global GVCID MC" $ do
            let
                result = parseASN1 parseFcltuGetParameterReturn
                                   getParameterClcwGlobalMC
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight

