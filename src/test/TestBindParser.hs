{-# LANGUAGE 
  OverloadedStrings
  , NoImplicitPrelude
#-}
module Main where

import           RIO
--import qualified RIO.Text                      as T
--import qualified Data.Text.IO                  as T
--import           Data.ASN1.Encoding
import           Data.ASN1.Types

import           Data.SLE.RAF
import           Data.SLE.Common
import           Data.SLE.ServiceInstanceID
import           Test.Hspec


bind :: [ASN1]
bind =
  [ Start (Container Context 100)
  , Other Context 0 ""
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "SLE_USER"
                         }
    )
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "55529"
                         }
    )
  , IntVal 0
  , IntVal 2
  , Start Sequence
  , Start Set
  , Start Sequence
  , OID [1, 3, 112, 4, 3, 1, 2, 52]
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "1"
                         }
    )
  , End Sequence
  , End Set
  , Start Set
  , Start Sequence
  , OID [1, 3, 112, 4, 3, 1, 2, 53]
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "VST-PASS0001"
                         }
    )
  , End Sequence
  , End Set
  , Start Set
  , Start Sequence
  , OID [1, 3, 112, 4, 3, 1, 2, 38]
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "1"
                         }
    )
  , End Sequence
  , End Set
  , Start Set
  , Start Sequence
  , OID [1, 3, 112, 4, 3, 1, 2, 22]
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "onlt1"
                         }
    )
  , End Sequence
  , End Set
  , End Sequence
  , End (Container Context 100)
  ]


attribute :: [ASN1]
attribute =
  [ Start Set
  , Start Sequence
  , OID [1, 3, 112, 4, 3, 1, 2, 53]
  , ASN1String
    (ASN1CharacterString { characterEncoding         = Visible
                         , getCharacterStringRawData = "VST-PASS0001"
                         }
    )
  , End Sequence
  , End Set
  ]



main :: IO ()
main = hspec $ do
  describe "Basic Parser Tests" $ do
    it "manyA test" $ do
      let vals   = [IntVal 1, IntVal 2, IntVal 3]
          result = parseASN1 (manyA parseIntVal) vals
      result `shouldBe` Right [1, 2, 3]

    it "Sequence test" $ do
      let vals   = [Start Sequence, IntVal 1, IntVal 2, IntVal 3, End Sequence]
          result = parseASN1 (parseSequence parseIntVal) vals
      result `shouldBe` Right [1, 2, 3]

    it "Set test" $ do
      let vals   = [Start Set, IntVal 1, IntVal 2, IntVal 3, End Set]
          result = parseASN1 (parseSet parseIntVal) vals
      result `shouldBe` Right [1, 2, 3]


  describe "Sle Bind Invocation" $ do
    it "Sle Bind Start test" $ do
      let result = parseASN1
            (parseBasicASN1 (== Start (Container Context 100)) (const ()))
            bind
      result `shouldBe` Right ()

    it "Sle Bind Start II" $ do
      let parser = do
            void $ parseBasicASN1 (== Start (Container Context 100)) (const ())
            parseCredentials
          result = parseASN1 parser bind
      result `shouldSatisfy` isRight
      result `shouldBe` Right Nothing

    it "Sle Bind Start III" $ do
      let parser = do
            void $ parseBasicASN1 (== Start (Container Context 100)) (const ())
            void parseCredentials
            parseAuthorityIdentifier
          result = parseASN1 parser bind
      result `shouldSatisfy` isRight
      result `shouldBe` Right (AuthorityIdentifier "SLE_USER")

    it "Sle Bind Start IV" $ do
      let result = parseASN1 parseServiceInstanceAttribute attribute
      result `shouldSatisfy` isRight
      result `shouldBe` Right (ServiceInstanceAttribute SPACK "VST-PASS0001")

    it "Sle Bind Invocation" $ do
      let result = parseASN1 parseSleBind bind
      result `shouldSatisfy` isRight
      result `shouldBe` Right (SleBindInvocation {
          _sleBindCredentials = Nothing
          , _sleBindInitiatorID = AuthorityIdentifier "SLE_USER"
          , _sleBindResponderPortID = PortID "55529"
          , _sleBindServiceType = RtnAllFrames
          , _sleVersionNumber = VersionNumber 2
          , _sleServiceInstanceID = ServiceInstanceIdentifier [
            ServiceInstanceAttribute SAGR "1"
            , ServiceInstanceAttribute SPACK "VST-PASS0001"
            , ServiceInstanceAttribute RSLFG "1"
            , ServiceInstanceAttribute RAF "onlt1"
            ]
        })
