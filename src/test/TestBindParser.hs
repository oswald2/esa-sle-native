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

bindEnc :: ByteString
bindEnc =
    "\191dj\128\NUL\SUB\bSLE_USER\SUB\ENQ55529\STX\SOH\NUL\STX\SOH\STX0O1\SO0\f\ACK\a+p\EOT\ETX\SOH\STX4\SUB\SOH11\EM0\ETB\ACK\a+p\EOT\ETX\SOH\STX5\SUB\fVST-PASS00011\SO0\f\ACK\a+p\EOT\ETX\SOH\STX&\SUB\SOH11\DC20\DLE\ACK\a+p\EOT\ETX\SOH\STX\SYN\SUB\ENQonlt1"

bindEncW8 :: ByteString
bindEncW8 = B.pack
    [ 191
    , 100
    , 106
    , 128
    , 0
    , 26
    , 8
    , 83
    , 76
    , 69
    , 95
    , 85
    , 83
    , 69
    , 82
    , 26
    , 5
    , 53
    , 53
    , 53
    , 50
    , 57
    , 2
    , 1
    , 0
    , 2
    , 1
    , 2
    , 48
    , 79
    , 49
    , 14
    , 48
    , 12
    , 6
    , 7
    , 43
    , 112
    , 4
    , 3
    , 1
    , 2
    , 52
    , 26
    , 1
    , 49
    , 49
    , 25
    , 48
    , 23
    , 6
    , 7
    , 43
    , 112
    , 4
    , 3
    , 1
    , 2
    , 53
    , 26
    , 12
    , 86
    , 83
    , 84
    , 45
    , 80
    , 65
    , 83
    , 83
    , 48
    , 48
    , 48
    , 49
    , 49
    , 14
    , 48
    , 12
    , 6
    , 7
    , 43
    , 112
    , 4
    , 3
    , 1
    , 2
    , 38
    , 26
    , 1
    , 49
    , 49
    , 18
    , 48
    , 16
    , 6
    , 7
    , 43
    , 112
    , 4
    , 3
    , 1
    , 2
    , 22
    , 26
    , 5
    , 111
    , 110
    , 108
    , 116
    , 49
    ]

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


bindS :: [ASN1]
bindS =
    [ {-Start (Container Context 100)
    , -}
      Other Context 0 ""
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

sleBindReturn :: [ASN1]
sleBindReturn =
    [ Start (Container Context 101)
    , Other Context 0 ""
    , ASN1String
        (ASN1CharacterString { characterEncoding         = Visible
                             , getCharacterStringRawData = "SLE_PROVIDER"
                             }
        )
    , Other Context 0 "\STX"
    , End (Container Context 101)
    ]

startWithTimes :: [ASN1]
startWithTimes =
    [ {- Start (Container Context 0)
    , -}
      Other Context 0 ""
    , IntVal 1
    , Start (Container Context 1)
    , Other Context 0 "\\\188\EOT\239m\128\NUL\NUL"
    , End (Container Context 1)
    , Start (Container Context 1)
    , Other Context 0 "^(\EOT\239m\128\NUL\NUL"
    , End (Container Context 1)
    , IntVal 2
    , End (Container Context 0)
    ]

start :: [ASN1]
start =
    [ {- Start (Container Context 0)
    , -}
      Other Context 0 ""
    , IntVal 1
    , Other Context 0 ""
    , Other Context 0 ""
    , IntVal 2
    , End (Container Context 0)
    ]


sleBind :: SleBindInvocation
sleBind = SleBindInvocation
    { _sleBindCredentials     = Nothing
    , _sleBindInitiatorID     = AuthorityIdentifier "SLE_USER"
    , _sleBindResponderPortID = PortID "55529"
    , _sleBindServiceType     = RtnAllFrames
    , _sleVersionNumber       = VersionNumber 2
    , _sleServiceInstanceID   = ServiceInstanceIdentifier
                                    [ ServiceInstanceAttribute SAGR "1"
                                    , ServiceInstanceAttribute SPACK
                                                               "VST-PASS0001"
                                    , ServiceInstanceAttribute RSLFG "1"
                                    , ServiceInstanceAttribute RAF   "onlt1"
                                    ]
    }

fcltuStartReturn =
    [ Start (Container Context 1)
    , Other Context 0 ""
    , IntVal 1
    , Start (Container Context 0)
    , Other Context 0 "\\\215\ETXf0z\NUL\NUL"
    , Other Context 0 ""
    , End (Container Context 0)
    , End (Container Context 1)
    ]


fcltuAsync :: [ASN1]
fcltuAsync =
    [ {- Start (Container Context 12)
    , -}
      Other Context 0 ""
    , Other Context 0 ""
    , Start (Container Context 1)
    , IntVal 1
    , Start (Container Context 1)
    , Other Context 0 "\\\219\ETX\191\fB\STXR"
    , End (Container Context 1)
    , IntVal 0
    , End (Container Context 1)
    , Start (Container Context 1)
    , IntVal 1
    , Other Context 0 "\\\219\ETX\191\fB\ETX\t"
    , End (Container Context 1)
    , IntVal 0
    , IntVal 3
    , End (Container Context 12)
    ]

cltuID :: [ASN1]
cltuID = [IntVal 1]

condTime :: [ASN1]
condTime =
    [ Start (Container Context 1)
    , Other Context 0 "\\\219\ETX\191\fB\STXR"
    , End (Container Context 1)
    ]

fwdDuStatus :: [ASN1]
fwdDuStatus = [IntVal 0]


cltuLastProcessed :: [ASN1]
cltuLastProcessed =
    [ Start (Container Context 1)
    , IntVal 1
    , Start (Container Context 1)
    , Other Context 0 "\\\219\ETX\191\fB\STXR"
    , End (Container Context 1)
    , IntVal 0
    , End (Container Context 1)
    ]

cltuLastOK :: [ASN1]
cltuLastOK =
    [ Start (Container Context 1)
    , IntVal 1
    , Other Context 0 "\\\219\ETX\191\fB\ETX\t"
    , End (Container Context 1)
    ]

main :: IO ()
main = hspec $ do
    -- describe "Basic Parser Tests" $ do
    --     it "manyA test" $ do
    --         let vals   = [IntVal 1, IntVal 2, IntVal 3]
    --             result = parseASN1 (manyA parseIntVal) vals
    --         result `shouldBe` Right [1, 2, 3]

        -- it "Sequence test" $ do
        --     let vals =
        --             [Start Sequence, IntVal 1, IntVal 2, IntVal 3, End Sequence]
        --         result = parseASN1 (parseSequence parseIntVal) vals
        --     result `shouldBe` Right [1, 2, 3]

        -- it "Set test" $ do
        --     let vals   = [Start Set, IntVal 1, IntVal 2, IntVal 3, End Set]
        --         result = parseASN1 (parseSet parseIntVal) vals
        --     result `shouldBe` Right [1, 2, 3]

    describe "FCLTU Async Parser" $ do
        it "CltuID Parser" $ do
            let result = parseASN1 parseCltuIdentification cltuID
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight

    describe "FCLTU Async Parser" $ do
        it "Conditional Time Parser" $ do
            let result = parseASN1 parseConditionalTime condTime
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight

    describe "FCLTU Async Parser" $ do
        it "Forward DU Status Parser" $ do
            let result = parseASN1 parseForwardDuStatus fwdDuStatus
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight

    describe "FCLTU Async Parser" $ do
        it "CltuLastProcessed Parser" $ do
            let result = parseASN1 parseCltuLastProcessed cltuLastProcessed
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight


    describe "FCLTU Async Parser" $ do
        it "CltuLastok Parser" $ do
            let result = parseASN1 parseCltuLastOk cltuLastOK
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight


    describe "FCLTU Async Parser" $ do
        it "Async" $ do
            let result = parseASN1 parseFcltuAsyncStatus fcltuAsync
            T.putStrLn $ "Result: " <> T.pack (show result)
            result `shouldSatisfy` isRight


    -- describe "Start Parser" $ do
    --     it "Sle Start with Times" $ do
    --         let result = parseASN1 parseRafStart startWithTimes
    --         result `shouldSatisfy` isRight

    --     it "Sle Start without Times" $ do
    --         let result = parseASN1 parseRafStart start
    --         result `shouldSatisfy` isRight

    -- describe "Sle Bind Invocation" $ do
    --     it "Sle Bind Start test" $ do
    --         let result = parseASN1
    --                 (parseBasicASN1 (== Start (Container Context 100))
    --                                 (const ())
    --                 )
    --                 bind
    --         result `shouldBe` Right ()

    --     it "Sle Bind Start II" $ do
    --         let parser = do
    --                 void $ parseBasicASN1 (== Start (Container Context 100))
    --                                       (const ())
    --                 parseCredentials
    --             result = parseASN1 parser bind
    --         result `shouldSatisfy` isRight
    --         result `shouldBe` Right Nothing

    --     it "Sle Bind Start III" $ do
    --         let parser = do
    --                 void $ parseBasicASN1 (== Start (Container Context 100))
    --                                       (const ())
    --                 void parseCredentials
    --                 parseAuthorityIdentifier
    --             result = parseASN1 parser bind
    --         result `shouldSatisfy` isRight
    --         result `shouldBe` Right (AuthorityIdentifier "SLE_USER")

    --     it "Sle Bind Start IV" $ do
    --         let result = parseASN1 parseServiceInstanceAttribute attribute
    --         result `shouldSatisfy` isRight
    --         result `shouldBe` Right
    --             (ServiceInstanceAttribute SPACK "VST-PASS0001")

    --     it "Sle Bind Invocation" $ do
    --         let result = parseASN1 parseSleBind bindS
    --         result `shouldSatisfy` isRight
    --         result `shouldBe` Right sleBind

    --     it "Sle encode/decode test" $ do
    --         let bind   = sleBindInvocation sleBind
    --             bind'  = encodeASN1 DER bind
    --             bind'' = decodeASN1 DER bind'

    --         bind'' `shouldSatisfy` isRight
    --         case bind'' of
    --             Left  _err    -> return ()
    --             Right asnBind -> do
    --                 let result = parseASN1 parseSleBind (drop 1 asnBind)
    --                 result `shouldSatisfy` isRight
    --                 case result of
    --                     Left  _err -> return ()
    --                     Right msg  -> do
    --                         msg `shouldBe` sleBind

