{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.CommonConfig
    ( SleAuthType(..)
    , CommonConfig(..)
    , Peer(..)
    , HashToUse(..)
    , defaultCommonConfig
    , cfgTML
    , cfgPeers
    , cfgLocal
    , cfgAuthorize
    , cfgSHAType
    , cfgPassword
    , cfgVersion
    , mkPeerSet
    , isPeer
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.HashMap                   as HM

import           Control.Lens

import           Data.Aeson

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.HexBytes
import           SLE.Data.TMLConfig      hiding ( loadConfigJSON
                                                , writeConfigJSON
                                                )


data HashToUse = SHA1 | SHA256
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance FromJSON HashToUse
instance ToJSON HashToUse where
    toEncoding = genericToEncoding defaultOptions


data SleAuthType =
    AuthNone
    | AuthBind
    | AuthAll
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance FromJSON SleAuthType
instance ToJSON SleAuthType where
    toEncoding = genericToEncoding defaultOptions

data Peer = Peer
    { cfgPeerAuthorityID :: !AuthorityIdentifier
    , cfgPeerPassword    :: !HexBytes
    }
    deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Peer
instance ToJSON Peer where
    toEncoding = genericToEncoding defaultOptions


data CommonConfig = CommonConfig
    { _cfgTML       :: !TMLConfig
    , _cfgPeers     :: [Peer]
    , _cfgLocal     :: !AuthorityIdentifier
    , _cfgAuthorize :: !SleAuthType
    , _cfgSHAType   :: !HashToUse
    , _cfgVersion   :: !SleVersion
    , _cfgPassword  :: !HexBytes
    }
    deriving (Show, Read, Generic)

instance FromJSON CommonConfig
instance ToJSON CommonConfig where
    toEncoding = genericToEncoding defaultOptions


mkPeerSet :: CommonConfig -> HashMap AuthorityIdentifier Peer
mkPeerSet cfg =
    HM.fromList $ map (\p -> (cfgPeerAuthorityID p, p)) (_cfgPeers cfg)

isPeer :: HashMap AuthorityIdentifier Peer -> AuthorityIdentifier -> Bool
isPeer hm auid = HM.member auid hm

defaultPeers :: [Peer]
defaultPeers =
    [ Peer (AuthorityIdentifier "EGSCC") (bsToHex "12345678")
    , Peer
        (AuthorityIdentifier "SLETT")
        (bsToHex (B.pack [0xaa, 0xbb, 0xcc, 0xdd, 0xaa, 0xbb, 0xcc, 0xdd]))
    ]

defaultCommonConfig :: CommonConfig
defaultCommonConfig = CommonConfig
    { _cfgTML       = SLE.Data.TMLConfig.defaultConfig
    , _cfgPeers     = defaultPeers
    , _cfgLocal     = AuthorityIdentifier "PARAGONTT"
    , _cfgAuthorize = AuthNone
    , _cfgSHAType   = SHA1
    , _cfgVersion   = SLE3
    , _cfgPassword  = bsToHex
                          (B.pack
                              [ 0x00
                              , 0x01
                              , 0x02
                              , 0x03
                              , 0x04
                              , 0x05
                              , 0x06
                              , 0x07
                              , 0x08
                              , 0x09
                              , 0x0a
                              , 0x0b
                              , 0x0c
                              , 0x0d
                              , 0x0e
                              , 0x0f
                              ]
                          )
    }
makeLenses ''CommonConfig


