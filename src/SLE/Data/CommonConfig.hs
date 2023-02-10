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
import qualified RIO.HashMap                   as HM

import           Control.Lens

import           Data.Aeson

import           SLE.Data.Bind
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
    , _cfgVersion   :: !VersionNumber
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
    , Peer (AuthorityIdentifier "SLETT") (bsToHex "aabbccddaabbccdd")
    ]

defaultCommonConfig :: CommonConfig
defaultCommonConfig = CommonConfig
    { _cfgTML       = SLE.Data.TMLConfig.defaultConfig
    , _cfgPeers     = defaultPeers
    , _cfgLocal     = AuthorityIdentifier "PARAGONTT"
    , _cfgAuthorize = AuthNone
    , _cfgSHAType   = SHA1
    , _cfgVersion   = VersionNumber 3
    , _cfgPassword  = bsToHex "PASSWD"
    }
makeLenses ''CommonConfig


