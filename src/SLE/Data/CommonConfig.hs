{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.CommonConfig
    ( SleAuthType(..)
    , CommonConfig(..)
    , Peer(..)
    , defaultCommonConfig
    , cfgTML
    , cfgPeers
    , cfgLocal
    , cfgAuthorize
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
import           SLE.Data.TMLConfig      hiding ( loadConfigJSON
                                                , writeConfigJSON
                                                )



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
    , cfgPeerPassword    :: !Password
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
    , _cfgVersion   :: !VersionNumber
    , _cfgPassword  :: !Text
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


defaultCommonConfig :: CommonConfig
defaultCommonConfig = CommonConfig
    { _cfgTML       = SLE.Data.TMLConfig.defaultConfig
    , _cfgPeers = [Peer (AuthorityIdentifier "EGSCC") (Password "0x12345678")]
    , _cfgLocal     = AuthorityIdentifier "PARAGONTT"
    , _cfgAuthorize = AuthNone
    , _cfgVersion   = VersionNumber 3
    , _cfgPassword  = "PASSWD"
    }
makeLenses ''CommonConfig


