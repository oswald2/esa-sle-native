{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.CommonConfig
    ( SleAuthType(..)
    , CommonConfig(..)
    , defaultCommonConfig
    , cfgTML
    , cfgInitiator
    , cfgResponder
    , cfgAuthorize
    , cfgPassword
    , cfgVersion
    ) where

import           RIO

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


data CommonConfig = CommonConfig
    { _cfgTML       :: !TMLConfig
    , _cfgInitiator :: !AuthorityIdentifier
    , _cfgResponder :: !AuthorityIdentifier
    , _cfgAuthorize :: !SleAuthType
    , _cfgVersion   :: !VersionNumber
    , _cfgPassword  :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON CommonConfig
instance ToJSON CommonConfig where
    toEncoding = genericToEncoding defaultOptions



defaultCommonConfig :: CommonConfig
defaultCommonConfig = CommonConfig
    { _cfgTML       = SLE.Data.TMLConfig.defaultConfig
    , _cfgInitiator = AuthorityIdentifier "EGSCC"
    , _cfgResponder = AuthorityIdentifier "PARAGONTT"
    , _cfgAuthorize = AuthNone
    , _cfgVersion   = VersionNumber 3
    , _cfgPassword  = "PASSWD"
    }
makeLenses ''CommonConfig


