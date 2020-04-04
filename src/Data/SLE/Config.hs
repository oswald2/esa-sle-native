{-# LANGUAGE 
  TemplateHaskell
#-}
module Data.SLE.Config
  ( Config(..)
  , SleAuthType(..)
  , Data.SLE.Config.defaultConfig
  , cfgTML
  , cfgInitiator
  , cfgAuthorize
  , cfgUserName
  , cfgPassword

  )
where


import           RIO
import           Control.Lens
import           Data.Aeson

import           Data.SLE.TMLConfig
import           Data.SLE.Bind


data SleAuthType =
    AuthNone
    | AuthBind
    | AuthAll
    deriving (Eq, Ord, Enum, Show, Generic)


instance FromJSON SleAuthType 
instance ToJSON SleAuthType where 
  toEncoding = genericToEncoding defaultOptions

data Config = Config {
  _cfgTML :: !TMLConfig
  , _cfgInitiator :: !AuthorityIdentifier
  , _cfgAuthorize :: !SleAuthType
  , _cfgPassword :: !Text
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions




defaultConfig :: Config
defaultConfig = Config { _cfgTML       = Data.SLE.TMLConfig.defaultConfig
                       , _cfgInitiator = AuthorityIdentifier "SLE_USER"
                       , _cfgAuthorize = AuthNone
                       , _cfgPassword = "PASSWD"
                       }


cfgUserName :: Getter Config Text 
cfgUserName = Control.Lens.to (unAuthorityID . _cfgInitiator) 

makeLenses ''Config
