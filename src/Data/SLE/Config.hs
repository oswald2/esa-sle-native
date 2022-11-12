{-# LANGUAGE 
  TemplateHaskell
#-}
module Data.SLE.Config
    ( Config(..)
    , SleAuthType(..)
    , Data.SLE.Config.defaultConfig
    , defaultConfigFileName
    , cfgTML
    , cfgInitiator
    , cfgAuthorize
    , cfgUserName
    , cfgPassword
    , writeConfigJSON
    , loadConfigJSON
    ) where


import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )

import           Data.SLE.Bind
import           Data.SLE.TMLConfig      hiding ( loadConfigJSON
                                                , writeConfigJSON
                                                )


data SleAuthType =
    AuthNone
    | AuthBind
    | AuthAll
    deriving (Eq, Ord, Enum, Show, Generic)


instance FromJSON SleAuthType
instance ToJSON SleAuthType where
    toEncoding = genericToEncoding defaultOptions

data Config = Config
    { _cfgTML       :: !TMLConfig
    , _cfgInitiator :: !AuthorityIdentifier
    , _cfgAuthorize :: !SleAuthType
    , _cfgPassword  :: !Text
    }
    deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions


defaultConfigFileName :: FilePath
defaultConfigFileName = "DefaultConfig.json"

defaultConfig :: Config
defaultConfig = Config { _cfgTML       = Data.SLE.TMLConfig.defaultConfig
                       , _cfgInitiator = AuthorityIdentifier "SLE_USER"
                       , _cfgAuthorize = AuthNone
                       , _cfgPassword  = "PASSWD"
                       }


cfgUserName :: Getter Config Text
cfgUserName = Control.Lens.to (unAuthorityID . _cfgInitiator)

makeLenses ''Config


configPretty :: Config -> Text
configPretty cfg = case (decodeUtf8' . B.toStrict . encodePretty) cfg of
    Left  err -> "Error decoding Config in UTF8: " <> T.pack (show err)
    Right val -> val


writeConfigJSON :: MonadIO m => Config -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ B.writeFile path (encodePretty cfg)

-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text Config)
loadConfigJSON path = do
    content <- liftIO $ B.readFile path
    case eitherDecode content of
        Left  err -> return $ Left (T.pack err)
        Right cfg -> return $ Right cfg
