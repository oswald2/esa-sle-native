{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.ProviderConfig
    ( ProviderConfig(..)
    , SleAuthType(..)
    , configPretty
    , defaultProviderConfigFileName
    , defaultProviderConfig
    , writeConfigJSON
    , loadConfigJSON
    , cfgCommon
    , SLE.Data.ProviderConfig.cfgUserName
    ) where


import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )

import           SLE.Data.Bind

import           SLE.Data.CommonConfig


newtype ProviderConfig = ProviderConfig
    { _cfgCommon :: CommonConfig
    }
    deriving (Show, Generic)

instance FromJSON ProviderConfig
instance ToJSON ProviderConfig where
    toEncoding = genericToEncoding defaultOptions


defaultProviderConfigFileName :: FilePath
defaultProviderConfigFileName = "DefaultProviderConfig.json"

defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig defaultCommonConfig


cfgUserName :: Getter ProviderConfig Text
cfgUserName = Control.Lens.to (unAuthorityID . _cfgInitiator . _cfgCommon)

makeLenses ''ProviderConfig


configPretty :: ProviderConfig -> Text
configPretty cfg = case (decodeUtf8' . B.toStrict . encodePretty) cfg of
    Left  err -> "Error decoding Config in UTF8: " <> T.pack (show err)
    Right val -> val


writeConfigJSON :: MonadIO m => ProviderConfig -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ B.writeFile path (encodePretty cfg)

-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text ProviderConfig)
loadConfigJSON path = do
    content <- liftIO $ B.readFile path
    case eitherDecode content of
        Left  err -> return $ Left (T.pack err)
        Right cfg -> return $ Right cfg
