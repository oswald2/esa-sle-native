{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.UserConfig
    ( UserConfig(..)
    , defaultUserConfig
    , defaultUserConfigFileName
    -- , cfgUserName
    , cfgCommon
    , writeConfigJSON
    , loadConfigJSON
    , configPretty
    ) where


import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )

-- import           SLE.Data.Bind
import           SLE.Data.CommonConfig



newtype UserConfig = UserConfig
    { _cfgCommon :: CommonConfig
    }
    deriving (Show, Generic)

instance FromJSON UserConfig
instance ToJSON UserConfig where
    toEncoding = genericToEncoding defaultOptions


defaultUserConfigFileName :: FilePath
defaultUserConfigFileName = "DefaultUserConfig.json"

defaultUserConfig :: UserConfig
defaultUserConfig = UserConfig defaultCommonConfig


-- cfgUserName :: Getter UserConfig Text
-- cfgUserName = Control.Lens.to (unAuthorityID . _cfgInitiator . _cfgCommon)

makeLenses ''UserConfig


configPretty :: UserConfig -> Text
configPretty cfg = case (decodeUtf8' . B.toStrict . encodePretty) cfg of
    Left  err -> "Error decoding Config in UTF8: " <> T.pack (show err)
    Right val -> val


writeConfigJSON :: MonadIO m => UserConfig -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ B.writeFile path (encodePretty cfg)

-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text UserConfig)
loadConfigJSON path = do
    content <- liftIO $ B.readFile path
    case eitherDecode content of
        Left  err -> return $ Left (T.pack err)
        Right cfg -> return $ Right cfg
