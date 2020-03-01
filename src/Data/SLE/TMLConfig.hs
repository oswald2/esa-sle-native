module Data.SLE.TMLConfig
  ( TMLConfig(..)
  , ConnectAddr(..)
  , defaultConfig
  , writeConfigJSON
  , loadConfigJSON
  )
where

import           RIO
import qualified RIO.Text                      as T
import           Data.Aeson
import           Data.ByteString.Lazy          as B



data ConnectAddr = ConnectAddr {
  host :: Text 
  , port :: Word16
  }


data TMLConfig = TMLConfig {
  cfgHeartbeat :: Word16
  , cfgDeadFactor :: Word16
  } deriving (Show, Generic)


instance FromJSON TMLConfig
instance ToJSON TMLConfig where
  toEncoding = genericToEncoding defaultOptions


defaultConfig :: TMLConfig
defaultConfig = TMLConfig { cfgHeartbeat = 30, cfgDeadFactor = 2 }


-- | write the config in JSON format to a file. Uses the aeson for conversion to/from JSON
writeConfigJSON :: MonadIO m => TMLConfig -> FilePath -> m ()
writeConfigJSON cfg path = liftIO $ encodeFile path cfg

-- | Load a config from a file in JSON format and return it.
-- | If there is an error on parsing, return 'Left error'
loadConfigJSON :: MonadIO m => FilePath -> m (Either Text TMLConfig)
loadConfigJSON path = do
  content <- liftIO $ B.readFile path
  case eitherDecode content of
    Left  err -> return $ Left (T.pack err)
    Right cfg -> return $ Right cfg
