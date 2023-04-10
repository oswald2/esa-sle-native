{-|
Module      : SLE.Data.TMLConfig
Description : Provides the configuration values for the TML (Transfer Message Layer)
Copyright   : (c) Michael Oswald, 2022
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides the configuration values for the TML layer itself.
-}
module SLE.Data.TMLConfig
  ( TMLConfig(..)
  , ConnectAddr(..)
  , defaultConfig
  , writeConfigJSON
  , loadConfigJSON
  , mkServerAddr
  )
where

import           RIO
import qualified RIO.Text                      as T
import           Data.Aeson
import           Data.ByteString.Lazy          as B

import           Network.Socket                 ( PortNumber )

-- | General structure for a connection with a hostname and a port
data ConnectAddr = ConnectAddr {
  host :: Text
  , port :: PortNumber
  }

-- | Convenienca function to create a ConnectAddr structure. Takes the specified port number and uses host "*" for 
-- the accept host
mkServerAddr :: PortNumber -> ConnectAddr
mkServerAddr = ConnectAddr "*"


-- | The configuration data of the TML layer itself
data TMLConfig = TMLConfig {
  -- | The heartbeat interval in seconds (default: 30 seconds)
  cfgHeartbeat :: Word16
  -- | The dead factor. If the hearbeat is missed this number of times, the connection is aborted (default: 2)
  , cfgDeadFactor :: Word16
  -- | The time (in seconds) a server is allowed to initialise (default: 30 seconds)
  , cfgServerInitTime :: Word16
  -- | The minimum allowed heartbeat in seconds (default: 2 seconds)
  , cfgMinHeartBeat :: Word16
  -- | The maximum allowed heartbeat in seconds (default: 3600 seconds)
  , cfgMaxHeartBeat :: Word16
  -- | The minimum allowed dead factor (default: 2)
  , cfgMinDeadFactor :: Word16
  -- | The maximum allowed dead factor (default: 60)
  , cfgMaxDeadFactor :: Word16
  } deriving (Show, Read, Generic)


instance FromJSON TMLConfig
instance ToJSON TMLConfig where
  toEncoding = genericToEncoding defaultOptions

-- | The default configuration for the TML layer
defaultConfig :: TMLConfig
defaultConfig = TMLConfig { cfgHeartbeat      = 30
                          , cfgDeadFactor     = 2
                          , cfgServerInitTime = 30
                          , cfgMinHeartBeat   = 3
                          , cfgMaxHeartBeat   = 3600
                          , cfgMinDeadFactor  = 2
                          , cfgMaxDeadFactor  = 60
                          }


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
