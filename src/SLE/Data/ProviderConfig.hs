{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.ProviderConfig
    ( ProviderConfig(..)
    , RAFConfig(..)
    , RAFIdx(..)
    , FCLTUConfig(..)
    , FCLTUIdx(..)
    , SleAuthType(..)
    , configPretty
    , defaultProviderConfigFileName
    , defaultProviderConfig
    , writeConfigJSON
    , loadConfigJSON
    , cfgCommon
    , cfgRAFs
    , cfgFCLTUs
    , cfgRAFSII
    , cfgRAFPort
    , cfgRAFPortID
    , cfgRAFAntennaID
    , cfgRAFBufferSize
    , cfgRAFLatency
    , cfgFCLTUSII
    , cfgFCLTUPort
    , cfgFCLTUPortID
    ) where


import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )

-- import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig


data RAFConfig = RAFConfig
    { _cfgRAFSII        :: !SII
    , _cfgRAFPort       :: !Word16
    , _cfgRAFPortID     :: !Text
    , _cfgRAFAntennaID  :: !AntennaID
    , _cfgRAFBufferSize :: !Word32
    , _cfgRAFLatency    :: !Int
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultRAFConfig :: RAFConfig
defaultRAFConfig = RAFConfig
    { _cfgRAFSII        = SII "sagr=3.spack=facility-PASS1.rsl-fg=1.raf=onlc1"
    , _cfgRAFPort       = 5008
    , _cfgRAFPortID     = "TMPORT"
    , _cfgRAFAntennaID  = LocalForm "PARAGONTT"
    , _cfgRAFBufferSize = 100
    , _cfgRAFLatency    = 1_000_000
    }

data FCLTUConfig = FCLTUConfig
    { _cfgFCLTUSII    :: !SII
    , _cfgFCLTUPort   :: !Word16
    , _cfgFCLTUPortID :: !Text
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultFCLTUConfig :: FCLTUConfig
defaultFCLTUConfig = FCLTUConfig
    { _cfgFCLTUSII    = SII "sagr=3.spack=facility-PASS1.fsl-fg=1.cltu=cltu1"
    , _cfgFCLTUPort   = 5009
    , _cfgFCLTUPortID = "TCPORT"
    }


data ProviderConfig = ProviderConfig
    { _cfgCommon :: !CommonConfig
    , _cfgRAFs   :: !(Vector RAFConfig)
    , _cfgFCLTUs :: !(Vector FCLTUConfig)
    }
    deriving (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)



defaultProviderConfigFileName :: FilePath
defaultProviderConfigFileName = "DefaultProviderConfig.json"

defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig
    { _cfgCommon = defaultCommonConfig
    , _cfgRAFs   = V.singleton defaultRAFConfig
    , _cfgFCLTUs = V.singleton defaultFCLTUConfig
    }


makeLenses ''ProviderConfig
makeLenses ''RAFConfig
makeLenses ''FCLTUConfig

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
