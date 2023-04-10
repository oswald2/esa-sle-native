{-|
Module      : SLE.Data.ProviderConfig
Description : Provides the configuration for SLE providers
Copyright   : (c) Michael Oswald, 2022
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides the data types and values for the configuration of SLE providers. This is for example
actively used in ParagonTT.
-}
{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.ProviderConfig
    ( ProviderConfig(..)
    , RAFConfig(..)
    , RAFIdx(..)
    , RCFConfig(..)
    , RCFIdx(..)
    , FCLTUConfig(..)
    , FCLTUIdx(..)
    , SleAuthType(..)
    , PusMasterChannel(..)
    , PusVirtualChannel(..)
    , RCFGvcid(..)
    , configPretty
    , defaultProviderConfigFileName
    , defaultProviderConfig
    , writeConfigJSON
    , loadConfigJSON
    , cfgCommon
    , cfgRAFs
    , cfgRCFs
    , cfgFCLTUs
    , cfgRAFSII
    , cfgRAFPort
    , cfgRAFPortID
    , cfgRAFAntennaID
    , cfgRAFBufferSize
    , cfgRAFLatency
    , cfgRCFSII
    , cfgRCFPort
    , cfgRCFPortID
    , cfgRCFAntennaID
    , cfgRCFBufferSize
    , cfgRCFLatency
    , cfgRAFDeliveryMode
    , cfgRCFGVCIDs
    , cfgFCLTUSII
    , cfgFCLTUPort
    , cfgFCLTUPortID
    , cfgFCLTUAssociatedTMPort
    ) where


import           RIO
import qualified RIO.ByteString.Lazy           as B
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.List.NonEmpty            as N

import           SLE.Data.Common
import           SLE.Data.CommonConfig

-- | The config for Return All Frames (RAF) services. Lenses for the fields are created automatically.
data RAFConfig = RAFConfig
    {
    -- | The Service Instance ID of the RAF service (default: @SII "sagr=3.spack=facility-PASS1.rsl-fg=1.raf=onlc1"@)
      _cfgRAFSII          :: !SII
    -- | The TCP/IP port where the SLE provider should listen on for this RAF service
    , _cfgRAFPort         :: !Word16
    -- | The SLE Port ID for this RAF service (a text value)
    , _cfgRAFPortID       :: !Text
    -- | The Antenna ID which will be used for this provider 
    , _cfgRAFAntennaID    :: !AntennaID
    -- | The RAF buffer size. The TM transfer frames will be collected in this buffer either until the 
    -- buffer is full or the latency has been reached. Then the whole buffer will be sent.
    , _cfgRAFBufferSize   :: !Word16
    -- | The latency. See above for interaction with the buffer size
    , _cfgRAFLatency      :: !Int
    -- | The delivery mode of this RAF service. Currently, this setting has not much effect, the provider acts 
    -- as if online-complete is always set.
    , _cfgRAFDeliveryMode :: !DeliveryMode
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The default RAF configuration, used for writing the default config to a file
defaultRAFConfig :: RAFConfig
defaultRAFConfig = RAFConfig
    { _cfgRAFSII          = SII "sagr=3.spack=facility-PASS1.rsl-fg=1.raf=onlc1"
    , _cfgRAFPort         = 5100
    , _cfgRAFPortID       = "TMPORT"
    , _cfgRAFAntennaID    = LocalForm "PARAGONTT"
    , _cfgRAFBufferSize   = 100
    , _cfgRAFLatency      = 1_000_000
    , _cfgRAFDeliveryMode = DelRtnCompleteOnline
    }

-- | The settings for a Master Channel. A master channel is defined of a spacecraft ID and a version
data PusMasterChannel = PusMasterChannel
    {
    -- | The spacecraft ID for this master channel 
      mcSCID    :: !Word64
    -- | The version for this master channel
    , mcVersion :: !Word64
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON PusMasterChannel
instance ToJSON PusMasterChannel where
    toEncoding = genericToEncoding defaultOptions

-- | The settings for a Virtual Channel. Additionally to the information as for a master channel, a virtual
-- channel ID can be specified.
data PusVirtualChannel = PusVirtualChannel
    {
    -- | The spacecraft ID for this virtual channel 
      vcSCID    :: !Word64
    -- | The version number for this virtual channel
    , vcVersion :: !Word64
    -- | The virtual channel ID for this virtual channel
    , vcVC      :: !Word64
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON PusVirtualChannel
instance ToJSON PusVirtualChannel where
    toEncoding = genericToEncoding defaultOptions

-- | A TCF (Return Channel Frames) Global Virtual Channel ID. Can be either a Master Channel or a Virtual Channel
data RCFGvcid =
    PusRcfMC !PusMasterChannel
    | PusRcfVC !PusVirtualChannel
    deriving (Eq, Show, Read, Generic)

instance FromJSON RCFGvcid
instance ToJSON RCFGvcid where
    toEncoding = genericToEncoding defaultOptions

-- | The config for the Return Channel Frame (RCF) services. Basically identical to the RAF config, but allows 
-- additionally to specify the applicable master- or virtual-channels for this service.
--
-- Note: The RCF service is not implemented yet!
data RCFConfig = RCFConfig
    {
    -- | The Service Instance ID of the RAF service (default: @SII "sagr=3.spack=facility-PASS1.rsl-fg=1.rcf=onlc1"@)
      _cfgRCFSII        :: !SII
    -- | The TCP/IP port where the SLE provider should listen on for this RCF service
    , _cfgRCFPort       :: !Word16
    -- | The SLE Port ID for this RCF service (a text value)
    , _cfgRCFPortID     :: !Text
    -- | The Antenna ID which will be used for this provider 
    , _cfgRCFAntennaID  :: !AntennaID
    -- | The RCF buffer size. The TM transfer frames will be collected in this buffer either until the 
    -- buffer is full or the latency has been reached. Then the whole buffer will be sent.
    , _cfgRCFBufferSize :: !Word32
    -- | The latency. See above for interaction with the buffer size
    , _cfgRCFLatency    :: !Int
    -- | A non-empty list of channels to be handled by this service (master- or virtual-channel)
    , _cfgRCFGVCIDs     :: NonEmpty RCFGvcid
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | The default RCF configuration to be written to a config file
defaultRCFConfig :: RCFConfig
defaultRCFConfig = RCFConfig
    { _cfgRCFSII        = SII "sagr=3.spack=facility-PASS1.rsl-fg=1.rcf=onlc1"
    , _cfgRCFPort       = 5101
    , _cfgRCFPortID     = "TMPORT1"
    , _cfgRCFAntennaID  = LocalForm "PARAGONTT"
    , _cfgRCFBufferSize = 100
    , _cfgRCFLatency    = 1_000_000
    , _cfgRCFGVCIDs     = N.fromList
                              [ PusRcfVC (PusVirtualChannel 3 0 0)
                              , PusRcfVC (PusVirtualChannel 3 0 2)
                              , PusRcfMC (PusMasterChannel 4 0)
                              ]
    }


-- | The config for the Forward Command Link Transmission Unit (FCLTU) service
data FCLTUConfig = FCLTUConfig
    {
    -- | The service instance ID for this FCLTU service (default: @SII "sagr=3.spack=facility-PASS1.fsl-fg=1.cltu=cltu1"@)
      _cfgFCLTUSII              :: !SII
    -- | The TCP/IP port the provider should listen on for this FCLTU service
    , _cfgFCLTUPort             :: !Word16
    -- | The SLE Port ID for this FCLTU service
    , _cfgFCLTUPortID           :: !Text
    -- | The associated TM port. As there can be multiple RAF or RCF services specified, this setting specifies the 
    -- SLE Port ID of the TM service (RAF or RCF), which is used to send back acknowledges for TCs coming in on this 
    -- FCLTU link.
    , _cfgFCLTUAssociatedTMPort :: !Text
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The default FCLTU configuration for writing to a file
defaultFCLTUConfig :: FCLTUConfig
defaultFCLTUConfig = FCLTUConfig
    { _cfgFCLTUSII = SII "sagr=3.spack=facility-PASS1.fsl-fg=1.cltu=cltu1"
    , _cfgFCLTUPort             = 5009
    , _cfgFCLTUPortID           = "TCPORT"
    , _cfgFCLTUAssociatedTMPort = "TMPORT"
    }

-- | The configuration of the SLE provider itself.
data ProviderConfig = ProviderConfig
    { 
    -- | The common configuration for SLE
    _cfgCommon :: !CommonConfig
    -- | A vector of RAF service configurations to be handled by this provider 
    , _cfgRAFs   :: !(Vector RAFConfig)
    -- | A vector of RCF service configurations to be handled by this provider (currently ignored)
    , _cfgRCFs   :: !(Vector RCFConfig)
    -- | A vector of FCLTU service configurations to be handled by this provider 
    , _cfgFCLTUs :: !(Vector FCLTUConfig)
    }
    deriving (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | The default file name for the default provider config
defaultProviderConfigFileName :: FilePath
defaultProviderConfigFileName = "DefaultProviderConfig.json"


-- | A default config for the provider to be written to a config file
defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig
    { _cfgCommon = defaultCommonConfig
    , _cfgRAFs   = V.singleton defaultRAFConfig
    , _cfgRCFs   = V.singleton defaultRCFConfig
    , _cfgFCLTUs = V.singleton defaultFCLTUConfig
    }


makeLenses ''ProviderConfig
makeLenses ''RAFConfig
makeLenses ''RCFConfig
makeLenses ''FCLTUConfig

-- | Format the config in a pretty way (JSON format)
configPretty :: ProviderConfig -> Text
configPretty cfg = case (decodeUtf8' . B.toStrict . encodePretty) cfg of
    Left  err -> "Error decoding Config in UTF8: " <> T.pack (show err)
    Right val -> val


-- | Write the given provider config to a file in JSON format
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
