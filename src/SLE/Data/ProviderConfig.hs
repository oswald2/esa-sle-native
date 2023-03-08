{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.ProviderConfig
    ( ProviderConfig(..)
    , ConfigFromApp(..)
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
    , FCLTUChannelType(..)
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
    , cfgFCLTUAcquisitionSequenceLength
    , cfgFCLTUPlop1InitSequenceLength
    , cfgFCLTUBitlockRequired
    , cfgFCLTURFAvailableRequired
    , cfgFCLTUGVCIDVersion
    , cfgFCLTUGVCIDChannelType
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


data ConfigFromApp = ConfigFromApp
    { appSCID :: !Word16
    }


data RAFConfig = RAFConfig
    { _cfgRAFSII          :: !SII
    , _cfgRAFPort         :: !Word16
    , _cfgRAFPortID       :: !Text
    , _cfgRAFAntennaID    :: !AntennaID
    , _cfgRAFBufferSize   :: !Word16
    , _cfgRAFLatency      :: !Int
    , _cfgRAFDeliveryMode :: !DeliveryMode
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

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

data PusMasterChannel = PusMasterChannel
    { mcSCID    :: !Word64
    , mcVersion :: !Word64
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON PusMasterChannel
instance ToJSON PusMasterChannel where
    toEncoding = genericToEncoding defaultOptions


data PusVirtualChannel = PusVirtualChannel
    { vcSCID    :: !Word64
    , vcVersion :: !Word64
    , vcVC      :: !Word64
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON PusVirtualChannel
instance ToJSON PusVirtualChannel where
    toEncoding = genericToEncoding defaultOptions


data RCFGvcid =
    PusRcfMC !PusMasterChannel
    | PusRcfVC !PusVirtualChannel
    deriving (Eq, Show, Read, Generic)

instance FromJSON RCFGvcid
instance ToJSON RCFGvcid where
    toEncoding = genericToEncoding defaultOptions


data RCFConfig = RCFConfig
    { _cfgRCFSII        :: !SII
    , _cfgRCFPort       :: !Word16
    , _cfgRCFPortID     :: !Text
    , _cfgRCFAntennaID  :: !AntennaID
    , _cfgRCFBufferSize :: !Word32
    , _cfgRCFLatency    :: !Int
    , _cfgRCFGVCIDs     :: NonEmpty RCFGvcid
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

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


data FCLTUChannelType = MasterChannel | VirtualChannel !Word8
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

data FCLTUConfig = FCLTUConfig
    { _cfgFCLTUSII                       :: !SII
    , _cfgFCLTUPort                      :: !Word16
    , _cfgFCLTUPortID                    :: !Text
    , _cfgFCLTUAssociatedTMPort          :: !Text
    , _cfgFCLTUAcquisitionSequenceLength :: !Word16
    , _cfgFCLTUPlop1InitSequenceLength   :: !Word16
    , _cfgFCLTUBitlockRequired           :: !Bool
    , _cfgFCLTURFAvailableRequired       :: !Bool
    , _cfgFCLTUGVCIDVersion              :: !Word16
    , _cfgFCLTUGVCIDChannelType          :: !FCLTUChannelType
    }
    deriving stock (Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultFCLTUConfig :: FCLTUConfig
defaultFCLTUConfig = FCLTUConfig
    { _cfgFCLTUSII = SII "sagr=3.spack=facility-PASS1.fsl-fg=1.cltu=cltu1"
    , _cfgFCLTUPort                      = 5009
    , _cfgFCLTUPortID                    = "TCPORT"
    , _cfgFCLTUAssociatedTMPort          = "TMPORT"
    , _cfgFCLTUAcquisitionSequenceLength = 65535
    , _cfgFCLTUPlop1InitSequenceLength   = 65535
    , _cfgFCLTUBitlockRequired           = True
    , _cfgFCLTURFAvailableRequired       = True
    , _cfgFCLTUGVCIDVersion              = 1
    , _cfgFCLTUGVCIDChannelType          = MasterChannel
    }


data ProviderConfig = ProviderConfig
    { _cfgCommon :: !CommonConfig
    , _cfgRAFs   :: !(Vector RAFConfig)
    , _cfgRCFs   :: !(Vector RCFConfig)
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
    , _cfgRCFs   = V.singleton defaultRCFConfig
    , _cfgFCLTUs = V.singleton defaultFCLTUConfig
    }


makeLenses ''ProviderConfig
makeLenses ''RAFConfig
makeLenses ''RCFConfig
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
