{-|
Module      : SLE.Data.CommonConfig
Description : Provides the common configuration values for the SLE protocol
Copyright   : (c) Michael Oswald, 2022
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This module provides the data types and values for the common configuration values for SLE.
-}
{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.CommonConfig
    ( SleAuthType(..)
    , CommonConfig(..)
    , Peer(..)
    , HashToUse(..)
    , defaultCommonConfig
    , cfgTML
    , cfgPeers
    , cfgLocal
    , cfgAuthorize
    , cfgSHAType
    , cfgPassword
    , cfgVersion
    , mkPeerSet
    , isPeer
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.HashMap                   as HM

import           Control.Lens

import           Data.Aeson

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.HexBytes
import           SLE.Data.TMLConfig      hiding ( loadConfigJSON
                                                , writeConfigJSON
                                                )

-- | Older SLE versions still use the SHA1 algorithm for hash calculations. In order to stay compatible to
-- these versions, the hash function can be specified. Currently supported is the old SHA1 and the SHA256
-- functions
data HashToUse = SHA1 | SHA256
    deriving (Eq, Ord, Enum, Show, Read, Generic)

instance FromJSON HashToUse
instance ToJSON HashToUse where
    toEncoding = genericToEncoding defaultOptions

-- | The Configuration for the Authentication. SLE can handle not Authentication at all (@AuthNone@), only 
-- authenticate the BIND operations (@AuthBind@) or authenticate every operation (@AuthAll@).
data SleAuthType =
    AuthNone
    | AuthBind
    | AuthAll
    deriving (Eq, Ord, Enum, Show, Read, Generic)


instance FromJSON SleAuthType
instance ToJSON SleAuthType where
    toEncoding = genericToEncoding defaultOptions

-- | The type for a Peer. Contains and Authority Identifier (basically a Text value) and a password in hex-ASCII.
data Peer = Peer
    { cfgPeerAuthorityID :: !AuthorityIdentifier
    , cfgPeerPassword    :: !HexBytes
    }
    deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Peer
instance ToJSON Peer where
    toEncoding = genericToEncoding defaultOptions

-- | The common configuration itself. For all fields, lenses are generated automatically.
data CommonConfig = CommonConfig
    { 
    -- | The configuration of the TML layer (see 'TMLConfig')
    _cfgTML       :: !TMLConfig
    -- | A list of peers, that are allowed to connect
    , _cfgPeers     :: [Peer]
    -- | The authority identifier of this SLE entity
    , _cfgLocal     :: !AuthorityIdentifier
    -- | Specify the authentication scheme for the SLE operations, see 'SleAuthType'
    , _cfgAuthorize :: !SleAuthType
    -- | The hash function to use for the credentials, see 'HashToUse'
    , _cfgSHAType   :: !HashToUse
    -- | The version number of SLE to be used. Currently supported are versions 3, 4 and 5.
    , _cfgVersion   :: !SleVersion
    -- | The password of the local entity in hex-ASCII
    , _cfgPassword  :: !HexBytes
    }
    deriving (Show, Read, Generic)

instance FromJSON CommonConfig
instance ToJSON CommonConfig where
    toEncoding = genericToEncoding defaultOptions

-- | Helper function to create a 'HashMap' from the 'AuthorityIdenfifier's to 'Peer's from the config 
mkPeerSet :: CommonConfig -> HashMap AuthorityIdentifier Peer
mkPeerSet cfg =
    HM.fromList $ map (\p -> (cfgPeerAuthorityID p, p)) (_cfgPeers cfg)

-- | Check if the given peer (the given 'AuthorityIdentifier') is in the allowed peer hashmap
isPeer :: HashMap AuthorityIdentifier Peer -> AuthorityIdentifier -> Bool
isPeer hm auid = HM.member auid hm

-- | A list of default peers, which area allowed. Mainly used to write a default configuration
defaultPeers :: [Peer]
defaultPeers =
    [ Peer (AuthorityIdentifier "EGSCC") (bsToHex "12345678")
    , Peer
        (AuthorityIdentifier "SLETT")
        (bsToHex (B.pack [0xaa, 0xbb, 0xcc, 0xdd, 0xaa, 0xbb, 0xcc, 0xdd]))
    ]

-- | The default common config. When a config is written to file, this is the config that is put there.
defaultCommonConfig :: CommonConfig
defaultCommonConfig = CommonConfig
    { _cfgTML       = SLE.Data.TMLConfig.defaultConfig
    , _cfgPeers     = defaultPeers
    , _cfgLocal     = AuthorityIdentifier "PARAGONTT"
    , _cfgAuthorize = AuthNone
    , _cfgSHAType   = SHA1
    , _cfgVersion   = SLE3
    , _cfgPassword  = bsToHex
                          (B.pack
                              [ 0x00
                              , 0x01
                              , 0x02
                              , 0x03
                              , 0x04
                              , 0x05
                              , 0x06
                              , 0x07
                              , 0x08
                              , 0x09
                              , 0x0a
                              , 0x0b
                              , 0x0c
                              , 0x0d
                              , 0x0e
                              , 0x0f
                              ]
                          )
    }
makeLenses ''CommonConfig


