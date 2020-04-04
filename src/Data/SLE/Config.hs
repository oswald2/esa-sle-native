{-# LANGUAGE 
  TemplateHaskell
#-}
module Data.SLE.Config
  ( Config(..)
  , Data.SLE.Config.defaultConfig
  , cfgTML
  , cfgInitiator
  )
where


import           RIO
import           Control.Lens

import           Data.SLE.TMLConfig
import           Data.SLE.Bind


data Config = Config {
  _cfgTML :: TMLConfig
  , _cfgInitiator :: AuthorityIdentifier
  } deriving (Show, Generic)
makeLenses ''Config


defaultConfig :: Config
defaultConfig = Config { _cfgTML = Data.SLE.TMLConfig.defaultConfig }
