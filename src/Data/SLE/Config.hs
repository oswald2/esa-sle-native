module Data.SLE.Config
(
  Config(..)
  , Data.SLE.Config.defaultConfig
)
where 


import RIO 

import Data.SLE.TMLConfig


data Config = Config {
  _cfgTML :: TMLConfig
  } deriving (Show, Generic)



defaultConfig :: Config 
defaultConfig = Config {
  _cfgTML = Data.SLE.TMLConfig.defaultConfig
  }