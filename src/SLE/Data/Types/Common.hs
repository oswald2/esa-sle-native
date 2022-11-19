module SLE.Data.Types.Common
    ( SII
    , mkSII
    , ServiceState(..)
    ) where

import           RIO


newtype SII = SII { serviceInstanceID :: Text }
    deriving stock (Eq, Ord, Show, Read, Generic)

mkSII :: Text -> SII
mkSII s = SII s

instance Display SII where 
    textDisplay (SII sii) = sii


data ServiceState = ServiceInit | ServiceBound | ServiceActive
    deriving stock (Eq, Ord, Enum, Show, Read, Generic)
