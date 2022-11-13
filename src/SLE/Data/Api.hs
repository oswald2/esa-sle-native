module SLE.Data.Api
    ( SleHandle
    , withSleHandle
    , bind
    ) where

import           RIO

import           SLE.Data.Bind
import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.Input
import           SLE.Data.PDU
import           SLE.Data.ProviderConfig
import           SLE.Data.ServiceInstanceID
import           SLE.Data.TMLConfig

import           SLE.Protocol.TMLProtocol

import           SLE.State.Events

import           Text.Builder





bind
    :: (MonadIO m)
    => CommonConfig
    -> SleHandle
    -> ApplicationIdentifier
    -> [ServiceInstanceAttribute]
    -> m ()
bind cfg hdl appID attrs = do
  -- create an SLE Bind Invocation
    let bnd = mkSleBindInvocation (cfg ^. cfgInitiator)
                                  (PortID (run (decimal (hdl ^. slePort))))
                                  appID
                                  (VersionNumber 2)
                                  (ServiceInstanceIdentifier attrs)
    -- send it to the lower layers
    writeSLEInput hdl (SLEPdu (SlePduBind bnd))



unbind :: (Monad m) => SleHandle -> m ()
unbind = undefined


sendFrame :: (Monad m) => SleHandle -> ByteString -> m ()
sendFrame = undefined

sendOCF :: (Monad m) => SleHandle -> Word32 -> m ()
sendOCF = undefined


receiveFrame :: (Monad m) => SleHandle -> m ByteString
receiveFrame = undefined

receiveOCF :: (Monad m) => SleHandle -> m Word32
receiveOCF = undefined


