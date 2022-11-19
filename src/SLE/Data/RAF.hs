{-# LANGUAGE 
  TemplateHaskell
  , LinearTypes
#-}
module SLE.Data.RAF
    ( RAF(..)
    , RAFVar 
    , SleRafCmd(..)
    , createRAF
    , rafStartState
    , rafSII
    , rafState 
    , bindRAF
    , newRAFVarIO
    , readRAFVar 
    , readRAFVarIO
    , writeRAFVar
    , sendSleRafCmd
    , rafStateMachine
    ) where

import           RIO hiding ((^.), (.~))

import           Control.Lens

import           SLE.Data.Types.Common
import           SLE.Data.ProviderConfig
import           SLE.Data.PDU


data RAF = RAF
    { _rafSII   :: !SII
    , _rafState :: !ServiceState
    }
makeLenses ''RAF

data RAFVar = RAFVar 
  { rafVar :: !(TVar RAF)
  , rafQueue :: !(TBQueue SleRafCmd)
  }

newRAFVarIO :: (MonadIO m) => RAF -> m RAFVar 
newRAFVarIO raf = do 
  var <- newTVarIO raf 
  q <- newTBQueueIO 100
  return $! (RAFVar var q)


readRAFVarIO :: (MonadIO m ) => RAFVar -> m RAF 
readRAFVarIO var = readTVarIO (rafVar var)

readRAFVar :: RAFVar -> STM RAF 
readRAFVar var = readTVar (rafVar var)

writeRAFVar :: RAFVar -> RAF -> STM () 
writeRAFVar var raf = writeTVar (rafVar var) raf 

sendSleRafCmd :: (MonadIO m) => RAFVar -> SleRafCmd -> m () 
sendSleRafCmd var cmd = atomically $ writeTBQueue (rafQueue var) cmd


-- | Command to be sent to the RAF
data SleRafCmd = BindRaf


rafStartState :: RAFConfig -> RAF
rafStartState cfg = RAF { _rafSII = mkSII (cfg ^. cfgRAFSII), _rafState = ServiceInit }

createRAF :: (MonadIO m) => RAFConfig -> m RAFVar
createRAF cfg = newRAFVarIO (rafStartState cfg)

bindRAF :: RAF -> RAF 
bindRAF raf = raf & rafState .~ ServiceBound


rafStateMachine :: RAFConfig -> RAFVar -> SlePdu -> m ()
rafStateMachine cfg var pdu = undefined 


processState :: (MonadIO m, MonadReader env m, HasLogFunc env) => RAFConfig -> RAF -> SleRafCmd -> m RAF
processState cfg raf cmd = 
  case raf ^. rafState of 
    ServiceInit -> do 
      -- set service to bound 
      let newRAF = raf & rafState .~ ServiceBound 
      return $! newRAF  
    ServiceBound -> do 
      logWarn $ "RAF: " <> display (raf ^. rafSII) <> ": received BIND, but service is already bound"
      return raf 
    ServiceActive -> do 
      logWarn $ "RAF: " <> display (raf ^. rafSII) <> ": received BIND, but service is already active"
      return raf 

-- findService :: SII -> Array RAF -> Maybe RAF
-- findService sii vec = V.find (\r -> sii == _rafSII r) vec


-- createRAFServiceInstances :: CommonConfig -> Vector RAFConfig -> Vector RAF
-- createRAFServiceInstances cfg rafConfigs


-- runRAFProvider :: RAFConfig -> IO ()
-- runRAFProvider cfg = do

