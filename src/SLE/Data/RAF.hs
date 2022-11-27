{-# LANGUAGE 
  TemplateHaskell
  , LinearTypes
#-}
module SLE.Data.RAF
    ( RAF(..)
    , RAFVar 
    , SleRafCmd(..)
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
    , rafVar 
    , rafQueue
    , rafSleHandle
    , rafVarCfg
    ) where

import           RIO hiding ((^.), (.~))

import           Control.Lens
import Control.Monad.Trans.Maybe 

import           SLE.Data.Types.Common
import           SLE.Data.ProviderConfig
import           SLE.Data.PDU
import SLE.Data.Bind 
import SLE.Data.Handle
import SLE.Data.Input

import Text.Show.Pretty 


data RAF = RAF
    { _rafSII   :: !SII
    , _rafState :: !ServiceState
    }
makeLenses ''RAF

-- | Command to be sent to the RAF
data SleRafCmd = BindRaf

data RAFVar = RAFVar 
  { _rafVar :: !(TVar RAF)
  , _rafQueue :: !(TBQueue SleRafCmd)
  , _rafSleHandle :: !SleHandle
  , _rafVarCfg :: !RAFConfig
  }
makeLenses ''RAFVar

newRAFVarIO :: (MonadIO m) => RAFConfig -> m RAFVar 
newRAFVarIO cfg = do 
  let raf = rafStartState cfg 
  var <- newTVarIO raf 
  q <- newTBQueueIO 100
  hdl <- newSleHandle (fromIntegral (cfg ^. cfgRAFPort))
  return $! (RAFVar var q hdl cfg)


readRAFVarIO :: (MonadIO m ) => RAFVar -> m RAF 
readRAFVarIO var = readTVarIO (_rafVar var)

readRAFVar :: RAFVar -> STM RAF 
readRAFVar var = readTVar (_rafVar var)

writeRAFVar :: RAFVar -> RAF -> STM () 
writeRAFVar var raf = writeTVar (_rafVar var) raf 

getRAFState :: (MonadIO m) => RAFVar -> m ServiceState
getRAFState var = _rafState <$> readTVarIO (_rafVar var)

sendSleRafCmd :: (MonadIO m) => RAFVar -> SleRafCmd -> m () 
sendSleRafCmd var cmd = atomically $ writeTBQueue (_rafQueue var) cmd

sendSlePdu :: (MonadIO m) => RAFVar -> SleInput -> m () 
sendSlePdu var input = writeSLEInput (_rafSleHandle var) input 





rafStartState :: RAFConfig -> RAF
rafStartState cfg = RAF { _rafSII = cfg ^. cfgRAFSII, _rafState = ServiceInit }

bindRAF :: RAF -> RAF 
bindRAF raf = raf & rafState .~ ServiceBound


rafStateMachine :: (MonadIO m, MonadReader env m, HasLogFunc env) => RAFConfig -> RAFVar -> SlePdu -> m ()
rafStateMachine cfg var pdu = do
  state <- getRAFState var 
  case state of 
    ServiceInit -> processInitState cfg var pdu 
    ServiceBound -> processBoundState cfg var pdu 
    ServiceActive -> processActiveState cfg var pdu   


processInitState :: (MonadIO m, MonadReader env m, HasLogFunc env) => RAFConfig -> RAFVar -> SlePdu -> m () 
processInitState cfg var (SlePduBind pdu) = do 
  logDebug "processInitState: BIND"
  let auth = AuthorityIdentifier (cfg ^. cfgRAFPeer)
  res <- runMaybeT $ do 
    -- first, when a bind comes in, perform some checks
    _ <- if (pdu ^. sleBindInitiatorID /= auth) 
      then do 
        logWarn $ "Access Denied, initiator now allowed: " <> display (pdu ^. sleBindInitiatorID)
        returnBind AccessDenied 
        return Nothing 
      else return $ Just True 
    -- Check, if we are a RAF Bind Request
    _ <- if (pdu ^. sleBindServiceType /= RtnAllFrames)
      then do 
        logWarn $ "Requested Service is not RAF: " <> display (pdu ^. sleBindServiceType)
        returnBind ServiceTypeNotSupported
        return Nothing 
      else return (Just True)
    -- check the requested SLE Version 
    if (pdu ^. sleVersionNumber /= VersionNumber 3)
      then do 
        logWarn $ "Version not supported: " <> display (pdu ^. sleVersionNumber)
        returnBind VersionNotSupported
        return Nothing 
      else return (Just True)
    -- if (pdu ^. sleServiceInstanceID) /= (cfg ^. cfgRAFSII)
    --   then do 
    --     logWarn $ "No such service instance supported: " <> display (pdu ^. sleServiceInstanceID)
    --     returnBind NoSuchServiceInstance
    --     return Nothing 
    --   else return (Just True)
  case res of 
    Just _ -> do 
      logDebug "OK, sending positive BIND response"
      let ret = SLEPdu $ SlePduBindReturn retPdu 
          retPdu = SleBindReturn {
                        _sleBindRetCredentials = Nothing 
                        , _sleBindRetResponderID = AuthorityIdentifier (cfg ^. cfgRAFPortID)
                        , _sleBindRetResult = BindResVersion (pdu ^. sleVersionNumber) 
                        }
      logDebug $ "ASN1: " <> displayShow (sleBindReturn retPdu)
      sendSlePdu var ret 
    Nothing -> return () 
  where 
    returnBind diag = do 
        let ret = SLEPdu $ SlePduBindReturn SleBindReturn {
          _sleBindRetCredentials = Nothing 
          , _sleBindRetResponderID = AuthorityIdentifier (cfg ^. cfgRAFPortID)
          , _sleBindRetResult = BindResDiag diag 
          }
        sendSlePdu var ret 


processInitState _cfg _var pdu = do 
  logWarn $ "Functionality for PDU not yet implemented: " <> fromString (ppShow pdu)


processBoundState cfg var pdu = undefined 


processActiveState cfg var pdu = undefined 


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

