module SLE.State.ProviderState
    ( ProviderState
    , initialState
    , sleRaiseEvent
    , setServiceState
    , appRAFs
    , appFCLTUs
    ) where

import           RIO
import qualified RIO.Vector                    as V

import           System.Timer.Updatable

import           SLE.State.Classes
import           SLE.State.Events
import           SLE.State.FCLTUClasses
import           SLE.State.RAFClasses

import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTU
import           SLE.Data.ProviderConfig
import           SLE.Data.RAF
import           SLE.Data.TMLConfig



data ProviderState = ProviderState
    { _appTimerHBT         :: !(TVar (Maybe (Updatable ())))
    , _appTimerHBR         :: !(TVar (Maybe (Updatable ())))
    , _appConfig           :: !ProviderConfig
    , _appHeartBeat        :: !(TVar Int64)
    , _appHeartBeatReceive :: !(TVar Int64)
    , _appLogFunc          :: !LogFunc
    , _appEventHandler     :: !SleEventHandler
    , _appRAFs             :: !(Vector RAFVar)
    , _appFCLTUs           :: !(Vector FCLTUVar)
    }


appRAFs :: Lens' ProviderState (Vector RAFVar)
appRAFs = lens _appRAFs (\s v -> s { _appRAFs = v })

appFCLTUs :: Lens' ProviderState (Vector FCLTUVar)
appFCLTUs = lens _appFCLTUs (\s v -> s { _appFCLTUs = v })


initialState
    :: (MonadIO m)
    => ProviderConfig
    -> LogFunc
    -> SleEventHandler
    -> ConfigFromApp
    -> m ProviderState
initialState cfg logFunc eventHandler appCfg = do
    var  <- liftIO $ newTVarIO Nothing
    var1 <- liftIO $ newTVarIO Nothing
    let tmlCfg = cfg ^. cfgCommon . cfgTML
    hbrec <- liftIO $ newTVarIO
        ( fromIntegral (cfgHeartbeat tmlCfg)
        * fromIntegral (cfgDeadFactor tmlCfg)
        * 1_000_000
        )
    hbtr <- liftIO $ newTVarIO (fromIntegral (cfgHeartbeat tmlCfg) * 1_000_000)

    -- create the RAFs 
    let createFunc idx cfg' = newRAFVarIO (cfg ^. cfgCommon) cfg' (RAFIdx idx)
        createFcltuFunc idx cfg' = do
            case
                    findTmIdx (cfg ^. cfgRAFs)
                              (cfg' ^. cfgFCLTUAssociatedTMPort)
                of
                    Nothing -> do
                        let
                            msg =
                                utf8BuilderToText
                                    $ "Could not find associated TM Port ID for FCLTU service "
                                    <> display (cfg' ^. cfgFCLTUSII)
                                    <> ", PortID "
                                    <> display
                                           (cfg' ^. cfgFCLTUAssociatedTMPort)
                                    <> " not found."
                        liftIO $ eventHandler (SLEWarning msg)
                        newFCLTUVarIO (cfg ^. cfgCommon)
                                      cfg'
                                      (FCLTUIdx idx)
                                      (TMFirst 0)
                                      appCfg 
                    Just tmIdx -> newFCLTUVarIO (cfg ^. cfgCommon)
                                                cfg'
                                                (FCLTUIdx idx)
                                                (TMRAF (RAFIdx tmIdx))
                                                appCfg

    rafs   <- V.imapM createFunc (cfg ^. cfgRAFs)
    fcltus <- V.imapM createFcltuFunc (cfg ^. cfgFCLTUs)

    return $! ProviderState { _appTimerHBT         = var
                            , _appTimerHBR         = var1
                            , _appConfig           = cfg
                            , _appLogFunc          = logFunc
                            , _appEventHandler     = eventHandler
                            , _appHeartBeat        = hbtr
                            , _appHeartBeatReceive = hbrec
                            , _appRAFs             = rafs
                            , _appFCLTUs           = fcltus
                            }
  where
    findTmIdx rafs port = V.findIndex (\c -> c ^. cfgRAFPortID == port) rafs

instance HasTimer ProviderState where
    getTimerHBT = lens _appTimerHBT (\c v -> c { _appTimerHBT = v })
    getTimerHBR = lens _appTimerHBR (\c v -> c { _appTimerHBR = v })
    hbr = lens _appHeartBeatReceive (\c v -> c { _appHeartBeatReceive = v })
    hbt         = lens _appHeartBeat (\c v -> c { _appHeartBeat = v })

-- | Instance of the logging function for the global state
instance HasLogFunc ProviderState where
    logFuncL = lens _appLogFunc (\c lf -> c { _appLogFunc = lf })


instance HasEventHandler ProviderState where
    sleRaiseEventIO = _appEventHandler


instance HasCommonConfig ProviderState where
    commonCfg = providerCfg . cfgCommon


instance HasProviderConfig ProviderState where
    providerCfg = lens _appConfig (\c cfg -> c { _appConfig = cfg })


instance HasRAF ProviderState where
    getRAFs = lens _appRAFs (\st rafs -> st { _appRAFs = rafs })
    getRAFVar' env (RAFIdx idx) = (V.!?) (env ^. getRAFs) idx

instance HasFCLTU ProviderState where
    getFCLTUs = lens _appFCLTUs (\st fcltus -> st { _appFCLTUs = fcltus })
    getFCLTUVar' env (FCLTUIdx idx) = (env ^. getFCLTUs) V.!? idx


setServiceState
    :: (MonadIO m, MonadReader env m, HasRAF env, HasFCLTU env)
    => TMIdx
    -> ServiceState
    -> m ()
setServiceState (TMRAF idx ) state  = setRAFServiceState idx state
setServiceState (TMRCF _idx) _state = return ()  -- TODO 
setServiceState (TMFirst idx) state = -- TODO 
    setRAFServiceState (RAFIdx (fromIntegral idx)) state
setServiceState (TCFCLTU idx) state = setFCLTUServiceState idx state

