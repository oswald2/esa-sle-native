module SLE.State.ProviderState
    ( ProviderState
    , initialState
    , sleRaiseEvent
    ) where

import           RIO
import qualified RIO.Vector                    as V

import           System.Timer.Updatable

import           SLE.State.Classes
import           SLE.State.Events
import           SLE.State.RAFClasses

import           SLE.Data.CommonConfig
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
    }


initialState
    :: (MonadIO m)
    => ProviderConfig
    -> LogFunc
    -> SleEventHandler
    -> m ProviderState
initialState cfg logFunc eventHandler = do
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

    rafs <- V.imapM createFunc (cfg ^. cfgRAFs)

    return $! ProviderState { _appTimerHBT         = var
                            , _appTimerHBR         = var1
                            , _appConfig           = cfg
                            , _appLogFunc          = logFunc
                            , _appEventHandler     = eventHandler
                            , _appHeartBeat        = hbtr
                            , _appHeartBeatReceive = hbrec
                            , _appRAFs             = rafs
                            }


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
