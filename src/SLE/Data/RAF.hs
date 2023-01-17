{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.RAF
    ( RAF
    , RAFVar
    , SleRafCmd(..)
    , bindRAF
    , newRAFVarIO
    , readRAFVar
    , readRAFVarIO
    , writeRAFVar
    , sendSleRafCmd
    , rafStateMachine
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )

import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )

import           SLE.Data.Bind
import           SLE.Data.Common                ( ServiceState(..)
                                                , SleAcknowledgement
                                                    ( SleAcknowledgement
                                                    , _sleAckCredentials
                                                    , _sleAckInvokeID
                                                    , _sleResult
                                                    )
                                                , sleStopCredentials
                                                , sleStopInvokeID
                                                )
import           SLE.Data.CommonConfig          ( cfgLocal
                                                , isPeer
                                                )
import           SLE.Data.PDU
import           SLE.Data.ProviderConfig        ( RAFConfig
                                                , cfgRAFPortID
                                                , cfgRAFSII
                                                )
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID     ( toSII )
import           SLE.Data.WriteCmd              ( SleWrite(SLEPdu) )
import           SLE.State.Classes              ( HasCommonConfig(..)
                                                , HasEventHandler
                                                , sleRaiseEvent
                                                )
import           SLE.State.Events               ( SleEvent
                                                    ( SLEBindFailed
                                                    , SLEBindReceived
                                                    , SLEBindSucceed
                                                    , SLERafStartReceived
                                                    , SLERafStartSucceed
                                                    , SLERafStatus
                                                    , SLERafStopReceived
                                                    , SLERafStopSucceed
                                                    , SLEUnbindReceived
                                                    , SLEUnbindSucceed
                                                    )
                                                )
import           SLE.State.RAFState

import           Text.Show.Pretty



bindRAF :: RAF -> RAF
bindRAF raf = raf & rafState .~ ServiceBound


rafStateMachine
    :: ( MonadIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasEventHandler env
       , HasLogFunc env
       )
    => RAFConfig
    -> RAFVar
    -> SlePdu
    -> m ()
rafStateMachine cfg var pdu = do
    state    <- getRAFState var
    newState <- case state of
        ServiceInit   -> processInitState cfg var pdu
        ServiceBound  -> processBoundState cfg var pdu
        ServiceActive -> processActiveState cfg var pdu
    setRAFState var newState
    sleRaiseEvent $ SLERafStatus (var ^. rafIdx) newState

processInitState
    :: ( MonadIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasEventHandler env
       , HasLogFunc env
       )
    => RAFConfig
    -> RAFVar
    -> SlePdu
    -> m ServiceState
processInitState cfg var (SlePduBind pdu) = do
    logDebug "processInitState: BIND"

    sleRaiseEvent (SLEBindReceived pdu)

    cmCfg <- RIO.view commonCfg

    let authSet = var ^. rafPeers
        sii     = toSII (pdu ^. sleServiceInstanceID)
        res     = do
-- first, when a bind comes in, perform some checks
            if not (isPeer authSet (pdu ^. sleBindInitiatorID))
                then Left
                    ( "Access Denied, initiator not allowed: "
                        <> display (pdu ^. sleBindInitiatorID)
                    , AccessDenied
                    )
                else Right ()
                                                                                            -- Check, if we are a RAF Bind Request
            if pdu ^. sleBindServiceType /= RtnAllFrames
                then Left
                    ( "Requested Service is not RAF: "
                        <> display (pdu ^. sleBindServiceType)
                    , ServiceTypeNotSupported
                    )
                else Right ()
                                                                                            -- check the requested SLE Version 
            if (pdu ^. sleVersionNumber /= VersionNumber 3)
                    && (pdu ^. sleVersionNumber /= VersionNumber 4)
                then Left
                    ( "Version not supported: "
                        <> display (pdu ^. sleVersionNumber)
                    , VersionNotSupported
                    )
                else Right ()
            if sii /= (cfg ^. cfgRAFSII)
                then Left
                    ( "No such service instance supported: " <> display sii
                    , NoSuchServiceInstance
                    )
                else Right ()
    case res of
        Right _ -> do
            logDebug "OK, sending positive BIND response"
            let ret    = SLEPdu $ SlePduBindReturn retPdu
                retPdu = SleBindReturn
                    { _sleBindRetCredentials = Nothing
                    , _sleBindRetResponderID = cmCfg ^. cfgLocal
                    , _sleBindRetResult      = BindResVersion
                                                   (pdu ^. sleVersionNumber)
                    }
            logDebug $ "ASN1: " <> displayShow (sleBindReturn retPdu)
            sendSlePdu var ret
            sleRaiseEvent (SLEBindSucceed sii)
            return ServiceBound
        Left (errmsg, diag) -> do
            logError errmsg
            let ret = SLEPdu $ SlePduBindReturn SleBindReturn
                    { _sleBindRetCredentials = Nothing
                    , _sleBindRetResponderID = AuthorityIdentifier
                                                   (cfg ^. cfgRAFPortID)
                    , _sleBindRetResult      = BindResDiag diag
                    }
            sendSlePdu var ret
            sleRaiseEvent (SLEBindFailed sii diag)
            return ServiceInit

processInitState _ _ (SlePduUnbind _) = do
    logDebug "Received UNBIND when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduBindReturn _) = do
    logDebug "Received BIND RETURN when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduUnbindReturn _) = do
    logDebug "Received UNBIND RETURN when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduRafStart _) = do
    logDebug "Received START when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduRafStartReturn _) = do
    logDebug "Received START RETURN when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduStop _) = do
    logDebug "Received STOP when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduAck _) = do
    logDebug "Received ACK when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduRafTransferBuffer _) = do
    logDebug "Received TRANSFER BUFFER when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduPeerAbort _) = do
    logDebug "Received PEER ABORT when in init state, ignored"
    return ServiceInit

-- processInitState _cfg _var pdu = do
--     logWarn
--         $  "Init State: Functionality for PDU not yet implemented: "
--         <> fromString (ppShow pdu)
--     return ServiceInit


processBoundState
    :: (MonadIO m, MonadReader env m, HasEventHandler env, HasLogFunc env)
    => RAFConfig
    -> RAFVar
    -> SlePdu
    -> m ServiceState
processBoundState cfg var (SlePduUnbind pdu) = do
    logDebug "processBoundState: UNBIND"

    sleRaiseEvent (SLEUnbindReceived pdu)

    logWarn $ "SLE Unbind received, unbind reason: " <> display
        (pdu ^. sleUnbindReason)
    let ret = SLEPdu $ SlePduUnbindReturn SleUnbindReturn
            { _sleUnbindRetCredentials = Nothing
            , _sleUnbindRetResult      = Positive
            }
    sendSlePdu var ret
    sleRaiseEvent (SLEUnbindSucceed (cfg ^. cfgRAFSII))
    return ServiceInit

processBoundState cfg var (SlePduRafStart pdu) = do
    logDebug "processBoundState: RAF START"

    sleRaiseEvent (SLERafStartReceived pdu)

    let diag = checkTimeRange (pdu ^. rafStartTime) (pdu ^. rafStopTime)

    -- send response 
    let ret = SLEPdu $ SlePduRafStartReturn $ RafStartReturn
            { _rafStartRetCredentials = pdu ^. rafStartCredentials
            , _rafStartRetInvokeID    = pdu ^. rafStartInvokeID
            , _rafStartRetResult      = diag
            }
    case diag of
        Just _  -> return ()
        Nothing -> modifyRAF
            var
            (\raf ->
                raf
                    &  rafStateStartTime
                    .~ pdu
                    ^. rafStartTime
                    &  rafStateStopTime
                    .~ pdu
                    ^. rafStopTime
                    &  rafStateRequestedQuality
                    .~ pdu
                    ^. rafStartRequestedTimeQual
            )
    sendSlePdu var ret
    sleRaiseEvent (SLERafStartSucceed (cfg ^. cfgRAFSII))
    return ServiceActive
  where
    checkTimeRange (Just t1) (Just t2) = if t1 <= t2
        then Nothing
        else Just (DiagRafStartSpecific RafStartInvalidStopTime)
    checkTimeRange _ _ = Nothing

processBoundState _cfg _var (SlePduBind _) = do
    logWarn "Received BIND when in bound state, ignored"
    return ServiceInit


processBoundState _cfg _var pdu = do
    logWarn
        $  "Bound State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceBound


processActiveState
    :: (MonadIO m, MonadReader env m, HasEventHandler env, HasLogFunc env)
    => RAFConfig
    -> RAFVar
    -> SlePdu
    -> m ServiceState
processActiveState cfg var (SlePduStop pdu) = do
    logDebug "processActiveState: RAF STOP"

    sleRaiseEvent (SLERafStopReceived pdu)

    -- TODO check values
    let diag = Nothing

    -- send response 
    let ret = SLEPdu $ SlePduAck $ SleAcknowledgement
            { _sleAckCredentials = pdu ^. sleStopCredentials
            , _sleAckInvokeID    = pdu ^. sleStopInvokeID
            , _sleResult         = diag
            }
    sendSlePdu var ret
    sleRaiseEvent (SLERafStopSucceed (cfg ^. cfgRAFSII))
    return ServiceBound

processActiveState _cfg _var pdu = do
    logWarn
        $  "Active State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceBound


