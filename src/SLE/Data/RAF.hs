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
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.PDU
import           SLE.Data.ProviderConfig
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID     ( toSII )
import           SLE.Data.WriteCmd              ( SleWrite(SLEPdu) )
import           SLE.State.Classes              ( HasCommonConfig(..)
                                                , HasEventHandler
                                                , sleRaiseEvent
                                                )
import           SLE.State.Events
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
    state    <- readRAFVarIO var

    newState <- case state ^. rafState of
        ServiceInit   -> processInitState cfg var state pdu
        ServiceBound  -> processBoundState cfg var state pdu
        ServiceActive -> processActiveState cfg var state pdu
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
    -> RAF
    -> SlePdu
    -> m ServiceState
processInitState cfg var _state ppdu@(SlePduBind pdu) = do
    logDebug "processInitState: BIND"

    rafResetState var

    sleRaiseEvent (SLEBindReceived pdu)

    cmCfg <- RIO.view commonCfg

    let peer = rafGetPeer var (pdu ^. sleBindInitiatorID)
        sii  = toSII (pdu ^. sleServiceInstanceID)

        res  = do
-- first, when a bind comes in, perform some checks
            if checkPermission (cmCfg ^. cfgAuthorize)
                               peer
                               (var ^. rafPeers)
                               ppdu
            then
                Right ()
            else
                Left
                    ( "Access Denied, credential check failed for inititator: "
                        <> display (pdu ^. sleBindInitiatorID)
                    , AccessDenied
                    )
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

            -- Bind is accepted, set new initiator for later authentications
            rafSetInitiator var peer
            -- send the bind response 
            sendSlePdu var ret
            -- notify the rest of the application, that the bind succeeded
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

processInitState _ _ _ (SlePduUnbind _) = do
    logDebug "Received UNBIND when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduBindReturn _) = do
    logDebug "Received BIND RETURN when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduUnbindReturn _) = do
    logDebug "Received UNBIND RETURN when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduRafStart _) = do
    logDebug "Received START when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduRafStartReturn _) = do
    logDebug "Received START RETURN when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduStop _) = do
    logDebug "Received STOP when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduAck _) = do
    logDebug "Received ACK when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduRafTransferBuffer _) = do
    logDebug "Received TRANSFER BUFFER when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduPeerAbort _) = do
    logDebug "Received PEER ABORT when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduGetParameter _) = do
    logDebug "Received GET PARAMETER when in init state, ignored"
    return ServiceInit

processInitState _cfg _var _state pdu = do
    logWarn
        $  "Init State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceInit


processBoundState
    :: ( MonadIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasCommonConfig env
       )
    => RAFConfig
    -> RAFVar
    -> RAF
    -> SlePdu
    -> m ServiceState
processBoundState cfg var state ppdu@(SlePduUnbind pdu) = do
    logDebug "processBoundState: UNBIND"

    sleRaiseEvent (SLEUnbindReceived pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. rafInitiator)
                       (var ^. rafPeers)
                       ppdu
        then do
            logWarn $ "SLE Unbind received, unbind reason: " <> display
                (pdu ^. sleUnbindReason)
            let ret = SLEPdu $ SlePduUnbindReturn SleUnbindReturn
                    { _sleUnbindRetCredentials = Nothing
                    , _sleUnbindRetResult      = Positive
                    }
            -- reply to the unbind operation
            sendSlePdu var ret
            -- notify the application
            sleRaiseEvent (SLEUnbindSucceed (cfg ^. cfgRAFSII))

            -- reset the state back to the initial values
            rafResetState var
            return ServiceInit
        else do
            logError $ "SLE Unbind failed credentials check"
            let ret = SLEPdu $ SlePduUnbindReturn SleUnbindReturn
                    { _sleUnbindRetCredentials = Nothing
                    , _sleUnbindRetResult      = Negative
                    }
            -- reply to the unbind operation
            sendSlePdu var ret
            -- notify the application
            sleRaiseEvent (SLEUnbindFailed (cfg ^. cfgRAFSII))
            return ServiceBound


processBoundState cfg var state ppdu@(SlePduRafStart pdu) = do
    logDebug "processBoundState: RAF START"

    sleRaiseEvent (SLERafStartReceived pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. rafInitiator)
                       (var ^. rafPeers)
                       ppdu
        then do
            let diag =
                    checkTimeRange (pdu ^. rafStartTime) (pdu ^. rafStopTime)

            -- send response 
            let ret = SLEPdu $ SlePduRafStartReturn $ RafStartReturn
                    { _rafStartRetCredentials = Nothing
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
        else do
            let
                ret = SLEPdu $ SlePduRafStartReturn $ RafStartReturn
                    { _rafStartRetCredentials = Nothing
                    , _rafStartRetInvokeID    = pdu ^. rafStartInvokeID
                    , _rafStartRetResult      = Just
                        (DiagRafStartCommon DiagOtherReason)
                    }
            sendSlePdu var ret
            sleRaiseEvent (SLERafStartFailed (cfg ^. cfgRAFSII))
            return ServiceBound

  where
    checkTimeRange (Just t1) (Just t2) = if t1 <= t2
        then Nothing
        else Just (DiagRafStartSpecific RafStartInvalidStopTime)
    checkTimeRange _ _ = Nothing

processBoundState _cfg _var _state (SlePduBind _) = do
    logWarn "Received BIND when in bound state, ignored"
    return ServiceInit

processBoundState cfg var state (SlePduGetParameter pdu) = do
    logDebug "processBoundState: GET PARAMETER"
    sleRaiseEvent (SLEGetParameterReceived pdu)
    processGetParameter cfg var state pdu
    return ServiceBound

processBoundState _cfg _var _state pdu = do
    logWarn
        $  "Bound State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceBound


processActiveState
    :: ( MonadIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasCommonConfig env
       )
    => RAFConfig
    -> RAFVar
    -> RAF
    -> SlePdu
    -> m ServiceState
processActiveState cfg var state ppdu@(SlePduStop pdu) = do
    logDebug "processActiveState: RAF STOP"

    sleRaiseEvent (SLERafStopReceived pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. rafInitiator)
                       (var ^. rafPeers)
                       ppdu
        then do
            -- send response 
            let ret = SLEPdu $ SlePduAck $ SleAcknowledgement
                    { _sleAckCredentials = Nothing
                    , _sleAckInvokeID    = pdu ^. sleStopInvokeID
                    , _sleResult         = Nothing
                    }
            sendSlePdu var ret
            sleRaiseEvent (SLERafStopSucceed (cfg ^. cfgRAFSII))
            return ServiceBound
        else do
            let ret = SLEPdu $ SlePduAck $ SleAcknowledgement
                    { _sleAckCredentials = Nothing
                    , _sleAckInvokeID    = pdu ^. sleStopInvokeID
                    , _sleResult         = Just DiagOtherReason
                    }
            sendSlePdu var ret
            sleRaiseEvent (SLERafStopFailed (cfg ^. cfgRAFSII))
            return ServiceActive


processActiveState _cfg _var _state pdu = do
    logWarn
        $  "Active State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceBound



processGetParameter
    :: (MonadIO m)
    => RAFConfig
    -> RAFVar
    -> RAF
    -> GetParameterInvocation
    -> m ()
processGetParameter cfg var state pdu = do
    -- get the parameter 
    res <- getParameter cfg var state (_gpParameter pdu)
    -- create the response 
    let response = SLEPdu $ SlePduRafParameterReturn $ RafGetParameterReturn
            { _rgpCredentials = Nothing
            , _rgpInvokeID    = pdu ^. gpInvokeID
            , _rgpResult      = res
            }
    -- send the response
    sendSlePdu var response
    return ()

getParameter
    :: (MonadIO m)
    => RAFConfig
    -> RAFVar
    -> RAF
    -> ParameterName
    -> m (Either DiagnosticRafGet RafGetParameter)
getParameter cfg _var _state ParBufferSize = do
    let bufsize = cfg ^. cfgRAFBufferSize
    return (Right (RafParBufferSize ParBufferSize bufsize))

getParameter _ _ _ _ = do
    return (Left (DiagRafGetSpecific RafUnknownParameter))
