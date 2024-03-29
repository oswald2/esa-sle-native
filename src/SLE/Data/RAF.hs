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
    :: ( MonadUnliftIO m
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
                    && (pdu ^. sleVersionNumber /= VersionNumber 5)
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
                    , _sleBindRetResponderID = cmCfg ^. cfgLocal
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
processInitState _ _ _ (SlePduScheduleStatusReport _) = do
    logDebug "Received SCHEDULE STATUS REPORT when in init state, ignored"
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
    :: ( MonadUnliftIO m
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

processBoundState cfg var state ppdu@(SlePduScheduleStatusReport pdu) = do
    logDebug "processBoundState: RAF SCHEDULE STATUS REPORT"

    sleRaiseEvent (SLERafScheduleStatusReceived (cfg ^. cfgRAFSII) pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. rafInitiator)
                       (var ^. rafPeers)
                       ppdu
        then do
            (ok, ret) <- case pdu ^. sleSchedRequestType of
                ReportImmediately -> do
                    processImmediateReport var pdu
                ReportPeriodically secs -> do
                    processPeriodicalReport var secs pdu
                ReportStop -> do
                    processReportStop var pdu

            sendSlePdu var (SLEPdu (SlePduScheduleStatusReturn ret))
            if ok
                then do
                    sleRaiseEvent
                        (SLERafScheduleStatusSuccess (cfg ^. cfgRAFSII))
                else do
                    let msg = "Error scheduling report: "
                            <> fromString (show (ret ^. sleSchedRetResult))
                    sleRaiseEvent
                        (SLERafScheduleStatusFailed (cfg ^. cfgRAFSII) msg)

            return ServiceBound
        else do
            let
                ret =
                    SLEPdu
                        $ SlePduScheduleStatusReturn
                        $ SleScheduleStatusReportReturn
                              { _sleSchedRetCredentials = Nothing
                              , _sleSchedRetInvokeID = pdu ^. sleSchedInvokeID
                              , _sleSchedRetResult = DiagScheduleStatusNegative
                                  (DiagScheduleCommon DiagOtherReason)
                              }
            sendSlePdu var ret
            sleRaiseEvent
                (SLERafScheduleStatusFailed (cfg ^. cfgRAFSII)
                                            "Authentication Failed!"
                )
            return ServiceBound

processBoundState _cfg _var _state (SlePduBind _) = do
    logWarn "Received BIND when in bound state, ignored"
    return ServiceBound

processBoundState _cfg _var _state (SlePduStop _) = do
    logWarn "Received STOP when in bound state, ignored"
    return ServiceBound

processBoundState cfg var state (SlePduGetParameter pdu) = do
    logDebug "processBoundState: GET PARAMETER"
    sleRaiseEvent (SLEGetParameterReceived (cfg ^. cfgRAFSII) pdu)
    processGetParameter cfg var state pdu
    return ServiceBound

processBoundState _cfg _var _state pdu = do
    logWarn
        $  "Bound State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceBound


processActiveState
    :: ( MonadUnliftIO m
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

processActiveState cfg var state ppdu@(SlePduScheduleStatusReport pdu) = do
    logDebug "processActiveState: RAF SCHEDULE STATUS REPORT"

    sleRaiseEvent (SLERafScheduleStatusReceived (cfg ^. cfgRAFSII) pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. rafInitiator)
                       (var ^. rafPeers)
                       ppdu
        then do
            (ok, ret) <- case pdu ^. sleSchedRequestType of
                ReportImmediately -> do
                    processImmediateReport var pdu
                ReportPeriodically secs -> do
                    processPeriodicalReport var secs pdu
                ReportStop -> do
                    processReportStop var pdu

            sendSlePdu var (SLEPdu (SlePduScheduleStatusReturn ret))
            if ok
                then do
                    sleRaiseEvent
                        (SLERafScheduleStatusSuccess (cfg ^. cfgRAFSII))
                else do
                    let msg = "Error scheduling report: "
                            <> fromString (show (ret ^. sleSchedRetResult))
                    sleRaiseEvent
                        (SLERafScheduleStatusFailed (cfg ^. cfgRAFSII) msg)

            return ServiceActive
        else do
            let
                ret =
                    SLEPdu
                        $ SlePduScheduleStatusReturn
                        $ SleScheduleStatusReportReturn
                              { _sleSchedRetCredentials = Nothing
                              , _sleSchedRetInvokeID = pdu ^. sleSchedInvokeID
                              , _sleSchedRetResult = DiagScheduleStatusNegative
                                  (DiagScheduleCommon DiagOtherReason)
                              }
            sendSlePdu var ret
            sleRaiseEvent
                (SLERafScheduleStatusFailed (cfg ^. cfgRAFSII)
                                            "Authentication Failed!"
                )
            return ServiceActive

processActiveState cfg var state (SlePduGetParameter pdu) = do
    logDebug "processActiveState: GET PARAMETER"
    sleRaiseEvent (SLEGetParameterReceived (cfg ^. cfgRAFSII) pdu)
    processGetParameter cfg var state pdu
    return ServiceActive

processActiveState _cfg _var _state pdu = do
    logWarn
        $  "Active State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceActive



processImmediateReport
    :: (MonadIO m)
    => RAFVar
    -> SleScheduleStatusReport
    -> m (Bool, SleScheduleStatusReportReturn)
processImmediateReport var pdu = do
    void $ rafStopSchedule var
    rafSendStatusReport var
    let retPdu = SleScheduleStatusReportReturn
            { _sleSchedRetCredentials = Nothing
            , _sleSchedRetInvokeID    = pdu ^. sleSchedInvokeID
            , _sleSchedRetResult      = DiagScheduleStatusPositive
            }
    return (True, retPdu)


processPeriodicalReport
    :: (MonadUnliftIO m)
    => RAFVar
    -> Word16
    -> SleScheduleStatusReport
    -> m (Bool, SleScheduleStatusReportReturn)
processPeriodicalReport var secs pdu = do
    if secs >= 2 && secs <= 600
        then do
            void $ rafStopSchedule var
            rafStartSchedule var secs
            let retPdu = SleScheduleStatusReportReturn
                    { _sleSchedRetCredentials = Nothing
                    , _sleSchedRetInvokeID    = pdu ^. sleSchedInvokeID
                    , _sleSchedRetResult      = DiagScheduleStatusPositive
                    }
            return (True, retPdu)
        else do
            let diag   = DiagScheduleSpecific InvalidReportingCycle
                retPdu = SleScheduleStatusReportReturn
                    { _sleSchedRetCredentials = Nothing
                    , _sleSchedRetInvokeID    = pdu ^. sleSchedInvokeID
                    , _sleSchedRetResult      = DiagScheduleStatusNegative diag
                    }
            return (False, retPdu)



processReportStop
    :: (MonadIO m)
    => RAFVar
    -> SleScheduleStatusReport
    -> m (Bool, SleScheduleStatusReportReturn)
processReportStop var pdu = do
    res <- rafStopSchedule var
    if res
        then do
            let retPdu = SleScheduleStatusReportReturn
                    { _sleSchedRetCredentials = Nothing
                    , _sleSchedRetInvokeID    = pdu ^. sleSchedInvokeID
                    , _sleSchedRetResult      = DiagScheduleStatusPositive
                    }
            return (True, retPdu)
        else do
            let diag   = DiagScheduleSpecific AlreadyStopped
                retPdu = SleScheduleStatusReportReturn
                    { _sleSchedRetCredentials = Nothing
                    , _sleSchedRetInvokeID    = pdu ^. sleSchedInvokeID
                    , _sleSchedRetResult      = DiagScheduleStatusNegative diag
                    }
            return (False, retPdu)


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
    return (Right (RafParBufferSize bufsize))

getParameter cfg _var _state ParDeliveryMode = do
    let mode = cfg ^. cfgRAFDeliveryMode
    return (Right (RafParDeliveryMode mode))

getParameter cfg _var _state ParLatencyLimit = do
    let latency = (cfg ^. cfgRAFLatency) `quot` 1_000_000
    return (Right (RafParLatencyLimit (Just (fromIntegral latency))))

getParameter _cfg _var state ParRequestedFrameQuality = do
    let qual = state ^. rafStateRequestedQuality
    return (Right (RafParReqFrameQuality qual))

getParameter _cfg _var _state ParMinReportingCycle = do
    return (Right (RafParMinReportingCycle 2))

getParameter _cfg _var _state ParReturnTimeoutPeriod = do
    return (Right (RafParReturnTimeout 60))

getParameter _cfg _var _state ParPermittedFrameQuality = do
    return (Right (RafParPermittedFrameQuality [AllFrames]))

getParameter _cfg var _state ParReportingCycle = do
    val <- rafGetReportSchedule var 
    return (Right (RafParReportingCycle val))


getParameter _ _ _ _ = do
    return (Left (DiagRafGetSpecific RafUnknownParameter))
