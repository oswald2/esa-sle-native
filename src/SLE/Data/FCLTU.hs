{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.FCLTU
    ( FCLTU
    , FCLTUVar
    , SleFcltuCmd(..)
    , bindFCLTU
    , newFCLTUVarIO
    , readFCLTUVar
    , readFCLTUVarIO
    , writeFCLTUVar
    , sendSleFcltuCmd
    , fcltuStateMachine
    ) where

import           RIO                     hiding ( (.~)
                                                , (^.)
                                                )

import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )

import           SLE.Data.Bind
import           SLE.Data.CCSDSTime
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.HexBytes
import           SLE.Data.PDU
import           SLE.Data.ProviderConfig
import           SLE.Data.ServiceInstanceID
import           SLE.Data.WriteCmd
import           SLE.State.Classes
import           SLE.State.Events
import           SLE.State.FCLTUState

import           Text.Show.Pretty        hiding ( Time )



bindFCLTU :: FCLTU -> FCLTU
bindFCLTU fcltu = fcltu & fcltuState .~ ServiceBound


fcltuStateMachine
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasEventHandler env
       , HasLogFunc env
       )
    => FCLTUConfig
    -> FCLTUVar
    -> (Word64 -> IO ())
    -> SlePdu
    -> m ()
fcltuStateMachine cfg var perfFunc pdu = do
    state    <- readFCLTUVarIO var

    newState <- case state ^. fcltuState of
        ServiceInit   -> processInitState cfg var state pdu
        ServiceBound  -> processBoundState cfg var state pdu
        ServiceActive -> processActiveState cfg var state perfFunc pdu
    setFCLTUState var newState
    sleRaiseEvent $ SLEFcltuStatus (var ^. fcltuIdx) newState

processInitState
    :: ( MonadIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasEventHandler env
       , HasLogFunc env
       )
    => FCLTUConfig
    -> FCLTUVar
    -> FCLTU
    -> SlePdu
    -> m ServiceState
processInitState cfg var _state ppdu@(SlePduBind pdu) = do
    logDebug "processInitState: BIND"

    fcltuResetState var
    sleRaiseEvent (SLEBindReceived pdu)

    cmCfg <- RIO.view commonCfg

    let peer = fcltuGetPeer var (pdu ^. sleBindInitiatorID)
        sii  = toSII (pdu ^. sleServiceInstanceID)

        res  = do
-- first, when a bind comes in, perform some checks
            if checkPermission (cmCfg ^. cfgAuthorize)
                               peer
                               (var ^. fcltuPeers)
                               ppdu
            then
                Right ()
            else
                Left
                    ( "Access Denied, initiator not allowed: "
                        <> display (pdu ^. sleBindInitiatorID)
                    , AccessDenied
                    )
                                                                                                                            -- Check, if we are a FCLTU Bind Request
            if pdu ^. sleBindServiceType /= FwdCltu
                then Left
                    ( "Requested Service is not FCLTU: "
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
            if sii /= (cfg ^. cfgFCLTUSII)
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

            -- bind is accepted, set the new initiator for later authentications
            fcltuSetInitiator var peer
            -- send the bind response 
            sendSleFcltuPdu var ret
            -- notify the application
            sleRaiseEvent (SLEBindSucceed sii)
            return ServiceBound
        Left (errmsg, diag) -> do
            logError errmsg
            let
                ret = SLEPdu $ SlePduBindReturn SleBindReturn
                    { _sleBindRetCredentials = Nothing
                    , _sleBindRetResponderID = AuthorityIdentifier
                                                   (cfg ^. cfgFCLTUPortID)
                    , _sleBindRetResult      = BindResDiag diag
                    }
            sendSleFcltuPdu var ret
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
    logDebug "Received RAF START when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduRafStartReturn _) = do
    logDebug "Received RAF START RETURN when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduStop _) = do
    logDebug "Received STOP when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduAck _) = do
    logDebug "Received ACK when in init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduRafTransferBuffer _) = do
    logDebug "Received RAF TRANSFER BUFFER when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduScheduleStatusReport _) = do
    logDebug "Received SCHEDULE STATUS REPORT when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ _ (SlePduPeerAbort _) = do
    logDebug "Received PEER ABORT when in init state, ignored"
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
    => FCLTUConfig
    -> FCLTUVar
    -> FCLTU
    -> SlePdu
    -> m ServiceState
processBoundState cfg var state ppdu@(SlePduUnbind pdu) = do
    logDebug "FCLTU processBoundState: UNBIND"

    sleRaiseEvent (SLEUnbindReceived pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. fcltuInitiator)
                       (var ^. fcltuPeers)
                       ppdu
        then do
            logWarn $ "SLE Unbind received, unbind reason: " <> display
                (pdu ^. sleUnbindReason)
            let ret = SLEPdu $ SlePduUnbindReturn SleUnbindReturn
                    { _sleUnbindRetCredentials = Nothing
                    , _sleUnbindRetResult      = Positive
                    }
            sendSleFcltuPdu var ret
            sleRaiseEvent (SLEUnbindSucceed (cfg ^. cfgFCLTUSII))
            fcltuResetState var
            return ServiceInit
        else do
            logError $ "SLE Unbind failed credentials check"
            let ret = SLEPdu $ SlePduUnbindReturn SleUnbindReturn
                    { _sleUnbindRetCredentials = Nothing
                    , _sleUnbindRetResult      = Negative
                    }
            -- reply to the unbind operation
            sendSleFcltuPdu var ret
            -- notify the application
            sleRaiseEvent (SLEUnbindFailed (cfg ^. cfgFCLTUSII))
            return ServiceBound


processBoundState cfg var state ppdu@(SlePduFcltuStart pdu) = do
    logDebug "processBoundState: FCLTU START"

    sleRaiseEvent (SLEFcltuStartReceived pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. fcltuInitiator)
                       (var ^. fcltuPeers)
                       ppdu
        then do
            now <- getCurrentTime

            let diag = FcltuStartPositive FcltuStartTimes
                    { _fcltuStartRadiationTime = Time now
                    , _fcltuStopRadiationTime  = Nothing
                    }
                cltuID = pdu ^. fcluStartFirstCltuIdentification

            void $ setCltuID var cltuID

            -- send response 
            let ret = SLEPdu $ SlePduFcltuStartReturn $ FcltuStartReturn
                    { _fcltuStartRetCredentials = Nothing
                    , _fcltuStartRetInvokeID    = pdu ^. fcltuStartInvokeID
                    , _fcltuStartRetResult      = diag
                    }
            case diag of
                FcltuStartPositive _ -> modifyFCLTU
                    var
                    (\fcltu -> fcltu & fcltuStartRadiationTime .~ now)
                _ -> return ()

            sendSleFcltuPdu var ret
            sleRaiseEvent (SLEFcltuStartSucceed (cfg ^. cfgFCLTUSII))
            return ServiceActive
        else do
            -- send response 
            let
                ret = SLEPdu $ SlePduFcltuStartReturn $ FcltuStartReturn
                    { _fcltuStartRetCredentials = Nothing
                    , _fcltuStartRetInvokeID    = pdu ^. fcltuStartInvokeID
                    , _fcltuStartRetResult      = FcltuStartNegative
                        (DiagFcltuStartCommon DiagOtherReason)
                    }

            sendSleFcltuPdu var ret
            sleRaiseEvent (SLEFcltuStartFailed (cfg ^. cfgFCLTUSII))
            return ServiceBound

processBoundState cfg var state ppdu@(SlePduScheduleStatusReport pdu) = do
    logDebug "processBoundState: FCLTU SCHEDULE STATUS REPORT"

    sleRaiseEvent (SLEFcltuScheduleStatusReceived (cfg ^. cfgFCLTUSII) pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. fcltuInitiator)
                       (var ^. fcltuPeers)
                       ppdu
        then do
            (ok, ret) <- case pdu ^. sleSchedRequestType of
                ReportImmediately -> do
                    processImmediateReport var pdu
                ReportPeriodically secs -> do
                    processPeriodicalReport var secs pdu
                ReportStop -> do
                    processReportStop var pdu

            sendSleFcltuPdu var (SLEPdu (SlePduScheduleStatusReturn ret))
            if ok
                then do
                    sleRaiseEvent
                        (SLEFcltuScheduleStatusSuccess (cfg ^. cfgFCLTUSII))
                else do
                    let msg = "Error scheduling report: "
                            <> fromString (show (ret ^. sleSchedRetResult))
                    sleRaiseEvent
                        (SLEFcltuScheduleStatusFailed (cfg ^. cfgFCLTUSII) msg)

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
            sendSleFcltuPdu var ret
            sleRaiseEvent
                (SLEFcltuScheduleStatusFailed (cfg ^. cfgFCLTUSII)
                                              "Authentication Failed!"
                )
            return ServiceBound

processBoundState _cfg _var _state (SlePduBind _) = do
    logWarn "Received BIND when in FCLTU bound state, ignored"
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
    => FCLTUConfig
    -> FCLTUVar
    -> FCLTU
    -> (Word64 -> IO ())
    -> SlePdu
    -> m ServiceState
processActiveState cfg var state _ ppdu@(SlePduStop pdu) = do
    logDebug "processActiveState: FCLTU STOP"

    sleRaiseEvent (SLEFcltuStopReceived pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. fcltuInitiator)
                       (var ^. fcltuPeers)
                       ppdu
        then do
            -- send response 
            let ret = SLEPdu $ SlePduAck $ SleAcknowledgement
                    { _sleAckCredentials = pdu ^. sleStopCredentials
                    , _sleAckInvokeID    = pdu ^. sleStopInvokeID
                    , _sleResult         = Nothing
                    }
            sendSleFcltuPdu var ret
            sleRaiseEvent (SLEFcltuStopSucceed (cfg ^. cfgFCLTUSII))
            return ServiceBound
        else do
            let ret = SLEPdu $ SlePduAck $ SleAcknowledgement
                    { _sleAckCredentials = pdu ^. sleStopCredentials
                    , _sleAckInvokeID    = pdu ^. sleStopInvokeID
                    , _sleResult         = Just DiagOtherReason
                    }
            sendSleFcltuPdu var ret
            sleRaiseEvent (SLEFcltuStopFailed (cfg ^. cfgFCLTUSII))
            return ServiceActive


processActiveState cfg var state perfFunc ppdu@(SlePduFcltuTransferData pdu) =
    do
        logDebug "processActiveState: FCLTU TRANSFER DATA"

        -- update the statistics
        liftIO $ perfFunc (fromIntegral (hlength (pdu ^. fcltuData)))

        cmCfg <- RIO.view commonCfg

        let diag       = Nothing
            !cltuID    = pdu ^. fcltuDataIdent
            !newCltuID = cltuID + 1

        if checkPermission (cmCfg ^. cfgAuthorize)
                           (state ^. fcltuInitiator)
                           (var ^. fcltuPeers)
                           ppdu
            then do
                -- set the CLTU ID in the state 
                void $ setCltuID var cltuID

                -- send response 
                let ret =
                        SLEPdu
                            $ SlePduFcltuTransReturn
                            $ FcltuTransferDataReturn
                                  { _fcltuTransRetCredentials     = Nothing
                                  , _fcltuTransRetInvokeID        =
                                      pdu ^. fcltuDataInvokeID
                                  , _fcltuTransRetIdentification  = newCltuID
                                  , _fcltuTransRetBufferAvailable = maxBound -- we return always the max value here
                                  , _fcltuTransRetResult          = diag
                                  }
                sendSleFcltuPdu var ret
                sleRaiseEvent
                    (SLEFcltuTransferData (cfg ^. cfgFCLTUSII)
                                          (var ^. fcltuIdx)
                                          (var ^. fcltuTMIdx)
                                          pdu
                    )
                return ServiceActive
            else do
                let
                    ret =
                        SLEPdu
                            $ SlePduFcltuTransReturn
                            $ FcltuTransferDataReturn
                                  { _fcltuTransRetCredentials     = Nothing
                                  , _fcltuTransRetInvokeID        =
                                      pdu ^. fcltuDataInvokeID
                                  , _fcltuTransRetIdentification  = newCltuID
                                  , _fcltuTransRetBufferAvailable = maxBound -- we return always the max value here
                                  , _fcltuTransRetResult          = Just
                                      (FcltuTransCommon DiagOtherReason)
                                  }
                sendSleFcltuPdu var ret
                sleRaiseEvent (SLEFcltuTransDataFailed (cfg ^. cfgFCLTUSII))
                return ServiceActive


processActiveState cfg var state _perfFunc ppdu@(SlePduScheduleStatusReport pdu) = do
    logDebug "processActiveState: FCLTU SCHEDULE STATUS REPORT"

    sleRaiseEvent (SLEFcltuScheduleStatusReceived (cfg ^. cfgFCLTUSII) pdu)

    cmCfg <- RIO.view commonCfg

    if checkPermission (cmCfg ^. cfgAuthorize)
                       (state ^. fcltuInitiator)
                       (var ^. fcltuPeers)
                       ppdu
        then do
            (ok, ret) <- case pdu ^. sleSchedRequestType of
                ReportImmediately -> do
                    processImmediateReport var pdu
                ReportPeriodically secs -> do
                    processPeriodicalReport var secs pdu
                ReportStop -> do
                    processReportStop var pdu

            sendSleFcltuPdu var (SLEPdu (SlePduScheduleStatusReturn ret))
            if ok
                then do
                    sleRaiseEvent
                        (SLEFcltuScheduleStatusSuccess (cfg ^. cfgFCLTUSII))
                else do
                    let msg = "Error scheduling report: "
                            <> fromString (show (ret ^. sleSchedRetResult))
                    sleRaiseEvent
                        (SLEFcltuScheduleStatusFailed (cfg ^. cfgFCLTUSII) msg)

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
            sendSleFcltuPdu var ret
            sleRaiseEvent
                (SLEFcltuScheduleStatusFailed (cfg ^. cfgFCLTUSII)
                                              "Authentication Failed!"
                )
            return ServiceActive

processActiveState _cfg _var _state _perfFunc pdu = do
    logWarn
        $  "Active State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceActive




processImmediateReport
    :: (MonadIO m)
    => FCLTUVar
    -> SleScheduleStatusReport
    -> m (Bool, SleScheduleStatusReportReturn)
processImmediateReport var pdu = do
    void $ fcltuStopSchedule var
    fcltuSendStatusReport var
    let retPdu = SleScheduleStatusReportReturn
            { _sleSchedRetCredentials = Nothing
            , _sleSchedRetInvokeID    = pdu ^. sleSchedInvokeID
            , _sleSchedRetResult      = DiagScheduleStatusPositive
            }
    return (True, retPdu)


processPeriodicalReport
    :: (MonadUnliftIO m)
    => FCLTUVar
    -> Word16
    -> SleScheduleStatusReport
    -> m (Bool, SleScheduleStatusReportReturn)
processPeriodicalReport var secs pdu = do
    if secs >= 2 && secs <= 600
        then do
            void $ fcltuStopSchedule var
            fcltuStartSchedule var secs
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
    => FCLTUVar
    -> SleScheduleStatusReport
    -> m (Bool, SleScheduleStatusReportReturn)
processReportStop var pdu = do
    res <- fcltuStopSchedule var
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
