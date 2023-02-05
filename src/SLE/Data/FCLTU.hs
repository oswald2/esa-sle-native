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
import qualified RIO.ByteString                as B

import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )

import           SLE.Data.Bind
import           SLE.Data.CCSDSTime
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
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
    :: ( MonadIO m
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
    state    <- getFCLTUState var
    newState <- case state of
        ServiceInit   -> processInitState cfg var pdu
        ServiceBound  -> processBoundState cfg var pdu
        ServiceActive -> processActiveState cfg var perfFunc pdu
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
    -> SlePdu
    -> m ServiceState
processInitState cfg var (SlePduBind pdu) = do
    logDebug "processInitState: BIND"

    fcltuResetState var
    sleRaiseEvent (SLEBindReceived pdu)

    cmCfg <- RIO.view commonCfg

    let authSet = var ^. fcltuPeers
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
            sendSleFcltuPdu var ret
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
    logDebug "Received RAF START when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduRafStartReturn _) = do
    logDebug "Received RAF START RETURN when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduStop _) = do
    logDebug "Received STOP when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduAck _) = do
    logDebug "Received ACK when in init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduRafTransferBuffer _) = do
    logDebug "Received RAF TRANSFER BUFFER when in FCLTU init state, ignored"
    return ServiceInit
processInitState _ _ (SlePduPeerAbort _) = do
    logDebug "Received PEER ABORT when in init state, ignored"
    return ServiceInit

processInitState _cfg _var pdu = do
    logWarn
        $  "Init State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceInit


processBoundState
    :: (MonadIO m, MonadReader env m, HasEventHandler env, HasLogFunc env)
    => FCLTUConfig
    -> FCLTUVar
    -> SlePdu
    -> m ServiceState
processBoundState cfg var (SlePduUnbind pdu) = do
    logDebug "FCLTU processBoundState: UNBIND"

    sleRaiseEvent (SLEUnbindReceived pdu)

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

processBoundState cfg var (SlePduFcltuStart pdu) = do
    logDebug "processBoundState: FCLTU START"

    sleRaiseEvent (SLEFcltuStartReceived pdu)

    now <- getCurrentTime

    let diag = FcltuStartPositive FcltuStartTimes
            { _fcltuStartRadiationTime = Time now
            , _fcltuStopRadiationTime  = Nothing
            }
        cltuID = pdu ^. fcluStartFirstCltuIdentification

    void $ setCltuID var cltuID

    -- send response 
    let ret = SLEPdu $ SlePduFcltuStartReturn $ FcltuStartReturn
            { _fcltuStartRetCredentials = pdu ^. fcltuStartCredentials
            , _fcltuStartRetInvokeID    = pdu ^. fcltuStartInvokeID
            , _fcltuStartRetResult      = diag
            }
    case diag of
        FcltuStartPositive _ ->
            modifyFCLTU var (\fcltu -> fcltu & fcltuStartRadiationTime .~ now)
        _ -> return ()

    sendSleFcltuPdu var ret
    sleRaiseEvent (SLEFcltuStartSucceed (cfg ^. cfgFCLTUSII))
    return ServiceActive


processBoundState _cfg _var (SlePduBind _) = do
    logWarn "Received BIND when in FCLTU bound state, ignored"
    return ServiceBound


processBoundState _cfg _var pdu = do
    logWarn
        $  "Bound State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceBound


processActiveState
    :: (MonadIO m, MonadReader env m, HasEventHandler env, HasLogFunc env)
    => FCLTUConfig
    -> FCLTUVar
    -> (Word64 -> IO ())
    -> SlePdu
    -> m ServiceState
processActiveState cfg var _ (SlePduStop pdu) = do
    logDebug "processActiveState: FCLTU STOP"

    sleRaiseEvent (SLEFcltuStopReceived pdu)

    -- TODO check values
    let diag = Nothing

    -- send response 
    let ret = SLEPdu $ SlePduAck $ SleAcknowledgement
            { _sleAckCredentials = pdu ^. sleStopCredentials
            , _sleAckInvokeID    = pdu ^. sleStopInvokeID
            , _sleResult         = diag
            }
    sendSleFcltuPdu var ret
    sleRaiseEvent (SLEFcltuStopSucceed (cfg ^. cfgFCLTUSII))
    return ServiceBound

processActiveState cfg var perfFunc (SlePduFcltuTransferData pdu) = do
    logDebug "processActiveState: FCLTU TRANSFER DATA"

    -- update the statistics
    liftIO $ perfFunc (fromIntegral (B.length (pdu ^. fcltuData)))

    -- TODO check values
    let diag       = Nothing
        !cltuID    = pdu ^. fcltuDataIdent
        !newCltuID = cltuID + 1

    -- set the CLTU ID in the state 
    void $ setCltuID var cltuID

    -- send response 
    let ret = SLEPdu $ SlePduFcltuTransReturn $ FcltuTransferDataReturn
            { _fcltuTransRetCredentials     = pdu ^. fcltuDataCredentials
            , _fcltuTransRetInvokeID        = pdu ^. fcltuDataInvokeID
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


processActiveState _cfg _var _perfFunc pdu = do
    logWarn
        $  "Active State: Functionality for PDU not yet implemented: "
        <> fromString (ppShow pdu)
    return ServiceActive


