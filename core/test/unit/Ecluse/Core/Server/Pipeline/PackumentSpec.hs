-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Coding negotiation, write-time admission and refusals through the request pipeline.
module Ecluse.Core.Server.Pipeline.PackumentSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (Value (String), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Network.HTTP.Types (Header, Method, methodGet, methodHead, status200, status403, status404, statusCode)
import Network.HTTP.Types.Header (RequestHeaders, hAcceptEncoding, hAuthorization, hContentEncoding, hContentLength, hETag, hIfNoneMatch, hVary)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders, requestMethod), Response, defaultRequest, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import System.IO.Error (mkIOError, resourceVanishedErrorType)
import Test.Hspec
import UnliftIO.Async (cancel, race_, wait, withAsync)
import UnliftIO.Exception (evaluate, impureThrow, throwIO)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.Npm.Route.Internal (npmPackumentContract, npmPackumentReplies)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Route (pypiRouter)
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MeterSettings (..), meterSnapshot, newMemoryMeter)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfKeptPermille, cfOutputPermille), MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (
    MountBinding (..),
    MountRouter,
    PackumentDeps (pdFirstParty, pdHelp, pdInboundToken, pdMetadata, pdMetadataCompression, pdMountBaseUrl, pdRules),
    RequestCtx (RequestCtx),
    ResponseAction (RunPipeline),
    RouteAction (RouteAction),
    ServeRuntime (srMemoryMeter),
    runHandler,
 )
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Encoding (MetadataCompression (GzipWhenAccepted, IdentityOnly))
import Ecluse.Core.Server.MetadataGzip (gzipBufferChargeBytes, gzipWorkspaceBytes)
import Ecluse.Core.Server.Pipeline.Internal (denialLabels, packumentServeDecision)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved), ReadFailure (ReadInvalid, ReadUnavailable))
import Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    firstPartyMissDecision,
    firstPartyMissReply,
    noSurvivors,
    servePackument,
 )
import Ecluse.Core.Server.Response (
    PackumentStatus (..),
    RejectReason (Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RetryAfter (RetryAfter),
    ServeDecision (Reject),
    Transience (WillResolve),
    mkHelpMessage,
    mkRefusal,
 )
import Ecluse.Core.Server.Response.Summary (detailedRejections, summarisedRejections)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics (CacheResult (Hit, Miss))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpAssembledCacheRequest, mpRuleEvalDuration))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (sriSha512Of, thingName, validSha1)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity, vsShasum), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndex, withFileKeys)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Handler (MissingFixtureResponse (MissingFixtureResponse), deliverServe, responseBytes, testServeRuntime, testServeRuntimeWith)
import Ecluse.Test.Server.Mount (npmServeDeps, pypiServeDeps, withPrivateBaseUrl)
import Ecluse.Test.Server.Response (reasonOf)
import Ecluse.Test.Wai (selfBaseUrl)

spec :: Spec
spec = do
    metadataGzipSpec
    listingWriteSpec
    noSurvivorsSpec
    firstPartyMissSpec

metadataGzipSpec :: Spec
metadataGzipSpec = describe "native metadata gzip over the live listing path" $
    for_ listings $ \listing -> describe (listingLabel listing) $ do
        it "gzips the identity listing's exact bytes for a client that accepts gzip" $
            withListing listing $ \serve -> do
                plain <- serve GzipWhenAccepted methodGet []
                packed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                map statusOf [plain, packed] `shouldBe` [200, 200]
                header hContentEncoding plain `shouldBe` Nothing
                header hContentEncoding packed `shouldBe` Just "gzip"
                map (header hVary) [plain, packed] `shouldBe` [Just "Accept-Encoding", Just "Accept-Encoding"]
                plainBytes <- responseBytes plain
                packedBytes <- responseBytes packed
                GZip.decompress packedBytes `shouldBe` plainBytes
                header hContentLength packed `shouldBe` Just (show (LBS.length packedBytes))
                header hETag packed `shouldSatisfy` maybe False (BS.isPrefixOf "W/")
                header hETag plain `shouldSatisfy` maybe False (BS.isPrefixOf "\"")
        it "states a gzip HEAD's exact length and validator without a body" $
            withListing listing $ \serve -> do
                packed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                headed <- serve GzipWhenAccepted methodHead [(hAcceptEncoding, "gzip")]
                statusOf headed `shouldBe` 200
                responseBytes headed `shouldReturn` ""
                packedBytes <- responseBytes packed
                header hContentLength headed `shouldBe` Just (show (LBS.length packedBytes))
                map (header hETag) [headed, packed] `shouldBe` replicate 2 (header hETag packed)
                header hContentEncoding headed `shouldBe` Just "gzip"
        it "answers 304 only to the selected coding's validator" $
            withListing listing $ \serve -> do
                plainTag <- validator =<< serve GzipWhenAccepted methodGet []
                gzipTag <- validator =<< serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                for_ [methodGet, methodHead] $ \method -> do
                    matched <- serve GzipWhenAccepted method [(hAcceptEncoding, "gzip"), (hIfNoneMatch, gzipTag)]
                    statusOf matched `shouldBe` 304
                    responseBytes matched `shouldReturn` ""
                    header hETag matched `shouldBe` Just gzipTag
                    header hVary matched `shouldBe` Just "Accept-Encoding"
                crossed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip"), (hIfNoneMatch, plainTag)]
                (statusOf crossed, header hContentEncoding crossed) `shouldBe` (200, Just "gzip")
                unpacked <- serve GzipWhenAccepted methodGet [(hIfNoneMatch, gzipTag)]
                (statusOf unpacked, header hContentEncoding unpacked) `shouldBe` (200, Nothing)
        it "refuses with 406 a request that excludes both codings" $
            withListing listing $ \serve -> do
                refused <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip;q=0, identity;q=0")]
                statusOf refused `shouldBe` 406
                header hVary refused `shouldBe` Just "Accept-Encoding"
        it "keeps the identity response unchanged when the operator opts out" $
            withListing listing $ \serve -> do
                plain <- serve IdentityOnly methodGet []
                plainBytes <- responseBytes plain
                for_ [[(hAcceptEncoding, "gzip")], [(hAcceptEncoding, "*;q=0")]] $ \offered -> do
                    response <- serve IdentityOnly methodGet offered
                    statusOf response `shouldBe` 200
                    responseHeaders response `shouldBe` responseHeaders plain
                    responseBytes response `shouldReturn` plainBytes
                header hVary plain `shouldBe` Nothing
        it "keeps the strong identity validator and no Vary on GET, HEAD and 304 when the operator opts out" $
            withListing listing $ \serve -> do
                plain <- serve IdentityOnly methodGet gzipOffer
                tag <- validator plain
                tag `shouldSatisfy` BS.isPrefixOf "\""
                headed <- serve IdentityOnly methodHead gzipOffer
                plainBytes <- responseBytes plain
                header hContentLength headed `shouldBe` Just (show (LBS.length plainBytes))
                matched <- forM [methodGet, methodHead] $ \method -> serve IdentityOnly method ((hIfNoneMatch, tag) : gzipOffer)
                map statusOf matched `shouldBe` [304, 304]
                for_ (plain : headed : matched) $ \response -> do
                    header hETag response `shouldBe` Just tag
                    header hContentEncoding response `shouldBe` Nothing
                    header hVary response `shouldBe` Nothing
        it "serves both codings from one identity entry in the assembled cache" $ withSweeper $ \deadlines -> do
            results <- newIORef []
            rt <- testServeRuntime deadlines noopMetricsPort{mpAssembledCacheRequest = \result -> modifyIORef' results (result :)}
            withListingIn rt listing $ \serve -> do
                packed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                plain <- serve GzipWhenAccepted methodGet []
                plainBytes <- responseBytes plain
                (GZip.decompress <$> responseBytes packed) `shouldReturn` plainBytes
                reverse <$> readIORef results `shouldReturn` [Miss, Hit]
        it "charges a gzip response its workspace and buffer, and returns every charge" $ withSweeper $ \deadlines -> do
            meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 1000, msEntryWaitMicros = 0}
            unmetered <- testServeRuntime deadlines noopMetricsPort
            testWithApplication (pure (listingOrigin listing)) $ \port -> do
                deps <- listingDeps listing port
                -- The reads' charge is gone by the time the body is sent, so each response's own charge is what it holds then.
                let chargeWhileSending headers = do
                        held <- newIORef 0
                        runListing unmetered{srMemoryMeter = meter} listing deps methodGet headers (\_ -> chargedOn meter >>= writeIORef held)
                        readIORef held
                void (chargeWhileSending [])
                plainCharge <- chargeWhileSending []
                gzipCharge <- chargeWhileSending [(hAcceptEncoding, "gzip")]
                gzipCharge - plainCharge `shouldBe` gzipWorkspaceBytes + gzipBufferChargeBytes
                snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
        it "keeps edge authentication, private authorisation and policy ahead of a cached gzip validator" $ withSweeper $ \deadlines -> do
            rt <- testServeRuntime deadlines noopMetricsPort
            privateState <- newIORef PrivateServes
            testWithApplication (pure (listingOrigin listing)) $ \publicPort ->
                testWithApplication (pure (privateOrigin privateState (listingOrigin listing))) $ \privatePort -> do
                    public <- listingDeps listing publicPort
                    let publicOnly = public{pdInboundToken = Just (mkSecret "edge-token")}
                        withPrivate = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) publicOnly
                        serve deps headers = captureListing rt listing deps methodGet (gzipOffer <> headers)
                        signed = [listingEdgeCredential listing]
                    tag <- validator =<< serve withPrivate signed
                    statusOf <$> serve withPrivate [(hIfNoneMatch, tag)] `shouldReturn` 401
                    statusOf <$> serve withPrivate ((hIfNoneMatch, tag) : signed) `shouldReturn` 304
                    writeIORef privateState PrivateRefuses
                    statusOf <$> serve withPrivate ((hIfNoneMatch, tag) : signed) `shouldReturn` 403
                    publicTag <- validator =<< serve publicOnly signed
                    statusOf <$> serve publicOnly ((hIfNoneMatch, publicTag) : signed) `shouldReturn` 304
                    statusOf <$> serve publicOnly{pdRules = []} ((hIfNoneMatch, publicTag) : signed) `shouldReturn` 404
        it "returns its admission slot and charges when cancelled while it responds" $ withSweeper $ \deadlines -> do
            admission <- newServeAdmissionTuned deadlines 1 0 0
            rt <- testServeRuntimeWith deadlines admission noopMetricsPort
            testWithApplication (pure (listingOrigin listing)) $ \port -> do
                deps <- listingDeps listing port
                ready <- MVar.newEmptyMVar
                blocked <- MVar.newEmptyMVar
                let stalled = runListing rt listing deps methodGet [(hAcceptEncoding, "gzip")] (\_ -> MVar.putMVar ready () >> MVar.takeMVar blocked)
                withAsync stalled $ \worker -> do
                    race_ (MVar.takeMVar ready) (wait worker)
                    cancel worker
                snChargedBytes <$> meterSnapshot (srMemoryMeter rt) `shouldReturn` 0
                next <- captureListing rt listing deps methodGet [(hAcceptEncoding, "gzip")]
                statusOf next `shouldBe` 200
        it "holds what its reads keep through policy, and only its output charge while it responds" $ withSweeper $ \deadlines -> do
            meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 1000, msEntryWaitMicros = 0}
            atGate <- newIORef Nothing
            unmetered <- testServeRuntime deadlines noopMetricsPort{mpRuleEvalDuration = \_ _ -> chargedOn meter >>= writeIORef atGate . Just}
            privateSent <- newIORef 0
            publicSent <- newIORef 0
            testWithApplication (pure (measuredOrigin publicSent (listingOrigin listing))) $ \publicPort ->
                testWithApplication (pure (measuredOrigin privateSent (listingOrigin listing))) $ \privatePort -> do
                    public <- listingDeps listing publicPort
                    let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) public{pdMetadataCompression = IdentityOnly}
                        factors = metadataChargeFactors (pdMetadata deps)
                    atRespond <- newIORef Nothing
                    runListing unmetered{srMemoryMeter = meter} listing deps methodGet [] (\_ -> chargedOn meter >>= writeIORef atRespond . Just)
                    sizes <- traverse readIORef [privateSent, publicSent]
                    readIORef atGate `shouldReturn` Just (sum (map (scaleCharge (cfKeptPermille factors)) sizes))
                    readIORef atRespond `shouldReturn` Just (scaleCharge (cfOutputPermille factors) (foldl' max 0 sizes))
                    chargedOn meter `shouldReturn` 0
  where
    header name = lookup name . responseHeaders
    validator response = maybe (throwIO MissingFixtureResponse) pure (header hETag response)
    gzipOffer = [(hAcceptEncoding, "gzip")]

-- What the private origin answers for the listing.
data PrivateState = PrivateServes | PrivateRefuses

chargedOn :: MemoryMeter -> IO Int
chargedOn meter = snChargedBytes <$> meterSnapshot meter

-- The origin's answers unchanged, recording the length of the last body it sent.
measuredOrigin :: IORef Int -> Application -> Application
measuredOrigin sent origin request respond = origin request $ \response -> do
    body <- responseBytes response
    writeIORef sent (fromIntegral (LBS.length body))
    respond (responseLBS (responseStatus response) (responseHeaders response) body)

privateOrigin :: IORef PrivateState -> Application -> Application
privateOrigin current origin request respond =
    readIORef current >>= \case
        PrivateServes -> origin request respond
        PrivateRefuses -> respond (responseLBS status403 [] "")

listingWriteSpec :: Spec
listingWriteSpec = describe "a listing whose body is being written" $
    for_ listings $ \listing -> describe (listingLabel listing) $ do
        it "has freed its CPU slot and still holds its output charge" $
            withOneSlot listing $ \rt deps sourceBytes -> do
                observed <- newIORef Nothing
                runListing rt listing deps methodGet [] $ \_ -> do
                    held <- chargedNow rt
                    beside <- captureListing rt listing deps methodGet []
                    writeIORef observed (Just (held, statusCode (responseStatus beside)))
                output <- scaleCharge (cfOutputPermille (metadataChargeFactors (pdMetadata deps))) <$> sourceBytes
                readIORef observed `shouldReturn` Just (output, 200)
                chargedNow rt `shouldReturn` 0
        it "evaluates its reply in the CPU slot, before the write" $
            withOneSlot listing $ \rt deps _ -> do
                logEnv <- newTestLogEnv
                let replies = namedReplies{packumentOk = \_ _ -> impureThrow Evaluated}
                    binding = MountBinding (listingPrefix listing :| []) (listingRouter listing) (listingCredential listing) deps Nothing
                runHandler logEnv mempty (RequestCtx rt binding) (servePackument replies (listingName listing) defaultRequest (\_ -> pure ResponseReceived))
                    `shouldThrow` (== Evaluated)
        it "returns its charge when the client goes away mid-body, and leaves the slot free" $
            withOneSlot listing $ \rt deps sourceBytes -> do
                held <- newIORef 0
                runListing rt listing deps methodGet [] (\_ -> chargedNow rt >>= writeIORef held >> throwIO clientGone)
                    `shouldThrow` (== clientGone)
                output <- scaleCharge (cfOutputPermille (metadataChargeFactors (pdMetadata deps))) <$> sourceBytes
                readIORef held `shouldReturn` output
                chargedNow rt `shouldReturn` 0
                statusCode . responseStatus <$> captureListing rt listing deps methodGet [] `shouldReturn` 200
  where
    chargedNow rt = snChargedBytes <$> meterSnapshot (srMemoryMeter rt)
    clientGone = mkIOError resourceVanishedErrorType "the client closed the connection" Nothing Nothing

-- One CPU slot with no waiting room, and a meter that charges each byte.
withOneSlot :: Listing -> (ServeRuntime -> PackumentDeps -> IO Int -> IO a) -> IO a
withOneSlot listing use = withSweeper $ \deadlines -> do
    admission <- newServeAdmissionTuned deadlines 1 0 0
    rt <- testServeRuntimeWith deadlines admission noopMetricsPort
    meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 1000, msEntryWaitMicros = 0}
    sourceBytes <- newIORef 0
    testWithApplication (pure (measuredOrigin sourceBytes (listingOrigin listing))) $ \port -> do
        deps <- listingDeps listing port
        use rt{srMemoryMeter = meter} deps (readIORef sourceBytes)

statusOf :: Response -> Int
statusOf = statusCode . responseStatus

noSurvivorsSpec :: Spec
noSurvivorsSpec = describe "the refusal for a listing no version survived" $ do
    it "joins detailed rejection texts as soon as its reply is evaluated" $
        evaluate (noSurvivors namedReplies Nothing PackumentForbidden [Reject (Rejection UpstreamInvalid (impureThrow Evaluated))])
            `shouldThrow` (== Evaluated)

    it "finishes grouped rejection text before an ignoring reply constructor returns" $
        evaluate (noSurvivors namedReplies{packumentForbiddenText = summarisedRejections} Nothing PackumentForbidden (Reject (Rejection UpstreamInvalid "detail") : impureThrow Evaluated))
            `shouldThrow` (== Evaluated)

    for_ [(PackumentOk, 500), (PackumentUnavailable Nothing, 503), (PackumentBadGateway, 502), (PackumentServerError, 500)] $ \(status, code) ->
        it ("keeps the npm error contract under " <> show status) $ do
            let response = responseToWai npmPackumentContract (noSurvivors npmPackumentReplies (Just (mkHelpMessage "Help.")) status [Reject (Rejection UpstreamInvalid "full upstream detail")])
            statusOf response `shouldBe` code
            responseBytes response `shouldReturn` encode (object ["error" .= ("full upstream detail Help." :: Text)])

    it "keeps private-access refusal text outside the grouped metadata response" $ do
        let response = responseToWai npmPackumentContract (packumentForbidden npmPackumentReplies [] (mkRefusal (Just (mkHelpMessage "Help.")) "private access was refused"))
        statusOf response `shouldBe` 403
        responseBytes response `shouldReturn` encode (object ["error" .= ("private access was refused Help." :: Text)])

    for_ [(Nothing, "MissingIntegrity: artifacts lack an integrity digest"), (Just validSha1, "BelowIntegrityFloor: artifact digests fall below the configured integrity floor")] $ \(shasum, cause) ->
        for_ (filter ((== "npm") . listingLabel) listings) $ \listing ->
            it ("summarises a private-only integrity refusal without claiming a public source: " <> toString (cause :: Text)) $
                withSweeper $ \deadlines -> do
                    rt <- testServeRuntime deadlines noopMetricsPort
                    let privateIntegrityOrigin = npmOriginWith (\version -> version{vsIntegrity = Nothing, vsShasum = shasum})
                    testWithApplication (pure privateIntegrityOrigin) $ \privatePort -> do
                        original <- listingDeps listing 1
                        let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) original{pdFirstParty = const True, pdHelp = Just (mkHelpMessage "Contact the operator.")}
                        denied <- captureListing rt listing deps methodGet []
                        headed <- captureListing rt listing deps methodHead []
                        map statusOf [denied, headed] `shouldBe` [404, 404]
                        responseBytes headed `shouldReturn` ""
                        responseBytes denied `shouldReturn` encode (object ["error" .= (cause <> " (rejection entries: 1) Contact the operator.")])

    for_ listings $ \listing -> do
        it ("preserves the " <> listingLabel listing <> " denial body and HEAD status through its route") $
            withSweeper $ \deadlines -> do
                rt <- testServeRuntime deadlines noopMetricsPort
                prepared <- prepare inertRuleDeps []
                testWithApplication (pure (listingOrigin listing)) $ \port -> do
                    original <- listingDeps listing port
                    let deps = original{pdRules = prepared, pdHelp = Just (mkHelpMessage "Contact the operator.")}
                    denied <- captureListing rt listing deps methodGet []
                    headed <- captureListing rt listing deps methodHead []
                    map statusOf [denied, headed] `shouldBe` [404, 404]
                    responseBytes headed `shouldReturn` ""
                    bytes <- responseBytes denied
                    if listingLabel listing == "npm"
                        then bytes `shouldBe` encode (object ["error" .= ("BlockedByDefault: no allow rule matched (rejection entries: 1) Contact the operator." :: Text)])
                        else bytes `shouldBe` "Contact the operator."

-- Raised by a value a spec expects the pipeline to evaluate before it writes.
data Evaluated = Evaluated
    deriving stock (Eq, Show)

instance Exception Evaluated

firstPartyMissSpec :: Spec
firstPartyMissSpec = describe "a first-party name whose private origin yielded nothing" $ do
    it "renders an origin that answered 404 as a 404" $
        replyFor MissAbsent `shouldBe` "not-found"

    it "renders an origin that was never read as a 503" $
        replyFor (MissUnresolved (ReadUnavailable Nothing)) `shouldBe` "unavailable"

    it "renders an origin whose reply was invalid as a 502" $
        replyFor (MissUnresolved ReadInvalid) `shouldBe` "bad-gateway"

    it "counts an absence as a denial the first-party rule decided" $ do
        packumentServeDecision [decisionFor MissAbsent] `shouldBe` Metric.Deny
        fmap denialLabels (reasonOf (decisionFor MissAbsent))
            `shouldBe` Just (Just "first-party", Metric.ReasonPolicy)

    it "counts an unread origin as an outage, carrying only the delay the origin asked for" $ do
        packumentServeDecision [decisionFor (MissUnresolved (ReadUnavailable Nothing))] `shouldBe` Metric.Unavailable
        reasonOf (decisionFor (MissUnresolved (ReadUnavailable Nothing))) `shouldBe` Just (Unavailable (WillResolve Nothing))
        reasonOf (decisionFor (MissUnresolved (ReadUnavailable (Just (RetryAfter 30))))) `shouldBe` Just (Unavailable (WillResolve (Just (RetryAfter 30))))

    it "counts an invalid private reply as an invalid upstream" $ do
        packumentServeDecision [decisionFor (MissUnresolved ReadInvalid)] `shouldBe` Metric.Unavailable
        reasonOf (decisionFor (MissUnresolved ReadInvalid)) `shouldBe` Just UpstreamInvalid

-- Each reply factory answers its own name, so a case reads back which one the pipeline chose.
namedReplies :: PackumentReplies Text
namedReplies =
    PackumentReplies
        { packumentForbiddenText = detailedRejections
        , packumentOk = \_ _ -> "ok"
        , packumentNotModified = const "not-modified"
        , packumentUnauthorised = \_ _ -> "unauthorised"
        , packumentPrivateRefused = \_ _ _ -> "private-refused"
        , packumentForbidden = \_ _ -> "forbidden"
        , packumentNotFound = \_ _ -> "not-found"
        , packumentNotAcceptable = \_ _ -> "not-acceptable"
        , packumentInternal = \_ _ -> "internal"
        , packumentBadGateway = \_ _ -> "bad-gateway"
        , packumentUnavailable = \_ _ -> "unavailable"
        }

replyFor :: OriginMiss -> Text
replyFor = firstPartyMissReply namedReplies Nothing thingName

decisionFor :: OriginMiss -> ServeDecision
decisionFor = firstPartyMissDecision thingName

-- One ecosystem's listing route, mount, and the public origin that serves its document.
data Listing = Listing
    { listingLabel :: String
    , listingName :: PackageName
    , listingRouter :: MountRouter
    , listingPath :: [Text]
    , listingPrefix :: Text
    , listingCredential :: CredentialMapping
    , listingEdgeCredential :: Header
    -- ^ The edge token @edge-token@ in the ecosystem's own presentation.
    , listingDeps :: Int -> IO PackumentDeps
    , listingOrigin :: Application
    }

listings :: [Listing]
listings =
    [ Listing "npm" (mkPackageName Npm Nothing "leftpad") npmRouter ["leftpad"] "npm" npmCredential (hAuthorization, "Bearer edge-token") (agedDeps npmServeDeps (UTCTime (fromGregorian 2020 1 1) 0)) npmOrigin
    , -- HTTP Basic, user:edge-token.
      Listing "PyPI" (mkPackageName PyPI Nothing "requests") pypiRouter ["simple", "requests"] "pypi" pypiCredential (hAuthorization, "Basic dXNlcjplZGdlLXRva2Vu") (agedDeps pypiServeDeps (UTCTime (fromGregorian 2026 6 20) 0)) pypiOrigin
    ]
  where
    agedDeps serveDeps now port = do
        prepared <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
        pure (serveDeps Nothing (loopbackRegistryUrl ("http://localhost:" <> show port)) NoMirrorWrite prepared (pure now)){pdMountBaseUrl = "http://proxy.test"}

npmOrigin :: Application
npmOrigin = npmOriginWith id

npmOriginWith :: (VersionSpec -> VersionSpec) -> Application
npmOriginWith changeVersion request respond
    | rawPathInfo request == "/leftpad" = respond (responseLBS status200 [] (encode document))
    | otherwise = respond (responseLBS status404 [] "")
  where
    tarball = selfBaseUrl request <> "/leftpad/-/leftpad-1.0.0.tgz"
    document = packumentValue "leftpad" "1.0.0" [("1.0.0", versionValue (changeVersion (versionSpec "leftpad" "1.0.0" tarball){vsIntegrity = Just (sriSha512Of "leftpad bytes")}))] ["1.0.0" .= ("2019-01-01T00:00:00.000Z" :: Text)] []

pypiOrigin :: Application
pypiOrigin request respond
    | rawPathInfo request == "/simple/requests/" = respond (responseLBS status200 [] (encode index))
    | otherwise = respond (responseLBS status404 [] "")
  where
    index = simpleIndex "requests" [withFileKeys [("url", String (selfBaseUrl request <> "/files/requests-1.0.0.tar.gz"))] (simpleFile "requests-1.0.0.tar.gz")]

-- Serve each request through a fresh runtime over one origin, choosing the mount's coding policy.
withListing :: Listing -> ((MetadataCompression -> Method -> RequestHeaders -> IO Response) -> IO a) -> IO a
withListing listing use = withSweeper $ \deadlines -> do
    rt <- testServeRuntime deadlines noopMetricsPort
    withListingIn rt listing use

withListingIn :: ServeRuntime -> Listing -> ((MetadataCompression -> Method -> RequestHeaders -> IO Response) -> IO a) -> IO a
withListingIn rt listing use =
    testWithApplication (pure (listingOrigin listing)) $ \port -> do
        deps <- listingDeps listing port
        use (\policy -> captureListing rt listing deps{pdMetadataCompression = policy})

captureListing :: ServeRuntime -> Listing -> PackumentDeps -> Method -> RequestHeaders -> IO Response
captureListing rt listing deps method headers = do
    captured <- newIORef Nothing
    runListing rt listing deps method headers (writeIORef captured . Just)
    maybe (throwIO MissingFixtureResponse) pure =<< readIORef captured

-- The listing's route under its own router, so a HEAD takes the bodiless contract.
runListing :: ServeRuntime -> Listing -> PackumentDeps -> Method -> RequestHeaders -> (Response -> IO ()) -> IO ()
runListing rt listing deps method headers deliver =
    case listingRouter listing method headers (listingPath listing) of
        RouteAction contract (RunPipeline _ action) -> do
            logEnv <- newTestLogEnv
            deliverServe logEnv contract rt binding (action request) deliver
        _ -> throwIO MissingFixtureResponse
  where
    binding = MountBinding (listingPrefix listing :| []) (listingRouter listing) (listingCredential listing) deps Nothing
    request = defaultRequest{requestMethod = method, requestHeaders = headers}
