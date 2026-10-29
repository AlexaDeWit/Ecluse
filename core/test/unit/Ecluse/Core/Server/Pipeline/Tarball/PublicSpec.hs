-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Public artifact admission and origin-failure observations.
module Ecluse.Core.Server.Pipeline.Tarball.PublicSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types (Status, mkStatus, status200, status401, status403, status404, status429, status500, status503, statusCode)
import Network.Wai (Request (pathInfo), defaultRequest, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (..), runSession)
import Network.Wai.Test qualified as Wai
import Test.Hspec

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
 )
import Ecluse.Core.Queue (MirrorQueue (enqueue))
import Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Project (projectName, projectVersion)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouter)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (
    Decision (Blocked, Undecidable),
    Inability (EvaluationFailed),
    Reason (RuleUnable),
    Rule (AllowIfOlderThan),
    Transience (WillResolve, WontResolve),
 )
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Core.Server.Context (MountBinding (MountBinding), RequestCtx (RequestCtx), ServeRuntime (srQueue), runHandler)
import Ecluse.Core.Server.Contract (PassthroughBody (..), passthroughContract, passthroughResponse, responseToWai)
import Ecluse.Core.Server.Pipeline.Tarball.Public (
    PublicArtifactGate (Admitted, Refused),
    publicArtifactGate,
    servePublicArtifact,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactOutcomeStatus)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (ArtifactServe (ServeFull))
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Response (
    ArtifactStatus (Forbidden, NotFound, ServerError, Unavailable'),
    renderRefusal,
 )
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit))
import Ecluse.Core.Telemetry.Metrics (RelayAnomaly (RelayNonSuccess))
import Ecluse.Core.Telemetry.Record (MetricsPort (mpPublicRelayAnomaly))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (captureJsonLog, lineMessage)
import Ecluse.Test.Package (npmVersion, sampleDetails, thingName, unsafeFilename)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps, revocation)
import Ecluse.Test.Server.Handler (testServeRuntime)
import Ecluse.Test.Server.Mount (serveDepsFor)
import Ecluse.Test.Support (expectRight)

details :: PackageDetails
details = sampleDetails thingName (npmVersion "1.0.0")

statusOf :: ArtifactAdmission -> Maybe ArtifactStatus
statusOf admission = case publicArtifactGate details admission [] of
    Admitted{} -> Nothing
    Refused decision -> Just (artifactOutcomeStatus decision)

spec :: Spec
spec = describe "publicArtifactGate -- the shared admission verdict on the serve surface" $ do
    originFailureSpec
    it "renders an inability the evaluator expects to clear as a 503" $
        -- The transience comes from the shared projection, the one the worker reads to
        -- redeliver the job.
        statusOf (AdmissionUndecidable (Undecidable (WillResolve Nothing) (RuleUnable "DenyIfCve" EvaluationFailed)))
            `shouldBe` Just (Unavailable' Nothing)

    it "renders an inability no retry can clear as a 500, never as a forwarded 404" $
        -- The 404 belongs to a version the upstream does not have. An internal fault that
        -- reported one would tell a client the version does not exist.
        statusOf (AdmissionUndecidable (Undecidable WontResolve (RuleUnable "DenyIfCve" EvaluationFailed)))
            `shouldBe` Just ServerError

    it "renders a version whose admitted file the upstream no longer carries as a 404" $
        -- The forwarded miss, and the one refusal the 404 override covers.
        statusOf AdmissionFileAbsent `shouldBe` Just NotFound

    it "renders a policy denial as a 403" $
        statusOf (AdmissionDenied (Blocked "test-deny" Nothing revocation)) `shouldBe` Just Forbidden

    it "renders an artifact the integrity floor refuses as a 403" $ do
        statusOf AdmissionBelowFloor `shouldBe` Just Forbidden
        statusOf AdmissionIntegrityMissing `shouldBe` Just Forbidden

originFailureSpec :: Spec
originFailureSpec = describe "public origin outcomes" $ do
    for_ [(status404, status404), (status500, status500), (status401, status401), (status403, status403), (status429, status429), (status503, status503)] $ \(originStatus, clientStatus) ->
        it ("observes HTTP " <> show (statusCode originStatus) <> " once without mirror enqueue") $ do
            (response, observations, messages, attempts, archive, requests) <- publicOutcome originStatus
            simpleStatus response `shouldBe` clientStatus
            requests `shouldBe` 1
            observations `shouldBe` [RelayNonSuccess]
            let failures = filter (T.isInfixOf "the public upstream answered a non-success") messages
            length failures `shouldBe` 1
            failures `shouldSatisfy` all (T.isInfixOf ("HTTP " <> show (statusCode originStatus)))
            simpleBody response `shouldBe` fromStrict archive
            attempts `shouldBe` 0
    it "does not report a successful artifact as a non-success" $ do
        (response, observations, messages, attempts, archive, requests) <- publicOutcome status200
        simpleStatus response `shouldBe` status200
        requests `shouldBe` 1
        simpleBody response `shouldBe` fromStrict archive
        observations `shouldBe` []
        filter (T.isInfixOf "the public upstream answered a non-success") messages `shouldBe` []
        attempts `shouldBe` 1

publicOutcome :: Status -> IO (SResponse, [RelayAnomaly], [Text], Int, ByteString, Int)
publicOutcome originStatus = withSweeper $ \deadlines -> do
    listing <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-root"
    archive <- BS.readFile "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
    observations <- newIORef []
    requests <- newIORef (0 :: Int)
    attempts <- newIORef (0 :: Int)
    let source request respond = case pathInfo request of
            ["info", "ecluse-fixture-root"] -> respond (responseLBS status200 [] (fromStrict listing))
            ["gems", "ecluse-fixture-root-1.0.0.gem"] -> modifyIORef' requests (+ 1) >> respond (responseLBS originStatus [("Content-Type", "application/octet-stream")] (fromStrict archive))
            _ -> respond (responseLBS status404 [] "")
        metrics =
            noopMetricsPort
                { mpPublicRelayAnomaly = \verdict -> modifyIORef' observations (<> [verdict])
                }
    testWithApplication (pure source) $ \port -> do
        baseRuntime <- testServeRuntime deadlines metrics
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan 0)]
        name <- expectRight (projectName "ecluse-fixture-root")
        version <- expectRight (projectVersion "1.0.0")
        let queue = srQueue baseRuntime
            runtime = baseRuntime{srQueue = queue{enqueue = \job -> modifyIORef' attempts (+ 1) >> enqueue queue job}}
            adapter = rubyGemsAdapter (ChargeFactors 1000 1000 1000)
            origin = loopbackRegistryUrl ("http://localhost:" <> show port)
            deps = serveDepsFor adapter (Just (loopbackRegistryUrl "http://localhost:1")) origin (MirrorOnAdmit (loopbackRegistryUrl "http://localhost:2")) rules (pure (UTCTime (fromGregorian 2030 1 1) 0))
            binding = MountBinding ("rubygems" :| []) rubyGemsRouter rubyReadCredential deps Nothing
            contract = passthroughContract "The public artifact response."
            replies =
                TarballReplies
                    { tarballError = \status headers -> passthroughResponse status headers . PassthroughBytes . fromStrict . encodeUtf8 . renderRefusal
                    , tarballPrivateRefused = \code headers -> passthroughResponse (mkStatus code "") headers . PassthroughBytes . fromStrict . encodeUtf8 . renderRefusal
                    , tarballStream = \status headers -> passthroughResponse status headers . PassthroughStream
                    , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
                    }
        (response, logged) <- captureJsonLog $ \logEnv -> runSession (Wai.request defaultRequest) $ \_ respond ->
            runHandler
                logEnv
                mempty
                (RequestCtx runtime binding)
                ( servePublicArtifact
                    ArtifactRequest
                        { arMode = ServeFull
                        , arReplies = replies
                        , arRuntime = runtime
                        , arDeps = deps
                        , arValidators = []
                        , arPackage = name
                        , arVersion = version
                        , arFile = unsafeFilename "ecluse-fixture-root-1.0.0.gem"
                        , arRespond = respond . responseToWai contract
                        }
                )
        requestCount <- readIORef requests
        (response,,,,archive,requestCount) <$> readIORef observations <*> pure (mapMaybe lineMessage (T.lines logged)) <*> readIORef attempts
