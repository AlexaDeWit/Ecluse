-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Pipeline.PublishSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Types (RequestHeaders, status201, status400, status401, status503)
import Network.Wai (Request (requestBodyLength, requestHeaders, requestMethod), RequestBodyLength (ChunkedBody), defaultRequest, pathInfo, setRequestBodyChunks)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), Session, request, runSession, setPath, srequest)
import Test.Hspec
import UnliftIO (timeout, wait, withAsync)

import Ecluse.Core.Credential (ClientCredential, bareCredential, mkSecret)
import Ecluse.Core.Registry (PublishRelayResponse (PublishRelayResponse))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (..), PublishRequestBody (..))
import Ecluse.Core.Registry.Npm.Adapter (npmPublish)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocToken))
import Ecluse.Core.Registry.Publish (PublishCodec (pcPrepareTarget, pcTargetReadPermille))
import Ecluse.Core.Registry.RubyGems.Credential (rubyOtpHeaders, rubyPublishCredential, rubyPublishExtras, rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouter)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned, withServeAdmission)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission, newByteAdmission, newByteAdmissionTuned, withByteAdmission)
import Ecluse.Core.Server.Admission.Meter (meterSnapshot)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (MountBinding (..), PublishDeps (..), RequestCtx (RequestCtx), ResponseAction (..), RouteAction (RouteAction), ServeRuntime (srAdmission, srMemoryMeter), runHandler)
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntime, testServeRuntimeWith)
import Ecluse.Test.Server.Mount (inertPackumentDeps)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "publication-specific credentials at a protected mount" $ do
        it "accepts the native raw key while the read mapping remains Basic" $
            withFixture Nothing $ \send observed -> do
                response <- send [("Authorization", "edge-key"), ("OTP", "123456")]
                simpleStatus response `shouldBe` status201
                readIORef observed `shouldReturn` [(Just (bareCredential (mkSecret "edge-key")), [("OTP", "123456")], "unchanged-native-body")]
        it "refuses unauthenticated requests before validation or relay" $
            withFixture Nothing $ \send observed -> do
                response <- send []
                simpleStatus response `shouldBe` status401
                readIORef observed `shouldReturn` []
        it "rejects duplicate OTP headers before relay" $
            withFixture Nothing $ \send observed -> do
                response <- send [("Authorization", "edge-key"), ("OTP", "one"), ("OTP", "two")]
                simpleStatus response `shouldBe` status400
                readIORef observed `shouldReturn` []
        it "substitutes a configured native target key only after edge authentication" $
            withFixture (Just (bareCredential (mkSecret "Bearer target-key"))) $ \send observed -> do
                response <- send [("Authorization", "edge-key")]
                simpleStatus response `shouldBe` status201
                readIORef observed `shouldReturn` [(Just (bareCredential (mkSecret "Bearer target-key")), [], "unchanged-native-body")]
    describe "target discovery at a protected mount" $
        it "holds the sum of its full-read chunks, not only the largest" $ do
            held <- newIORef Nothing
            withCodecFixture (discoveryPaying held) Nothing $ \send _ -> do
                response <- send [("Authorization", "edge-key")]
                simpleStatus response `shouldBe` status201
            readIORef held >>= (`shouldSatisfy` maybe False (>= scaleCharge discoveryPermille (3 * discoveryChunk)))
    around_ (timeout 10_000_000 >=> (`shouldBe` Just ())) $ describe "admission order at a protected mount" $ do
        it "reads the body under the byte gate alone, without a serve slot or memory charge" $
            withGatedFixture (const id) $ \runtime budget run observed -> do
                paused <- newEmptyMVar
                release <- newEmptyMVar
                drained <- newIORef False
                body <- uploadBody [(pass, "unchanged-"), (putMVar paused () >> takeMVar release, "native-body")] drained
                withAsync (run (request (gemPush body))) $ \publish -> do
                    takeMVar paused
                    withServeAdmission noopMetricsPort (srAdmission runtime) pass `shouldReturn` Right ()
                    snChargedBytes <$> meterSnapshot (srMemoryMeter runtime) `shouldReturn` 0
                    withByteAdmission noopMetricsPort budget 1 pass `shouldReturn` Nothing
                    putMVar release ()
                    simpleStatus <$> wait publish `shouldReturn` status201
                readIORef observed `shouldReturn` [(Just (bareCredential (mkSecret "edge-key")), [], "unchanged-native-body")]
        it "holds the serve slot and a memory charge through inspection, discovery and relay" $ do
            held <- newIORef []
            withGatedFixture (probingGates held) $ \_ _ run _ -> do
                body <- uploadBody [(pass, "unchanged-native-body")] =<< newIORef False
                simpleStatus <$> run (request (gemPush body)) `shouldReturn` status201
            readIORef held `shouldReturn` replicate 3 (True, True)
        it "reads the whole body before a full serve gate refuses with 503 and Retry-After" $
            withGatedFixture (const id) $ \runtime _ run observed -> do
                drained <- newIORef False
                body <- uploadBody [(pass, "unchanged-native-body")] drained
                response <- expectRight =<< withServeAdmission noopMetricsPort (srAdmission runtime) (run (request (gemPush body)))
                simpleStatus response `shouldBe` status503
                lookup "Retry-After" (simpleHeaders response) `shouldBe` Just "1"
                simpleBody response `shouldBe` "server is busy; retry later"
                readIORef drained `shouldReturn` True
                readIORef observed `shouldReturn` []
  where
    withFixture = withCodecFixture (const id)

-- The credential, native headers and body of each relayed publication.
type Relayed = (Maybe ClientCredential, RequestHeaders, ByteString)

-- A mount whose admission never binds, for the credential and discovery cases.
withCodecFixture :: (ServeRuntime -> AdapterPublish -> AdapterPublish) -> Maybe ClientCredential -> ((RequestHeaders -> IO SResponse) -> IORef [Relayed] -> IO a) -> IO a
withCodecFixture adjust static use = withSweeper $ \deadlines -> do
    runtime <- testServeRuntime deadlines noopMetricsPort
    budget <- newByteAdmission deadlines (1024 * 1024)
    withMount runtime budget adjust static $ \run ->
        use (\headers -> run (srequest (SRequest (setPath defaultRequest{requestMethod = "POST", requestHeaders = headers} "/api/v1/gems") "unchanged-native-body")))

-- One serve slot and a body budget of one chunked upload, neither with a waiting room, so a held gate refuses at once.
withGatedFixture :: (ServeRuntime -> AdapterPublish -> AdapterPublish) -> (ServeRuntime -> ByteAdmission -> (Session SResponse -> IO SResponse) -> IORef [Relayed] -> IO a) -> IO a
withGatedFixture adjust use = withSweeper $ \deadlines -> do
    admission <- newServeAdmissionTuned deadlines 1 0 0
    runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
    budget <- newByteAdmissionTuned deadlines 1024 0 0
    withMount runtime budget adjust Nothing (use runtime budget)

-- A RubyGems publish mount over stub inspection and relay. The relay records what it sends and answers 201.
withMount :: ServeRuntime -> ByteAdmission -> (ServeRuntime -> AdapterPublish -> AdapterPublish) -> Maybe ClientCredential -> ((Session SResponse -> IO SResponse) -> IORef [Relayed] -> IO a) -> IO a
withMount runtime budget adjust static use = do
    logEnv <- newTestLogEnv
    observed <- newIORef []
    let publish =
            adjust
                runtime
                npmPublish
                    { publishCredential = rubyPublishCredential
                    , publishRequestExtras = rubyPublishExtras
                    , publishUploadClaims = \_ _ _ _ -> pure (Right ["widget"])
                    , publishRelay = \origin _ body -> do
                        modifyIORef' observed (<> [(ocToken origin, rubyOtpHeaders (publishBodyExtras body), publishBodyBytes body)])
                        pure (Right (PublishRelayResponse 201 "created"))
                    }
        deps =
            PublishDeps
                { pubReadUrl = loopbackRegistryUrl "http://localhost:1"
                , pubTargetUrl = loopbackRegistryUrl "http://localhost:1"
                , pubAllowed = const True
                , pubStaticCredential = static
                , pubInboundToken = Just (mkSecret "edge-key")
                , pubMaxRequestBytes = 1024
                , pubLimits = defaultLimits
                , pubBodyBudget = budget
                , pubHelp = Nothing
                , pubProjectName = rightToMaybe . projectName
                , pubAdapter = publish
                }
        binding = MountBinding ("ruby" :| []) rubyGemsRouter rubyReadCredential inertPackumentDeps (Just deps)
        app incoming respond = case bindingRouter binding (requestMethod incoming) (requestHeaders incoming) (pathInfo incoming) of
            RouteAction routeContract action -> case action of
                AnswerLocally answer -> respond (responseToWai routeContract answer)
                AnswerRefusal answer -> respond (responseToWai routeContract (answer Nothing))
                RunPipeline _ run -> runHandler logEnv mempty (RequestCtx runtime binding) (run incoming (respond . responseToWai routeContract))
    use (`runSession` app) observed

-- An authenticated gem push of undeclared length, which reserves the whole body cap.
gemPush :: IO ByteString -> Request
gemPush body =
    setRequestBodyChunks body (setPath defaultRequest{requestMethod = "POST", requestHeaders = [("Authorization", "edge-key")], requestBodyLength = ChunkedBody} "/api/v1/gems")

-- Run each action before sending its chunk, then end the body and record that it drained.
uploadBody :: [(IO (), ByteString)] -> IORef Bool -> IO (IO ByteString)
uploadBody chunks drained = do
    remaining <- newIORef chunks
    pure $
        readIORef remaining >>= \case
            [] -> writeIORef drained True $> ""
            (ahead, chunk) : rest -> ahead >> writeIORef remaining rest $> chunk

-- Record at inspection, target preparation and relay whether the one serve slot is taken and the meter holds a charge.
probingGates :: IORef [(Bool, Bool)] -> ServeRuntime -> AdapterPublish -> AdapterPublish
probingGates held runtime publish =
    publish
        { publishUploadClaims = \limits workspace registry body -> probe >> publishUploadClaims publish limits workspace registry body
        , publishCodec = codec{pcPrepareTarget = \origin target -> probe >> pcPrepareTarget codec origin target}
        , publishRelay = \origin name body -> probe >> publishRelay publish origin name body
        }
  where
    codec = publishCodec publish
    probe = do
        slotTaken <- isLeft <$> withServeAdmission noopMetricsPort (srAdmission runtime) pass
        charged <- (> 0) . snChargedBytes <$> meterSnapshot (srMemoryMeter runtime)
        modifyIORef' held (<> [(slotTaken, charged)])

-- Pay three equal chunks, each large enough that one chunk and the entry step stay under their sum.
discoveryPaying :: IORef (Maybe Int) -> ServeRuntime -> AdapterPublish -> AdapterPublish
discoveryPaying held runtime publish =
    publish
        { publishCodec =
            codec
                { pcTargetReadPermille = discoveryPermille
                , pcPrepareTarget = \origin target -> do
                    replicateM_ 3 (ocChargeFullRead origin discoveryChunk)
                    meterSnapshot (srMemoryMeter runtime) >>= writeIORef held . Just . snChargedBytes
                    pure (Right target)
                }
        }
  where
    codec = publishCodec publish

discoveryChunk :: Int
discoveryChunk = 8 * 1024 * 1024

discoveryPermille :: Int
discoveryPermille = 1500
