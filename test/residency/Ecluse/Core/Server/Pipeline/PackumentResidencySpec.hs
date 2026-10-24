-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A heap census of the live listing path over corpus captures. Before the render, the meter covers
what the request holds. Once the body exists, the request no longer reaches its reads' documents.
-}
module Ecluse.Core.Server.Pipeline.PackumentResidencySpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Network.HTTP.Types (methodGet, status200, statusCode)
import Network.Wai (Application, defaultRequest, requestMethod, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Exception (evaluate, throwIO)

import Ecluse.Composition.MemoryPlan.Transient (meterStepBytes)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageName, pkgEcosystem, renderPackageName)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, meterSnapshot)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (MountBinding (bindingRouter), ResponseAction (RunPipeline), RouteAction (RouteAction), ServeRuntime (srMemoryMeter))
import Ecluse.Core.Server.MemoryModel.Calibration (Figures (readPeak, retainedShared))
import Ecluse.Core.Server.MemoryModel.HeapSample (HeapSample (live), sample)
import Ecluse.Core.Server.MemoryModel.Probe (Residency (residencyFigures, residencyUpstream), packages, residencyFor)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Record (MetricsPort (mpRuleEvalDuration, mpServeDecision))
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Corpus (CaptureUpstream (upstreamOrigin), CorpusPackage (cpPackage), cpName, readCapture)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Handler (MissingFixtureResponse (MissingFixtureResponse), deliverServe, testServeRuntime)
import Ecluse.Test.Server.Mount (serveDepsFor)
import Ecluse.Test.Support (testMemoryMeter)
import Ecluse.Test.Wai (rebaseAuthority, selfBaseUrl)

-- | Each capture of at least one meter step, read from a private and a public origin that both serve it.
spec :: Spec
spec = describe "listing heap census" $
    for_ censused $ \(package, record, figures) -> it (toString (cpName package)) $ do
        capture <- readCapture package
        census <- takeCensus package (upstreamOrigin (residencyUpstream record)) capture
        report package census
        censusStatus census `shouldBe` statusCode status200
        -- The control: the census sees a document while the request holds it.
        held census (censusGate census) `shouldSatisfy` (>= ceiling (retainedShared figures * toRational (BS.length capture)))
        for_ [censusGate census, censusPlanned census] $ \point ->
            held census point `shouldSatisfy` (<= toInteger (pointCharged point))
        censusAtRespond census - censusAfter census `shouldSatisfy` (<= toInteger requestState)

-- The request's own state while it responds, apart from the body: its frames, ticket and headers.
requestState :: Int
requestState = 64 * 1024

censused :: [(CorpusPackage, Residency, Figures)]
censused =
    [ (package, record, figures)
    | package <- packages
    , Just record <- [residencyFor (pkgEcosystem (cpPackage package))]
    , Just figures <- [Map.lookup (cpName package) (residencyFigures record)]
    , isJust (readPeak figures)
    ]

-- | Live bytes after major collections around one measured listing.
data Census = Census
    { censusStatus :: Int
    , censusBefore :: Integer
    , censusGate :: Point
    -- ^ Policy has decided every version.
    , censusPlanned :: Point
    -- ^ The merge is planned and the render has not taken its output charge.
    , censusAtRespond :: Integer
    , censusAfter :: Integer
    -- ^ After the request, while the test still holds the response it sent.
    }

-- | Live bytes and what the meter held at one point inside the request.
data Point = Point {pointLive :: Integer, pointCharged :: Int}

held :: Census -> Point -> Integer
held census point = pointLive point - censusBefore census

{- A warm-up listing settles one-off state first. The measured listing runs on a fresh cache, so it
renders its body instead of finding the warm-up's. -}
takeCensus :: CorpusPackage -> Text -> ByteString -> IO Census
takeCensus package captured capture = do
    logEnv <- newTestLogEnv
    rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
    gate <- newIORef Nothing
    planned <- newIORef Nothing
    responded <- newIORef Nothing
    adapter <- maybe (fail "census ecosystem has no adapter") pure (adapterFor (pkgEcosystem name))
    testWithApplication (servingCapture captured capture) $ \publicPort ->
        testWithApplication (servingCapture captured capture) $ \privatePort -> do
            -- Every version is old enough to pass, so the policy evaluates the whole document.
            let deps = serveDepsFor adapter (Just (origin privatePort)) (origin publicPort) NoMirrorWrite rules (pure (UTCTime (fromGregorian 2030 1 1) 0))
            binding <- maybe (fail "census ecosystem has no mount") pure (mountBindingFor (pkgEcosystem name) deps Nothing)
            warmUp <- testServeRuntime noopMetricsPort
            listing logEnv warmUp binding (const pass)
            meter <- testMemoryMeter
            measured <- testServeRuntime noopMetricsPort{mpRuleEvalDuration = \_ _ -> samplePoint meter gate, mpServeDecision = \_ -> samplePoint meter planned}
            atStart <- live <$> sample
            listing logEnv measured{srMemoryMeter = meter} binding $ \response -> do
                atRespond <- live <$> sample
                writeIORef responded (Just (response, atRespond))
            atEnd <- live <$> sample
            (response, atRespond) <- maybe (throwIO MissingFixtureResponse) pure =<< readIORef responded
            atGate <- maybe (throwIO MissingFixtureResponse) pure =<< readIORef gate
            atPlan <- maybe (throwIO MissingFixtureResponse) pure =<< readIORef planned
            pure
                Census
                    { censusStatus = statusCode (responseStatus response)
                    , censusBefore = toInteger atStart
                    , censusGate = atGate
                    , censusPlanned = atPlan
                    , censusAtRespond = toInteger atRespond
                    , censusAfter = toInteger atEnd
                    }
  where
    name = cpPackage package
    origin port = loopbackRegistryUrl ("http://localhost:" <> show port)
    listing logEnv rt binding deliver = case bindingRouter binding methodGet [] (listingPath name) of
        RouteAction contract (RunPipeline _ action) -> deliverServe logEnv contract rt binding (action defaultRequest{requestMethod = methodGet}) deliver
        _ -> throwIO MissingFixtureResponse

samplePoint :: MemoryMeter -> IORef (Maybe Point) -> IO ()
samplePoint meter point = do
    now <- live <$> sample
    charged <- snChargedBytes <$> meterSnapshot meter
    writeIORef point (Just (Point (toInteger now) charged))

listingPath :: PackageName -> [Text]
listingPath name = case pkgEcosystem name of
    Npm -> T.splitOn "/" (renderPackageName name)
    PyPI -> ["simple", renderPackageName name]
    RubyGems -> [renderPackageName name]

{- Serve the capture with its locations on this origin's own authority, as projection keeps no other.
The body is rebased once, so the origin holds the same bytes at every sample. -}
servingCapture :: Text -> ByteString -> IO Application
servingCapture captured capture = do
    rebased <- newIORef Nothing
    pure $ \request respond -> do
        body <- readIORef rebased >>= maybe (rebase request >>= \fresh -> fresh <$ writeIORef rebased (Just fresh)) pure
        respond (responseLBS status200 [] body)
  where
    authority = T.intercalate "/" (take 3 (T.splitOn "/" captured))
    rebase request = evaluate (rebaseAuthority authority (selfBaseUrl request) (LBS.fromStrict capture))

report :: CorpusPackage -> Census -> IO ()
report package census =
    putStrLn . ("listing-census " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "package" .= cpName package
            , "held_at_gate" .= held census (censusGate census)
            , "charged_at_gate" .= pointCharged (censusGate census)
            , "held_when_planned" .= held census (censusPlanned census)
            , "charged_when_planned" .= pointCharged (censusPlanned census)
            , "request_while_responding" .= (censusAtRespond census - censusAfter census)
            , "meter_step" .= meterStepBytes
            ]
