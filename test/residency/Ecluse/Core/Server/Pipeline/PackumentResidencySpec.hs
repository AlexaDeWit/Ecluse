-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Isolated heap censuses of the live listing path. The meter covers each phase, and the request
releases its source documents before writing the body.
-}
module Ecluse.Core.Server.Pipeline.PackumentResidencySpec (spec, Census, probeListingCensus) where

import Data.Aeson (FromJSON, ToJSON, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import Network.HTTP.Types (hContentType, methodGet, status200, statusCode)
import Network.Wai (Application, Request (..), defaultRequest, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Exception (bracket, evaluate, throwIO)

import Ecluse.Composition.MemoryPlan.Transient (meterStepBytes)
import Ecluse.Core.Deadline (Deadlines, sweepTick)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (PackageName, pkgEcosystem, renderPackageName)
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, meterSnapshot)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (MountBinding (bindingRouter), ResponseAction (RunPipeline), RouteAction (RouteAction), ServeRuntime (srMemoryMeter, srMetadataCache, srMetrics))
import Ecluse.Core.Server.MemoryModel.Calibration (Figures (readPeak, retainedShared))
import Ecluse.Core.Server.MemoryModel.HeapSample (HeapSample (live), sample)
import Ecluse.Core.Server.MemoryModel.Probe (Residency (residencyFigures, residencyUpstream), measureInChild, packages, requireResidency, residencyFor)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Record (MetricsPort (mpRuleEvalDuration, mpServeDecision))
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Corpus (CaptureUpstream (upstreamOrigin), CorpusPackage (cpPackage, cpPath), cpName, readCapture)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.Cargo.Fixtures (cargoShard, cargoViewDiscovery)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Test.Server.Handler (deliverServe, testServeRuntime)
import Ecluse.Test.Server.Mount (serveDepsFor)
import Ecluse.Test.Support (expectRight, testMemoryMeter)
import Ecluse.Test.Wai (rebaseAuthority, selfBaseUrl)

-- | Censuses use each capture with a recorded listing peak, from private and public origins.
spec :: Spec
spec = describe "listing heap census" $
    for_ censused $ \(package, figures) -> it (toString (cpName package)) $ do
        capture <- readCapture package
        census <- measureInChild Mode.ListingCensus package >>= expectRight
        report package capture census
        censusStatus census `shouldBe` statusCode status200
        -- The control: the census sees a document while the request holds it.
        held census (censusGate census) `shouldSatisfy` (>= ceiling (retainedShared figures * toRational (BS.length capture)))
        for_ [censusGate census, censusPlanned census] $ \point ->
            held census point `shouldSatisfy` (<= toInteger (pointCharged point))
        censusAtRespond census - censusAfter census `shouldSatisfy` (<= toInteger requestState)

-- The request's own state while it responds, apart from the body: its frames, ticket and headers.
requestState :: Int
requestState = 64 * 1024

censused :: [(CorpusPackage, Figures)]
censused =
    [ (package, figures)
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
    deriving stock (Generic)

instance FromJSON Census
instance ToJSON Census

-- Live bytes and what the meter held at one point inside the request.
data Point = Point {pointLive :: Integer, pointCharged :: Int}
    deriving stock (Generic)

instance FromJSON Point
instance ToJSON Point

held :: Census -> Point -> Integer
held census point = pointLive point - censusBefore census

-- | Measure one authenticated capture with no previous example's pools or caches in the process.
probeListingCensus :: CorpusPackage -> IO Census
probeListingCensus package = do
    record <- requireResidency (pkgEcosystem (cpPackage package))
    capture <- readCapture package
    takeCensus package (upstreamOrigin (residencyUpstream record)) capture

-- A fresh cache forces the measured request to read and render over the warmed connection pools.
takeCensus :: CorpusPackage -> Text -> ByteString -> IO Census
takeCensus package captured capture = withSweeper $ \deadlines -> do
    logEnv <- newTestLogEnv
    rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
    gate <- newIORef Nothing
    planned <- newIORef Nothing
    responded <- newIORef Nothing
    adapter <- maybe (fail "census ecosystem has no adapter") pure (adapterFor (pkgEcosystem name))
    testWithApplication (servingCapture (pkgEcosystem name) captured capture) $ \publicPort ->
        testWithApplication (servingCapture (pkgEcosystem name) captured capture) $ \privatePort -> do
            -- Every version is old enough to pass, so the policy evaluates the whole document.
            let deps = serveDepsFor adapter (Just (origin privatePort)) (origin publicPort) NoMirrorWrite rules (pure (UTCTime (fromGregorian 2030 1 1) 0))
            binding <- maybe (fail "census ecosystem has no mount") pure (mountBindingFor (pkgEcosystem name) deps Nothing Nothing)
            warmUp <- testServeRuntime deadlines noopMetricsPort
            listing logEnv warmUp binding (const pass)
            meter <- testMemoryMeter
            cache <- newMetadataCache defaultCacheConfig
            let measured = warmUp{srMetadataCache = cache, srMetrics = noopMetricsPort{mpRuleEvalDuration = \_ _ -> samplePoint deadlines meter gate, mpServeDecision = \_ -> samplePoint deadlines meter planned}}
            -- The process keeps its runtime alive. Its pools and caches must outlive every sample too.
            bracket (newStablePtr (warmUp, measured)) freeStablePtr $ \root -> do
                atStart <- live <$> sampleAfterSweep deadlines
                listing logEnv measured{srMemoryMeter = meter} binding $ \response -> do
                    atRespond <- live <$> sampleAfterSweep deadlines
                    writeIORef responded (Just (response, atRespond))
                atEnd <- live <$> sampleAfterSweep deadlines
                _ <- deRefStablePtr root
                (response, atRespond) <- maybe (throwIO MissingCensusResponse) pure =<< readIORef responded
                atGate <- maybe (throwIO MissingCensusGate) pure =<< readIORef gate
                atPlan <- maybe (throwIO MissingCensusPlan) pure =<< readIORef planned
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
    listing logEnv rt binding deliver = case bindingRouter binding methodGet [] segments of
        RouteAction contract (RunPipeline _ action) -> deliverServe logEnv contract rt binding (action request) deliver
        _ -> throwIO MissingCensusRoute
      where
        (segments, request) = listingOperation name

-- Disarmed deadlines retain worker stacks until the sweeper removes their entries.
sampleAfterSweep :: Deadlines -> IO HeapSample
sampleAfterSweep deadlines = sweepTick deadlines >> sample

samplePoint :: Deadlines -> MemoryMeter -> IORef (Maybe Point) -> IO ()
samplePoint deadlines meter point = do
    now <- live <$> sampleAfterSweep deadlines
    charged <- snChargedBytes <$> meterSnapshot meter
    writeIORef point (Just (Point (toInteger now) charged))

data CensusFixtureFailure
    = MissingCensusResponse
    | MissingCensusGate
    | MissingCensusPlan
    | MissingCensusRoute
    deriving stock (Show)

instance Exception CensusFixtureFailure

listingOperation :: PackageName -> ([Text], Request)
listingOperation name = case pkgEcosystem name of
    Npm -> ordinary (T.splitOn "/" (renderPackageName name))
    PyPI -> ordinary ["simple", renderPackageName name]
    Cargo -> ordinary (T.splitOn "/" (cargoShard (renderPackageName name)))
    RubyGems -> ordinary ["info", renderPackageName name]
  where
    ordinary segments = (segments, defaultRequest{requestMethod = methodGet})

{- Serve the capture with its locations on this origin's own authority, as projection keeps no other.
The body is rebased once, so the origin holds the same bytes at every sample. -}
servingCapture :: Ecosystem -> Text -> ByteString -> IO Application
servingCapture ecosystem captured capture = do
    rebased <- newIORef Nothing
    pure $ \request respond ->
        if ecosystem == Cargo && pathInfo request == ["config.json"]
            then respond (responseLBS status200 [(hContentType, "application/json")] (LBS.fromStrict (cargoViewDiscovery (selfBaseUrl request) Nothing False)))
            else do
                body <- readIORef rebased >>= maybe (rebase request >>= \fresh -> fresh <$ writeIORef rebased (Just fresh)) pure
                respond (responseLBS status200 [] body)
  where
    authority = T.intercalate "/" (take 3 (T.splitOn "/" captured))
    rebase request = evaluate (rebaseAuthority authority (selfBaseUrl request) (LBS.fromStrict capture))

report :: CorpusPackage -> ByteString -> Census -> IO ()
report package capture census =
    putStrLn . ("listing-census " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "package" .= cpName package
            , "ecosystem" .= ecosystemName (pkgEcosystem (cpPackage package))
            , "path" .= cpPath package
            , "sha256" .= sha256Hex capture
            , "held_at_gate" .= held census (censusGate census)
            , "charged_at_gate" .= pointCharged (censusGate census)
            , "held_when_planned" .= held census (censusPlanned census)
            , "charged_when_planned" .= pointCharged (censusPlanned census)
            , "request_while_responding" .= (censusAtRespond census - censusAfter census)
            , "meter_step" .= meterStepBytes
            ]
