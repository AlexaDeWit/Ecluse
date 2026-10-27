-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated managed reads retain encoded input across cold, warm, and replacement renders.
module Ecluse.Core.Server.MirrorViewResidencySpec (spec, probeMain) where

import Control.Concurrent (yield)
import Control.Exception (bracket, evaluate)
import Data.Aeson (FromJSON, ToJSON, Value (Object, String), eitherDecodeStrict, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Mirror.State.Codec (stateExchangeCharge)
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision))
import Ecluse.Core.Package (infoVersions, pkgArtifacts)
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Registry.PyPI.Metadata (pypiChargeFactors)
import Ecluse.Core.Server.Admission.Budget (roundUpToStep, scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Core.Server.Cache.Types (Source (Source), fullKey)
import Ecluse.Core.Server.MirrorView (mirrorRetentionWeight)
import Ecluse.Core.Server.MirrorView.CompanionProbe qualified as CompanionProbe
import Ecluse.Test.Mirror.Read
import Ecluse.Test.Mirror.RubyGems.Residency
import Ecluse.Test.Mirror.State (mirrorPackage)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue, projectPyPIIndex)
import Ecluse.Test.Support (expectRight)

-- Absolute counters preserve the distinction between allocation and live heap.
data Sample = Sample
    { sampleLive :: Word64
    , samplePeak :: Word64
    , sampleAllocated :: Word64
    }
    deriving stock (Generic, Show)

instance FromJSON Sample
instance ToJSON Sample

data Phase = Cold | Warm | Replacement
    deriving stock (Eq, Show, Read, Enum, Bounded)

-- The child's heap samples, output size, and metered charge. The parent derives the modelled charges.
data Observed = Observed
    { obsBaseline :: Sample
    , obsReady :: Sample
    , obsHeld :: Sample
    , obsReleased :: Sample
    , obsOutputBytes :: Int
    , obsAdmissionBytes :: Int
    }
    deriving stock (Generic, Show)

instance FromJSON Observed
instance ToJSON Observed

-- The 1000-version fixture on disk, with the sizes and retention charges derived from it once.
data Prepared = Prepared
    { prepDirectory :: FilePath
    , prepOldBytes :: Int
    , prepNewBytes :: Int
    , prepNativeBytes :: Int
    , prepOldCharge :: Int
    , prepNewCharge :: Int
    }

data Measurement = Measurement
    { phase :: Text
    , inputBytes :: Int
    , baseline :: Sample
    , beforeRequest :: Sample
    , held :: Sample
    , released :: Sample
    , outputBytes :: Int
    , admissionBytes :: Int
    , nativeCharge :: Int
    , oldRetainedBudget :: Int
    , newRetainedBudget :: Int
    , requestAllocated :: Integer
    , preparationAllocated :: Integer
    }
    deriving stock (Generic, Show)

instance ToJSON Measurement

-- | The recurring tier checks exchange counts, complete output, and reports admission evidence.
spec :: Spec
spec = do
    managedReadSpec
    nativeCompanionSpec

managedReadSpec :: Spec
managedReadSpec = aroundAll withPrepared $ do
    describe "managed mirror project requests" $ do
        it "1 versions use one exchange per request" $ \_ -> makeReadFixture 1 >>= checkExchanges 1
        it "1000 versions use one exchange per request" $ \prepared -> loadReadFixture (prepDirectory prepared) >>= checkExchanges 1000
    it "charges a 304 for one cached record and the conditional exchange" checkConditionalCharge
    for_ [Cold, Warm, Replacement] $ \mode ->
        it (show mode <> " retains transport, cached state, and output within their charges") (checkPhase mode)

-- Write the fixture once for every child, and decode both revisions once under default RTS flags.
withPrepared :: (Prepared -> IO ()) -> IO ()
withPrepared action = withSystemTempDirectory "ecluse-mirror-read" $ \directory -> do
    fixture <- makeReadFixture 1000
    writeReadFixture directory fixture
    oldCharge <- readRetainedCharge fixture
    newCharge <- readRetainedCharge fixture{fixtureOld = fixtureNew fixture}
    action (Prepared directory (BS.length (fixtureOld fixture)) (BS.length (fixtureNew fixture)) (BS.length (fixturePrivate fixture)) oldCharge newCharge)

checkExchanges :: Int -> ReadFixture -> Expectation
checkExchanges count fixture = do
    session <- newReadSession fixture
    initialRead <- readManagedProject session
    cachedRead <- readManagedProject session
    advanceReadSession session
    third <- readManagedProject session
    checkResult count (3 * count + 3) initialRead
    checkResult count (3 * count + 3) cachedRead
    checkResult count (3 * count + 4) third
    readBody cachedRead `shouldBe` readBody initialRead
    exchanges <- readExchanges session
    case exchanges of
        (3, [Nothing, Just old, Just same]) -> old `shouldBe` same
        other -> expectationFailure ("unexpected state exchanges: " <> show other)

checkConditionalCharge :: Prepared -> Expectation
checkConditionalCharge prepared = do
    fixture <- loadReadFixture (prepDirectory prepared)
    session <- newReadSession fixture
    coldCharge <- validateManagedProject session
    warmCharge <- validateManagedProject session
    advanceReadSession session
    changedCharge <- validateManagedProject session
    let target = fixtureTarget fixture
        key = fullKey (Source (show target)) mirrorPackage
        entryWeight = mirrorRetentionWeight key (StateRevision target mirrorPackage "one") (prepOldCharge prepared)
    warmCharge `shouldBe` roundUpToStep 1_048_576 (entryWeight + stateExchangeCharge)
    coldCharge `shouldSatisfy` (> warmCharge)
    changedCharge `shouldSatisfy` (> warmCharge)

checkPhase :: Phase -> Prepared -> Expectation
checkPhase mode prepared = do
    executable <- getExecutablePath
    (status, output, errors) <- readProcessWithExitCode executable ["--mirror-read-probe", show mode, prepDirectory prepared, "+RTS", "-T", "-N1", "-F1", "-A128k", "-RTS"] ""
    unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
    measured <- measurement prepared mode <$> expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))
    putStrLn ("managed-mirror-residency " <> decodeUtf8 (encode measured))
    checkMeasurement measured

checkResult :: Int -> Int -> ReadResult -> Expectation
checkResult versions files result = do
    readManagedVersions result `shouldBe` versions
    readUnmanagedVersions result `shouldBe` 1
    readPendingVersions result `shouldBe` 0
    readUnmanagedPreserved result `shouldBe` True
    (info, _) <- expectRight (projectPyPIIndex readLimits mirrorPackage (readBody result))
    Map.size (infoVersions info) `shouldBe` versions + 1
    sum (map (length . pkgArtifacts) (Map.elems (infoVersions info))) `shouldBe` files
    wire <- expectRight (eitherDecodeStrict (readBody result))
    let entries = [fields | (_, Object fields) <- simpleFiles (documentFromValue wire)]
    length [() | fields <- entries, KeyMap.lookup "requires-python" fields == Just (String ">=3.8")] `shouldBe` files - 3
    length [() | fields <- entries, KeyMap.lookup "upload-time" fields == Just (String "2020-01-01T00:00:00Z")] `shouldBe` files - 3
    length [() | fields <- entries, KeyMap.lookup "requires-python" fields == Just (String ">=3.9")] `shouldBe` 3

measurement :: Prepared -> Phase -> Observed -> Measurement
measurement prepared mode observed =
    Measurement
        { phase = show mode
        , inputBytes = prepNativeBytes prepared + primedWire + requestWire
        , baseline = obsBaseline observed
        , beforeRequest = obsReady observed
        , held = obsHeld observed
        , released = obsReleased observed
        , outputBytes = obsOutputBytes observed
        , admissionBytes = obsAdmissionBytes observed
        , nativeCharge = roundUpToStep 1_048_576 (scaleCharge (cfFullReadPermille pypiChargeFactors) (prepNativeBytes prepared))
        , oldRetainedBudget = if mode == Cold then 0 else prepOldCharge prepared
        , newRetainedBudget = if mode == Replacement then prepNewCharge prepared else prepOldCharge prepared
        , requestAllocated = allocated (obsHeld observed) (obsReady observed)
        , preparationAllocated = allocated (obsReady observed) (obsBaseline observed)
        }
  where
    primedWire = if mode == Cold then 0 else prepOldBytes prepared
    requestWire = case mode of
        Cold -> prepOldBytes prepared
        Warm -> 0
        Replacement -> prepNewBytes prepared
    allocated high low = toInteger (sampleAllocated high) - toInteger (sampleAllocated low)

checkMeasurement :: Measurement -> Expectation
checkMeasurement measured = do
    inputBytes measured `shouldSatisfy` (> 0)
    outputBytes measured `shouldSatisfy` (> 0)
    requestAllocated measured `shouldSatisfy` (> 0)
    preparationAllocated measured `shouldSatisfy` (> 0)
    let peak = toInteger (samplePeak (held measured)) - toInteger (sampleLive (baseline measured))
        retained = toInteger (sampleLive (held measured)) - toInteger (sampleLive (baseline measured))
        residual = max 0 (toInteger (sampleLive (released measured)) - toInteger (sampleLive (baseline measured)))
        temporaryBound = toInteger (nativeCharge measured) + toInteger (oldRetainedBudget measured) + toInteger (admissionBytes measured)
        retainedBound = toInteger (nativeCharge measured) + toInteger (newRetainedBudget measured) + 2 * toInteger (outputBytes measured)
    retained `shouldSatisfy` (> 0)
    peak `shouldSatisfy` (<= temporaryBound)
    retained `shouldSatisfy` (<= retainedBound)
    residual `shouldSatisfy` (<= 16 * 1024)
    10 * residual `shouldSatisfy` (<= retained)

-- | Each child measures one phase. Peak samples include that phase's cache priming and warm-up.
probeMain :: String -> FilePath -> IO ()
probeMain rawPhase directory
    | "RubyCompanion" `T.isPrefixOf` toText rawPhase = CompanionProbe.probeMain rawPhase directory
    | otherwise = managedProbeMain rawPhase directory

managedProbeMain :: String -> FilePath -> IO ()
managedProbeMain rawPhase directory = do
    mode <- maybe (fail "unknown managed read phase") pure (readMaybe rawPhase)
    void (measure mode directory)
    baselineSample <- sample
    (ready, result, size, paid) <- measure mode directory
    finalSample <- sample
    LBS.putStr (encode (Observed baselineSample ready result finalSample size paid))

measure :: Phase -> FilePath -> IO (Sample, Sample, Int, Int)
measure mode directory = do
    session <- newReadSessionFromFiles directory
    bracket (newStablePtr session) freeStablePtr $ \root -> do
        when (mode /= Cold) (void (readManagedProject session))
        _ <- readAdmissionCharge session
        when (mode == Replacement) (advanceReadSession session)
        ready <- sample
        (result, size, paid) <- measuredRead session
        _ <- deRefStablePtr root
        pure (ready, result, size, paid)

measuredRead :: ReadSession -> IO (Sample, Int, Int)
measuredRead session = do
    result <- readManagedProject session
    bracket (newStablePtr result) freeStablePtr $ \root -> do
        measured <- sample
        value <- deRefStablePtr root
        size <- evaluate (BS.length (readBody value))
        charged <- readAdmissionCharge session
        pure (measured, size, charged)

sample :: IO Sample
sample = do
    performMajorGC
    yield
    performMajorGC
    stats <- getRTSStats
    evaluate (Sample (gcdetails_live_bytes (gc stats)) (max_live_bytes stats) (allocated_bytes stats))

nativeCompanionSpec :: Spec
nativeCompanionSpec = aroundAll (withRubyStateFiles LargeObjectState) $
    describe "RubyGems external compact object reservations" $
        for_ ["RubyCompanionRead", "RubyCompanionValidate", "RubyCompanionCorrupt", "RubyCompanionCancel", "RubyCompanionCancelValidation"] $ \mode ->
            it (mode <> " releases independently buffered objects and their request reservation") $ \files -> do
                rsfObjectBytes files `shouldSatisfy` (> 1024 * 1024)
                executable <- getExecutablePath
                (status, output, errors) <- readProcessWithExitCode executable ["--mirror-read-probe", mode, rsfDirectory files, "+RTS", "-T", "-N1", "-F1", "-A128k", "-RTS"] ""
                unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
                measured <- expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))
                putStrLn ("managed-companion-residency " <> mode <> " " <> output)
                checkNativeCompanion mode measured

checkNativeCompanion :: String -> CompanionProbe.CompanionMeasurement -> Expectation
checkNativeCompanion mode measured = do
    let retained = toInteger (CompanionProbe.cmHeldLive measured) - toInteger (CompanionProbe.cmBaselineLive measured)
        residual = max 0 (toInteger (CompanionProbe.cmReleasedLive measured) - toInteger (CompanionProbe.cmBaselineLive measured))
        paid = toInteger (CompanionProbe.cmPeakTicketCharge measured)
    CompanionProbe.cmReferenceBytes measured `shouldSatisfy` (> 64 * 1024)
    CompanionProbe.cmReadBytes measured `shouldSatisfy` (> 0)
    CompanionProbe.cmReadChunks measured `shouldSatisfy` (> 0)
    CompanionProbe.cmRequestedReadCharge measured `shouldSatisfy` (>= CompanionProbe.cmReadBytes measured)
    CompanionProbe.cmPeakTicketCharge measured `shouldSatisfy` (>= CompanionProbe.cmRequestedReadCharge measured)
    CompanionProbe.cmSampledPeakLiveGrowth measured `shouldSatisfy` (<= paid)
    CompanionProbe.cmAllocatedBytes measured `shouldSatisfy` (> 0)
    CompanionProbe.cmChargeAfterRelease measured `shouldBe` 0
    residual `shouldSatisfy` (<= 16 * 1024)
    case mode of
        "RubyCompanionCancel" -> do
            CompanionProbe.cmOutcome measured `shouldBe` "cancelled"
            CompanionProbe.cmReturnedBytes measured `shouldBe` 0
            CompanionProbe.cmReadBytes measured `shouldSatisfy` (< CompanionProbe.cmReferenceBytes measured)
            CompanionProbe.cmChargeDuringRequest measured `shouldBe` Nothing
        "RubyCompanionCancelValidation" -> do
            CompanionProbe.cmOutcome measured `shouldBe` "cancelled"
            CompanionProbe.cmReturnedBytes measured `shouldBe` 0
            CompanionProbe.cmReadBytes measured `shouldBe` CompanionProbe.cmReferenceBytes measured
            CompanionProbe.cmChargeDuringRequest measured `shouldBe` Nothing
        "RubyCompanionCorrupt" -> do
            CompanionProbe.cmOutcome measured `shouldSatisfy` T.isInfixOf "retained companion identity mismatch"
            CompanionProbe.cmReturnedBytes measured `shouldBe` 0
            CompanionProbe.cmReadBytes measured `shouldBe` CompanionProbe.cmReferenceBytes measured
        _ -> do
            CompanionProbe.cmOutcome measured `shouldBe` "verified"
            CompanionProbe.cmReturnedBytes measured `shouldBe` CompanionProbe.cmReferenceBytes measured
            CompanionProbe.cmReadBytes measured `shouldBe` CompanionProbe.cmReferenceBytes measured
            CompanionProbe.cmChargeDuringRequest measured `shouldSatisfy` maybe False (> 0)
            retained `shouldSatisfy` (> 0)
            retained `shouldSatisfy` (<= paid)
