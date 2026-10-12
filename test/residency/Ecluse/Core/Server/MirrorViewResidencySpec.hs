-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated managed reads retain encoded input across cold, warm, and replacement renders.
module Ecluse.Core.Server.MirrorViewResidencySpec (spec, probeMain) where

import Control.Exception (evaluate)
import Data.Aeson (FromJSON, ToJSON, Value (Object, String), eitherDecodeStrict, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats)
import System.Directory (getFileSize)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Mirror.State.Codec (stateExchangeCharge)
import Ecluse.Core.Package (infoVersions, pkgArtifacts)
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Registry.PyPI.Metadata (pypiChargeFactors)
import Ecluse.Core.Server.Admission.Budget (roundUpToStep, scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Test.Mirror.Read
import Ecluse.Test.Mirror.State (mirrorPackage)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Support (expectRight)

-- | Absolute counters preserve the distinction between allocation and live heap.
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

instance FromJSON Measurement
instance ToJSON Measurement

-- | The recurring tier checks exchange counts, complete output, and reports admission evidence.
spec :: Spec
spec = do
    describe "managed mirror project requests" $
        for_ [1, 1000] $ \count -> it (show count <> " versions use one exchange per request") $ do
            fixture <- makeReadFixture count
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
    it "charges a 304 for one cached record and the conditional exchange" $ do
        fixture <- makeReadFixture 1000
        retained <- readRetainedCharge fixture
        session <- newReadSession fixture
        coldCharge <- validateManagedProject session
        warmCharge <- validateManagedProject session
        advanceReadSession session
        changedCharge <- validateManagedProject session
        warmCharge `shouldBe` roundUpToStep 1_048_576 (retained + 256 + stateExchangeCharge)
        coldCharge `shouldSatisfy` (> warmCharge)
        changedCharge `shouldSatisfy` (> warmCharge)
    for_ [Cold, Warm, Replacement] $ \mode ->
        it (show mode <> " retains transport, cached state, and output within their charges") $
            withSystemTempDirectory "ecluse-mirror-read" $ \directory -> do
                makeReadFixture 1000 >>= writeReadFixture directory
                executable <- getExecutablePath
                (status, output, errors) <- readProcessWithExitCode executable ["--mirror-read-probe", show mode, directory, "+RTS", "-T", "-N1", "-F1", "-A128k", "-RTS"] ""
                unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
                measured <- expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))
                putStrLn ("managed-mirror-residency " <> output)
                checkMeasurement measured

checkResult :: Int -> Int -> ReadResult -> Expectation
checkResult versions files result = do
    readManagedVersions result `shouldBe` versions
    readUnmanagedVersions result `shouldBe` 1
    readPendingVersions result `shouldBe` 0
    readUnmanagedPreserved result `shouldBe` True
    (info, document) <- expectRight (projectPyPIIndex readLimits mirrorPackage (readBody result))
    Map.size (infoVersions info) `shouldBe` versions + 1
    sum (map (length . pkgArtifacts) (Map.elems (infoVersions info))) `shouldBe` files
    let entries = [fields | (_, Object fields) <- simpleFiles document]
    length [() | fields <- entries, KeyMap.lookup "note" fields == Just (String "retained source fields")] `shouldBe` files - 3
    length [() | fields <- entries, KeyMap.lookup "upload-time" fields == Just (String "2020-01-01T00:00:00Z")] `shouldBe` files - 3
    length [() | fields <- entries, KeyMap.lookup "note" fields == Just (String "native fields")] `shouldBe` 3

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
probeMain rawPhase directory = do
    mode <- maybe (fail "unknown managed read phase") pure (readMaybe rawPhase)
    void (measure mode directory)
    before <- sample
    (ready, result, size, paid) <- measure mode directory
    after <- sample
    fixture <- loadReadFixture directory
    oldWeight <- readRetainedCharge fixture
    newWeight <- readRetainedCharge fixture{fixtureOld = fixtureNew fixture}
    nativeSize <- fromInteger <$> getFileSize (directory </> "private.json")
    let oldBudget = if mode == Cold then 0 else oldWeight
        primedWire = if mode == Cold then 0 else BS.length (fixtureOld fixture)
        requestWire = if mode == Warm then 0 else BS.length (if mode == Cold then fixtureOld fixture else fixtureNew fixture)
        wire = nativeSize + primedWire + requestWire
        allocated high low = toInteger (sampleAllocated high) - toInteger (sampleAllocated low)
    LBS.putStr . encode $
        Measurement
            (show mode)
            wire
            before
            ready
            result
            after
            size
            paid
            (roundUpToStep 1_048_576 (scaleCharge (cfFullReadPermille pypiChargeFactors) nativeSize))
            oldBudget
            (if mode == Replacement then newWeight else oldWeight)
            (allocated result ready)
            (allocated ready before)

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
