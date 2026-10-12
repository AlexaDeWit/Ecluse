-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated managed reads retain encoded input across cold, warm, and replacement renders.
module Ecluse.Core.Server.MirrorViewResidencySpec (spec, probeMain) where

import Data.Aeson (FromJSON, ToJSON, Value (Object, String), eitherDecodeStrict, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats)
import System.Environment (getExecutablePath)
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Package (infoVersions, pkgArtifacts)
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
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

data Measurement = Measurement
    { inputBytes :: Int
    , baseline :: Sample
    , prepared :: Sample
    , cold :: Sample
    , warm :: Sample
    , replacement :: Sample
    , released :: Sample
    , outputBytes :: [Int]
    , admissionBytes :: [Int]
    , retainedBudget :: Int
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
            first <- readManagedProject session
            second <- readManagedProject session
            advanceReadSession session
            third <- readManagedProject session
            checkResult count (3 * count + 3) first
            checkResult count (3 * count + 3) second
            checkResult count (3 * count + 4) third
            readBody second `shouldBe` readBody first
            exchanges <- readExchanges session
            case exchanges of
                (3, [Nothing, Just old, Just same]) -> old `shouldBe` same
                other -> expectationFailure ("unexpected state exchanges: " <> show other)
    it "reports the cold, warm, and old-record replacement working sets" $
        withSystemTempDirectory "ecluse-mirror-read" $ \directory -> do
            makeReadFixture 1000 >>= writeReadFixture directory
            executable <- getExecutablePath
            (status, output, errors) <- readProcessWithExitCode executable ["--mirror-read-probe", directory, "+RTS", "-T", "-N1", "-F1", "-A128k", "-RTS"] ""
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
    outputBytes measured `shouldSatisfy` all (> 0)
    sampleLive (cold measured) `shouldSatisfy` (> sampleLive (baseline measured))
    sampleAllocated (replacement measured) `shouldSatisfy` (> sampleAllocated (warm measured))
    let residual = max 0 (toInteger (sampleLive (released measured)) - toInteger (sampleLive (baseline measured)))
        retained = toInteger (sampleLive (replacement measured)) - toInteger (sampleLive (baseline measured))
    case admissionBytes measured of
        [coldCharge, warmCharge, changedCharge] -> do
            warmCharge `shouldSatisfy` (< coldCharge)
            warmCharge `shouldSatisfy` (< changedCharge)
            let abovePrepared = toInteger (samplePeak (replacement measured)) - toInteger (sampleLive (prepared measured))
            abovePrepared `shouldSatisfy` (<= toInteger (retainedBudget measured) + toInteger changedCharge)
        other -> expectationFailure ("unexpected admission samples: " <> show other)
    let oldRecordLive = toInteger (sampleLive (cold measured)) - toInteger (sampleLive (prepared measured)) - toInteger (fromMaybe 0 (listToMaybe (outputBytes measured)))
    oldRecordLive `shouldSatisfy` (> 0)
    oldRecordLive `shouldSatisfy` (<= toInteger (retainedBudget measured))
    residual `shouldSatisfy` (<= 16 * 1024)
    10 * residual `shouldSatisfy` (<= retained)

-- | Fixture generation stays outside the child. Peak counters include one complete warm-up.
probeMain :: FilePath -> IO ()
probeMain directory = do
    void (measure directory)
    before <- sample
    (size, ready, first, second, third, sizes, charges) <- measure directory
    after <- sample
    retention <- loadReadFixture directory >>= readRetainedCharge
    LBS.putStr (encode (Measurement size before ready first second third after sizes charges retention))

measure :: FilePath -> IO (Int, Sample, Sample, Sample, Sample, [Int], [Int])
measure directory = do
    fixture <- loadReadFixture directory
    session <- newReadSession fixture
    bracket (newStablePtr (fixture, session)) freeStablePtr $ \root -> do
        ready <- sample
        (coldSample, firstSize, coldCharge) <- measuredRead session
        (warmSample, secondSize, warmCharge) <- measuredRead session
        advanceReadSession session
        (changedSample, thirdSize, changedCharge) <- measuredRead session
        (held, _) <- deRefStablePtr root
        let size = BS.length (fixtureOld held) + BS.length (fixtureNew held) + BS.length (fixturePrivate held)
        _ <- evaluate size
        pure (size, ready, coldSample, warmSample, changedSample, [firstSize, secondSize, thirdSize], [coldCharge, warmCharge, changedCharge])

measuredRead :: ReadSession -> IO (Sample, Int, Int)
measuredRead session = do
    result <- readManagedProject session
    bracket (newStablePtr result) freeStablePtr $ \root -> do
        measured <- sample
        held <- deRefStablePtr root
        size <- evaluate (BS.length (readBody held))
        charged <- readAdmissionCharge session
        pure (measured, size, charged)

sample :: IO Sample
sample = do
    performMajorGC
    yield
    performMajorGC
    stats <- getRTSStats
    evaluate (Sample (gcdetails_live_bytes (gc stats)) (max_live_bytes stats) (allocated_bytes stats))
