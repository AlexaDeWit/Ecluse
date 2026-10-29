-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Labelled synthetic named-gem histories exercise existing listing guards above one meter step.
module Ecluse.Core.Registry.RubyGems.ServingResidencySpec (spec) where

import Data.Aeson (ToJSON, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Version (showVersion)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Info (arch, compilerName, compilerVersion)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Composition.MemoryPlan.Transient (meterStepBytes)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Server.MemoryModel.Calibration (SyntheticListingFigures (..), rubyGemsSyntheticFigures)
import Ecluse.Core.Server.MemoryModel.Probe (ListingPeaks (..), SingleListing (..), measurePathInChild, writeNamedMergeDocuments)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Core.Server.MemoryModelResidencySpec (checkOutputCharge, checkOutputLimit, checkPaid, checkPhases, checkReadPeak, checkRegressionLimits)
import Ecluse.Test.Corpus.Merge (MergeShape (..), realisticShape)
import Ecluse.Test.Corpus.RubyGems (benchGem, syntheticInfoBytes)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Support (expectRight)

data GeneratedSource = GeneratedSource
    { sourceDirectory :: FilePath
    , sourcePath :: FilePath
    , sourceBytes :: ByteString
    }

-- | Preserve public captures while requiring single and merged synthetic source/basis coverage.
spec :: Spec
spec = describe "RubyGems synthetic above-step listings" $ aroundAll withGeneratedSource $ do
    it "preserves native facts in all generated merge inputs" $ \input -> do
        original <- expectRight (referenceInfo (sourceBytes input))
        length original `shouldBe` generatedVersionCount
        let declared = Map.fromList [((rowVersion row, rowPlatform row), normalise row) | row <- original]
        for_ [minBound .. maxBound] $ \shape -> do
            (private, public) <- mergePaths shape input
            inputs <- traverse (readFileBS >=> expectRight . referenceInfo) [private, public]
            map length inputs `shouldBe` expectedCounts shape
            for_ inputs $ \rows -> do
                rows `shouldSatisfy` (not . null)
                for_ rows $ \row -> Map.lookup (rowVersion row, rowPlatform row) declared `shouldBe` Just (normalise row)
    it "measures the required single-source per-byte guards" $ \input -> do
        let mode = Mode.ListingFor "rubygems" "bench-gem"
        measured <- measurePathInChild mode (sourcePath input)
        report input "single" mode (sourcePath input) [sourcePath input] measured
        result <- expectRight measured
        let peaks = singlePeaks result
        checkBasis input peaks
        listingSourceBytes peaks `shouldBe` BS.length (sourceBytes input)
        singleDocumentLive result `shouldSatisfy` (> 0)
        singleDocumentLive result `shouldSatisfy` (<= toInteger (singleDocumentCharge result))
        runChargeGuards peaks
        checkReadPeak (syntheticReadPeak <$> rubyGemsSyntheticFigures) peaks
        checkOutputLimit (syntheticSingleOutput <$> rubyGemsSyntheticFigures) peaks
    for_ [minBound .. maxBound] $ \shape ->
        it ("measures native merge guards for " <> show shape) $ \input -> do
            (private, public) <- mergePaths shape input
            let mode = Mode.MergeFor "rubygems" "bench-gem" private
            measured <- measurePathInChild mode public
            report input (show shape) mode public [private, public] measured
            peaks <- expectRight measured
            checkBasis input peaks
            expectedSourceBytes <- sum <$> traverse (fmap BS.length . BS.readFile) [private, public]
            listingSourceBytes peaks `shouldBe` expectedSourceBytes
            runChargeGuards peaks
            when (realisticShape shape) (checkOutputLimit (rubyGemsSyntheticFigures >>= Map.lookup shape . syntheticMergedOutputs) peaks)
  where
    normalise row = row{rowPosition = 0, rowFields = filter ((/= "ecluse_benchmark") . fst) (rowFields row)}
    expectedCounts = \case
        Identical -> [generatedVersionCount, generatedVersionCount]
        Overlapping -> [generatedVersionCount - generatedVersionCount `div` 3, generatedVersionCount - (generatedVersionCount + 2) `div` 3]
        Disjoint -> [(generatedVersionCount + 1) `div` 2, generatedVersionCount `div` 2]
        PublishOrder -> [(generatedVersionCount + 3) `div` 4, generatedVersionCount]
        HeavyBase -> [(generatedVersionCount + 9) `div` 10, generatedVersionCount]
        HeavyOldBase -> [(generatedVersionCount + 9) `div` 10, generatedVersionCount]

withGeneratedSource :: (GeneratedSource -> IO a) -> IO a
withGeneratedSource action = withSystemTempDirectory "ecluse-generated-ruby-listing" $ \directory -> do
    let path = directory </> "bench-gem.info"
        bytes = syntheticInfoBytes generatedVersionCount
    BS.writeFile path bytes
    action (GeneratedSource directory path bytes)

generatedVersionCount :: Int
generatedVersionCount = 16384

mergePaths :: MergeShape -> GeneratedSource -> IO (FilePath, FilePath)
mergePaths shape input = do
    let directory = sourceDirectory input </> show shape
    createDirectoryIfMissing True directory
    writeNamedMergeDocuments shape directory benchGem (sourcePath input)

checkBasis :: GeneratedSource -> ListingPeaks -> Expectation
checkBasis input peaks = do
    BS.length (sourceBytes input) `shouldSatisfy` (>= meterStepBytes)
    listingSourceBytes peaks `shouldSatisfy` (>= meterStepBytes)
    listingBasisBytes peaks `shouldSatisfy` (>= meterStepBytes)

runChargeGuards :: ListingPeaks -> Expectation
runChargeGuards peaks = do
    adapter <- maybe (fail "RubyGems synthetic listing has no measured adapter factors") pure (adapterFor RubyGems)
    let factors = metadataChargeFactors (adapterMetadata adapter)
    checkPaid factors peaks
    checkPhases factors peaks
    checkOutputCharge factors peaks
    for_ rubyGemsSyntheticFigures $ \figures ->
        checkRegressionLimits factors [syntheticReadPeak figures] (syntheticSingleOutput figures : Map.elems (syntheticMergedOutputs figures))

-- Raw results precede every basis, charge and committed-figure assertion.
report :: (ToJSON a) => GeneratedSource -> Text -> Mode.ProbeMode -> FilePath -> [FilePath] -> Either String a -> IO ()
report input shape mode path paths measured = do
    files <- traverse BS.readFile paths
    source <- lookupEnv "ECLTEST_SOURCE"
    commit <- lookupEnv "GITHUB_SHA"
    run <- lookupEnv "GITHUB_RUN_ID"
    attempt <- lookupEnv "GITHUB_RUN_ATTEMPT"
    job <- lookupEnv "GITHUB_JOB"
    let outcome = either (const ("failed" :: Text)) (const "measured") measured
    putTextLn . ("metadata-synthetic-listing " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "input_kind" .= ("synthetic" :: Text)
            , "fixture" .= ("rubygems/long-history-v1" :: Text)
            , "generator" .= ("Ecluse.Test.Corpus.RubyGems.syntheticInfoBytes" :: Text)
            , "generator_version" .= (1 :: Int)
            , "version_count" .= generatedVersionCount
            , "package" .= renderPackageName benchGem
            , "shape" .= shape
            , "input_bytes" .= BS.length (sourceBytes input)
            , "input_sha256" .= hexSha256Of (sourceBytes input)
            , "source_bytes" .= map BS.length files
            , "source_sha256" .= map hexSha256Of files
            , "source" .= (source <|> commit)
            , "run" .= run
            , "attempt" .= attempt
            , "job" .= job
            , "architecture" .= arch
            , "compiler" .= (compilerName <> "-" <> showVersion compilerVersion)
            , "child_arguments" .= map (\argument -> if argument `elem` paths then "<generated-source>" else argument) (Mode.probeArguments mode path)
            , "committed_figures" .= isJust rubyGemsSyntheticFigures
            , "outcome" .= outcome
            , "error" .= leftToMaybe measured
            , "measurement" .= rightToMaybe measured
            ]
