-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the load fixtures read the corpus as a private upstream's cut copy, and weight a listing mix.
module Ecluse.BenchLoad.FixtureSpec (spec) where

import Data.Aeson (Value, encode, toJSON)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text qualified as T
import System.IO.Error (ioeGetErrorString, isUserError)
import Test.Hspec
import UnliftIO (throwIO)

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (delayUpstream, loadCorpusBodies, loadCorpusCuts, preflightDistinct, probeBaseline, weightedMix)
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Security (Limits, defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath, cpTier, cpWeight), CorpusTier (Medium), cargoCorpusPackages, corpusPackages, cpName, pypiCorpusPackages, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.Cargo (projectCargoTree)
import Ecluse.Test.Corpus.RubyGems (projectRubyGemsTree)
import Ecluse.Test.Corpus.Subset (newestCargoShare, newestNpmShare, newestPyPIShare, newestRubyGemsShare)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)

spec :: Spec
spec = do
    describe "upstream delay" $
        it "returns without scheduling a wait for nonpositive latency" $ do
            delayUpstream 0 `shouldReturn` ()
            delayUpstream (-1) `shouldReturn` ()
    describe "listing preflight" $ do
        it "checks each distinct URL before invoking the measurement callback" $ do
            calls <- newIORef ([] :: [Text])
            let record name = modifyIORef' calls (<> [name])
            preflightDistinct record ["a", "b", "a", "b"] (record "measure")
            readIORef calls `shouldReturn` ["a", "b", "measure"]
        it "does not measure when a native listing fails preflight" $
            preflightDistinct (const (throwIO (BenchLoadError "invalid listing"))) ["a"] (expectationFailure "failed preflight measured")
                `shouldThrow` (\(BenchLoadError message) -> message == "invalid listing")
    describe "native RTT baseline" $ do
        it "warms before timing and counts only successful native samples" $ do
            calls <- newIORef ([] :: [Text])
            let fetch name = do
                    modifyIORef' calls (<> [name])
                    pure (if name == "failed" then Nothing else Just 80)
            probeBaseline 5 True ["native", "failed"] fetch `shouldReturn` MeasuredRtt 80 1
            readIORef calls `shouldReturn` ["native", "native", "failed"]
        it "declares fallback for disabled, empty, failed warm-up, and failed measured probes" $ do
            probeBaseline 5 False ["native"] (const (expectationFailure "disabled probe fetched" >> pure (Just 99))) `shouldReturn` InjectedFallback 5
            probeBaseline 2 True [] (const (pure (Just 99))) `shouldReturn` InjectedFallback 2
            probeBaseline 3 True ["native"] (const (pure Nothing)) `shouldReturn` InjectedFallback 3
            replies <- newIORef [Just 80, Nothing]
            let fetch _ =
                    atomicModifyIORef'
                        replies
                        ( \case
                            [] -> ([], Nothing)
                            result : more -> (more, result)
                        )
            probeBaseline 4 True ["native"] fetch `shouldReturn` InjectedFallback 4

    describe "loadCorpusBodies" $ do
        it "returns the authenticated bytes unchanged for both ecosystems" $ do
            let packages = mediumNpm <> pypiCorpusPackages
            expected <- Map.fromList <$> traverse (\cp -> (cpName cp,) <$> readFileLBS (cpPath cp)) packages
            loadCorpusBodies packages `shouldReturn` expected

        it "rejects a capture path outside its catalogue entry before loading or cutting" $
            for_ mediumNpm $ \cp -> do
                let packages = [cp{cpPath = "bench/corpus/unregistered.json"}]
                    wrongPath err = isUserError err && ioeGetErrorString err == "capture pin path differs for " <> toString (cpName cp)
                loadCorpusBodies packages `shouldThrow` wrongPath
                loadCorpusCuts (\_ _ -> Left "the cut must not run") packages `shouldThrow` wrongPath

    describe "loadCorpusCuts" $ do
        it "serves each capture as its cut, keyed by the package name" $ do
            sizes <- traverse (fmap BS.length . readFileBS . cpPath) mediumNpm
            cuts <- loadCorpusCuts (\name bytes -> Right (toJSON (renderPackageName name, BS.length bytes))) mediumNpm
            cuts `shouldBe` Map.fromList [(cpName cp, encode (cpName cp, size)) | (cp, size) <- zip mediumNpm sizes]

        it "refuses a capture the cut rejects" $
            loadCorpusCuts (\_ _ -> Left "no versions") mediumNpm
                `shouldThrow` (\(BenchLoadError message) -> "no versions" `T.isInfixOf` message)

        it "keeps the newest 5% of every npm capture's versions by publish time, rounded up" $
            keepsNewestShare newestNpmShare projectNpmManifest corpusPackages

        it "keeps the newest 5% of every PyPI capture's versions by publish time, rounded up" $
            keepsNewestShare newestPyPIShare projectPyPIIndex pypiCorpusPackages

        it "keeps the newest 5% of every Cargo capture's versions by publish time, rounded up" $
            keepsNewestShare newestCargoShare projectCargoTree cargoCorpusPackages

        it "keeps the newest 5% of every RubyGems capture's versions by publish time, rounded up" $
            keepsNewestShare newestRubyGemsShare projectRubyGemsTree rubyGemsCorpusPackages

    describe "weightedMix" $
        it "repeats each package's URL on the port by its weight, in corpus order" $
            weightedMix cpWeight (\port name -> show port <> "/" <> name) mediumNpm 8080
                `shouldBe` concat [replicate (cpWeight cp) ("8080/" <> cpName cp) | cp <- mediumNpm]
  where
    mediumNpm = filter ((== Medium) . cpTier) corpusPackages

keepsNewestShare ::
    (Rational -> PackageName -> ByteString -> Either String Value) ->
    (Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, b)) ->
    [CorpusPackage] ->
    Expectation
keepsNewestShare cut project packages = do
    cuts <- loadCorpusCuts (cut (5 % 100)) packages
    for_ packages $ \cp -> do
        capture <- readFileBS (cpPath cp)
        let versions :: ByteString -> Either Text (Map Text PackageDetails)
            versions = bimap show (infoVersions . fst) . project defaultLimits (cpPackage cp)
            outcome = do
                whole <- versions capture
                kept <- versions . toStrict =<< maybeToRight "no cut" (Map.lookup (cpName cp) cuts)
                pure (Map.size kept, max 1 (ceiling (toInteger (Map.size whole) * 5 % 100)), newestOf whole kept)
        (cpName cp, outcome) `shouldSatisfy` \case
            (_, Right (count, expected, newest)) -> count == expected && newest
            _ -> False

newestOf :: Map Text PackageDetails -> Map Text PackageDetails -> Bool
newestOf whole kept =
    Map.keysSet kept `Set.isSubsetOf` Map.keysSet whole
        && fromMaybe True ((<=) <$> Set.lookupMax (published (Map.difference whole kept)) <*> Set.lookupMin (published kept))
  where
    published = Set.fromList . map pkgPublishedAt . Map.elems
