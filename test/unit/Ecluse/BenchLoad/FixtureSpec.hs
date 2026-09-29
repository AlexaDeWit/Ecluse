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
import Test.Hspec

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (loadCorpusCuts, weightedMix)
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Security (Limits, defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath, cpTier, cpWeight), CorpusTier (Medium), corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Subset (newestNpmShare, newestPyPIShare)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)

spec :: Spec
spec = do
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

    describe "weightedMix" $
        it "repeats each package's URL on the port by its weight, in corpus order" $
            weightedMix cpWeight (\port name -> show port <> "/" <> name) mediumNpm 8080
                `shouldBe` concat [replicate (cpWeight cp) ("8080/" <> cpName cp) | cp <- mediumNpm]
  where
    mediumNpm = filter ((== Medium) . cpTier) corpusPackages

-- Each cut, as the proxy projects it, holds the share of its capture's versions and none older than a dropped one.
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
