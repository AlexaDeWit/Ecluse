-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the advisory variants' shape, the captures the corpus advisories name, and the store reply the proxy's sync reads.
module Ecluse.BenchLoad.AdvisoriesSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Advisories (advisoryStoreReply, allRulesAdvisories, compileCorpusAdvisories, shippedAdvisories)
import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Harness (Driver (DriveInProcess), Scenario (..), Target (Target), defaultLoadKnobs, scenario)
import Ecluse.BenchLoad.ProxyProcess (advisoryBucket)
import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbLookup), CveLookup (cveCoveredNames), openCveDb)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired), osvDbFileName)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (AwsEndpoint))
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), DbEtag (DbEtag), FetchedObject (..), S3CveSource (s3CveFetchFor), newS3CveSource)
import Ecluse.Test.Corpus (corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Env (withAmbientAws)
import Ecluse.Test.Stub (Stub (stubPort), withRoutedStub)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "advisory variants" $ do
        let counterpart = (scenario "merge-cold" "The no-database load." (\_ k -> k (Target Nothing (DriveInProcess (pure []))))){scenarioServiceTime = False, scenarioConcurrencyScale = 4}
        it "suffix the counterpart's name and extend its description" $ do
            scenarioName (shippedAdvisories Npm counterpart) `shouldBe` "merge-cold-advisories"
            scenarioName (allRulesAdvisories Npm counterpart) `shouldBe` "merge-cold-all-advisory-rules"
            scenarioDescription (allRulesAdvisories Npm counterpart) `shouldSatisfy` T.isPrefixOf "The no-database load. The proxy syncs"
        it "keep the counterpart's passes and connection scale" $
            for_ [shippedAdvisories Npm counterpart, allRulesAdvisories Npm counterpart] $ \variant -> do
                scenarioServiceTime variant `shouldBe` False
                scenarioConcurrencyScale variant `shouldBe` 4
                scenarioInProcess variant `shouldBe` False
        it "refuse an in-process counterpart, which boots no proxy to sync the database" $
            scenarioBoot (shippedAdvisories Npm counterpart{scenarioInProcess = True}) defaultLoadKnobs (const pass)
                `shouldThrow` (\(BenchLoadError message) -> "boots no proxy" `T.isInfixOf` message)
    describe "compileCorpusAdvisories" $
        it "names the load corpus captures whose rule lookups find advisories" $
            for_ [(Npm, corpusPackages, ["@babel/core", "express", "lodash", "react", "request", "webpack"]), (PyPI, pypiCorpusPackages, ["numpy", "requests"])] $ \(eco, captures, advised) ->
                withSystemTempDirectory "ecluse-corpus-advisories" $ \dir -> do
                    compiled <- compileCorpusAdvisories eco dir
                    bracket (openCveDb eco EpssRequired compiled >>= expectRight) cveDbClose $ \db -> do
                        covered <- cveCoveredNames (cveDbLookup db)
                        sort (filter (`elem` covered) (map cpName captures)) `shouldBe` advised
    describe "advisoryStoreReply" $ do
        let publishedAt = UTCTime (fromGregorian 2026 9 27) 3_723
            artifact = "compiled advisory bytes"
            withStore eco use = withRoutedStub (advisoryStoreReply eco publishedAt artifact) $ \stub -> withAmbientAws $ do
                source <- newS3CveSource (Just (AwsEndpoint False "127.0.0.1" (stubPort stub)))
                use (\key -> s3CveFetchFor source advisoryBucket key 1_048_576)
            keyOf eco = toText (osvDbFileName (ecosystemName eco))
        it "answers the proxy's S3 HEAD with an ETag and the publication time" $
            withStore Npm $ \fetchFor -> do
                headed <- fetchHead (fetchFor (keyOf Npm))
                (fmap foPushedAt <$> headed) `shouldBe` Right (Just (Just publishedAt))
                (fmap (isQuoted . foEtag) <$> headed) `shouldBe` Right (Just True)
        it "answers the proxy's S3 GET with the artifact under the same ETag" $
            withStore Npm $ \fetchFor -> withSystemTempDirectory "ecluse-advisory-stub" $ \dir -> do
                headed <- fetchHead (fetchFor (keyOf Npm))
                downloaded <- fetchDownload (fetchFor (keyOf Npm)) (dir </> "artifact")
                (Just . foEtag <$> downloaded) `shouldBe` (fmap foEtag <$> headed)
                readFileLBS (dir </> "artifact") `shouldReturn` artifact
        it "reports another ecosystem's artifact as absent" $
            withStore Npm $
                \fetchFor -> fetchHead (fetchFor (keyOf PyPI)) >>= (`shouldBe` Right Nothing)
  where
    isQuoted (DbEtag tag) = T.length tag > 2 && T.isPrefixOf "\"" tag && T.isSuffixOf "\"" tag
