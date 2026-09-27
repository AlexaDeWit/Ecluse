-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the advisory variants' shape and the object store stub the proxy's own sync reads.
module Ecluse.BenchLoad.AdvisoriesSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.Wai.Handler.Warp (testWithApplication)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Advisories (advisoryStoreStub, allAdvisoryRules, shippedAdvisories)
import Ecluse.BenchLoad.Harness (Driver (DriveInProcess), Scenario (..), Target (Target), scenario)
import Ecluse.BenchLoad.ProxyProcess (advisoryBucket)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (AwsEndpoint))
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), DbEtag (DbEtag), FetchedObject (..), S3CveSource (s3CveFetchFor), newS3CveSource)
import Ecluse.Test.Env (withAmbientAws)

spec :: Spec
spec = do
    describe "advisory variants" $ do
        let counterpart = (scenario "merge-cold" "The no-database load." (\_ k -> k (Target Nothing (DriveInProcess (pure []))))){scenarioServiceTime = False, scenarioConcurrencyScale = 4}
        it "suffix the counterpart's name and extend its description" $ do
            scenarioName (shippedAdvisories Npm counterpart) `shouldBe` "merge-cold-advisories"
            scenarioName (allAdvisoryRules Npm counterpart) `shouldBe` "merge-cold-all-advisory-rules"
            scenarioDescription (allAdvisoryRules Npm counterpart) `shouldSatisfy` T.isPrefixOf "The no-database load. The proxy syncs"
        it "keep the counterpart's passes and connection scale" $
            for_ [shippedAdvisories Npm counterpart, allAdvisoryRules Npm counterpart] $ \variant -> do
                scenarioServiceTime variant `shouldBe` False
                scenarioConcurrencyScale variant `shouldBe` 4
                scenarioInProcess variant `shouldBe` False
    describe "advisoryStoreStub" $ do
        let publishedAt = UTCTime (fromGregorian 2026 9 27) 3_723
            artifact = "compiled advisory bytes"
            withStore eco use = testWithApplication (pure (advisoryStoreStub eco publishedAt artifact)) $ \port -> withAmbientAws $ do
                source <- newS3CveSource (Just (AwsEndpoint False "127.0.0.1" port))
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
