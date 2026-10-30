-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The advisory stub answers the proxy's S3 HEAD and GET as the sync expects.
module Ecluse.BenchLoad.AdvisoryStoreSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.AdvisoryStore (advisoryBucket, advisoryStoreReply)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (AwsEndpoint))
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), DbEtag (DbEtag), FetchedObject (..), S3CveSource (s3CveFetchFor), newS3CveSource)
import Ecluse.Test.Env (withAmbientAws)
import Ecluse.Test.Stub (Stub (stubPort), withRoutedStub)

spec :: Spec
spec =
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
