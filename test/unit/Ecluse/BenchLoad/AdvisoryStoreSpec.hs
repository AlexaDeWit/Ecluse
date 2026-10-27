-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory sync and native state retain their HTTP contracts on one owned endpoint.
module Ecluse.BenchLoad.AdvisoryStoreSpec (spec) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Header, status200, status304, status404)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai.Handler.Warp (testWithApplication)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.AdvisoryStore (AdvisoryObject (AdvisoryObject), advisoryBucket, advisoryStoreApplication, advisoryStoreReply)
import Ecluse.BenchLoad.Cargo.CopyState (copyStateApplication)
import Ecluse.BenchLoad.ProxyProcess (mirrorStateBucket)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI), ecosystemName)
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (AwsEndpoint))
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (..), DbEtag (DbEtag), FetchedObject (..), S3CveSource (s3CveFetchFor), newS3CveSource)
import Ecluse.Test.Env (withAmbientAws)
import Ecluse.Test.Mirror.Read (ReadFixture (fixtureOld, fixtureTarget), makeReadFixtureFor)
import Ecluse.Test.Mirror.State (mirrorPackage)
import Ecluse.Test.Stub (Stub (stubPort), withRoutedStub)

-- | A shared endpoint must preserve both advisory and native record replies.
spec :: Spec
spec = do
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
    describe "shared S3 endpoint" $
        it "serves Cargo advisory HEAD/GET and native conditional records, keeping missing objects separate" $ do
            input <- makeReadFixtureFor (stateTarget "https://private.fixture") 1
            let publishedAt = UTCTime (fromGregorian 2026 9 27) 3_723
                artifact = "compiled advisory bytes"
                nativePath = encodeUtf8 ("/" <> mirrorStateBucket <> "/" <> projectKey (fixtureTarget input) mirrorPackage <> "/project.json")
                native = copyStateApplication 0 (Map.singleton nativePath (fixtureOld input))
                application = advisoryStoreApplication (AdvisoryObject Cargo publishedAt artifact) native
            testWithApplication (pure application) $ \port -> withAmbientAws $ do
                source <- newS3CveSource (Just (AwsEndpoint False "127.0.0.1" port))
                let fetch = s3CveFetchFor source advisoryBucket (toText (osvDbFileName (ecosystemName Cargo))) 1_048_576
                headed <- fetchHead fetch
                (fmap foPushedAt <$> headed) `shouldBe` Right (Just (Just publishedAt))
                withSystemTempDirectory "ecluse-combined-advisory-store" $ \dir -> do
                    downloaded <- fetchDownload fetch (dir </> "artifact")
                    (Just . foEtag <$> downloaded) `shouldBe` (fmap foEtag <$> headed)
                    readFileLBS (dir </> "artifact") `shouldReturn` artifact
                manager <- HTTP.newManager HTTP.defaultManagerSettings
                let fetchObject :: ByteString -> [Header] -> IO (HTTP.Response LByteString)
                    fetchObject path headers = do
                        request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> toString (decodeUtf8 path :: Text))
                        HTTP.httpLbs request{HTTP.requestHeaders = headers, HTTP.checkResponse = \_ _ -> pass} manager
                stored <- fetchObject nativePath []
                HTTP.responseStatus stored `shouldBe` status200
                toStrict (HTTP.responseBody stored) `shouldBe` fixtureOld input
                case lookup hETag (HTTP.responseHeaders stored) of
                    Nothing -> expectationFailure "native state has no opaque revision"
                    Just revision -> do
                        unchanged <- fetchObject nativePath [(hIfNoneMatch, revision)]
                        HTTP.responseStatus unchanged `shouldBe` status304
                        HTTP.responseBody unchanged `shouldBe` ""
                        lookup hETag (HTTP.responseHeaders unchanged) `shouldBe` Just revision
                missingAdvisory <- fetchObject (encodeUtf8 ("/" <> advisoryBucket <> "/missing.db")) []
                HTTP.responseStatus missingAdvisory `shouldBe` status404
                HTTP.responseBody missingAdvisory `shouldBe` ""
                missingState <- fetchObject (encodeUtf8 ("/" <> mirrorStateBucket <> "/missing/project.json")) []
                HTTP.responseStatus missingState `shouldBe` status404
                HTTP.responseBody missingState `shouldBe` "<Error><Code>NoSuchKey</Code></Error>"
  where
    isQuoted (DbEtag tag) = T.length tag > 2 && T.isPrefixOf "\"" tag && T.isSuffixOf "\"" tag
