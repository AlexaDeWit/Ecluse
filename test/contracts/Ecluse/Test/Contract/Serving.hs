-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

-- | Admission, origin authority and HTTP representation laws over native request drivers.
module Ecluse.Test.Contract.Serving (
    ServingDriver (..),
    ServingSession (..),
    ServingRelease (..),
    ServingResponse (..),
    servingContract,
    artifactIntegrityCase,
    artifactDigestCase,
    artifactHeadCase,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Time (nominalDay)
import Network.HTTP.Types (Header, Method, methodGet, methodHead)
import Test.Hspec

import Ecluse.Core.Rules.Types (Rule (AllowByIdentity, AllowIfOlderThan, DenyByIdentity))
import Ecluse.Test.Contract.Catalogue (caseLabel)

-- | A release fixture with controlled age, bytes and integrity presence.
data ServingRelease = ServingRelease
    { releaseVersion :: Text
    , releaseAgeDays :: Integer
    , releaseBytes :: ByteString
    , releaseHasIntegrity :: Bool
    }

-- | Native response observations. Readers must inspect the served wire document independently.
data ServingResponse = ServingResponse
    { responseStatus :: Int
    , responseBody :: LByteString
    , responseHeaders :: [Header]
    , responseVersions :: [Text]
    , responseDigests :: Map Text [Text]
    }
    deriving stock (Eq, Show)

-- | Requests against one isolated proxy and its upstreams.
data ServingSession = ServingSession
    { requestMetadata :: Method -> [Header] -> IO ServingResponse
    , requestArtifact :: Text -> IO ServingResponse
    }

-- | Native fixture encoding, resource ownership, requests and digest spelling.
data ServingDriver = ServingDriver
    { withServing :: forall a. [Rule] -> [ServingRelease] -> [ServingRelease] -> (ServingSession -> IO a) -> IO a
    , servingIdentity :: Text -> Text
    , servingDigest :: ByteString -> Text
    }

-- | Run the shared policy, authority, integrity and conditional response obligations.
servingContract :: Text -> ServingDriver -> Spec
servingContract provider driver = describe "shared serving contract" $ do
    contractExample "default-denial" "denies public versions by default" $
        run [] [] [old] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 403
            artifact <- requestArtifact session "1.2.3"
            responseStatus artifact `shouldBe` 403
    contractExample "identity-precedence" "admits an explicit identity and lets denial take precedence" $ do
        let packageIdentity = servingIdentity driver "1.2.3"
        run [AllowByIdentity packageIdentity] [] [young] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3"]
        run [AllowByIdentity packageIdentity, DenyByIdentity packageIdentity] [] [old] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 403
            artifact <- requestArtifact session "1.2.3"
            responseStatus artifact `shouldBe` 403
    contractExample "authorised-union" "serves the union of trusted-private and gated-public versions" $
        run agePolicy [old] [old, other] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3", "2.3.4"]
    contractExample "quarantine-private-authority" "gates public versions while trusting private release age" $
        run agePolicy [young{releaseVersion = "3.4.5"}] [old, young{releaseVersion = "2.3.4"}] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3", "3.4.5"]
    contractExample "private-divergence" "serves private integrity and bytes when public content diverges" $
        run agePolicy [old{releaseBytes = "private bytes"}] [old{releaseBytes = "public bytes"}] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3"]
            Map.lookup "1.2.3" (responseDigests response) `shouldBe` Just [servingDigest driver "private bytes"]
            artifact <- requestArtifact session "1.2.3"
            responseStatus artifact `shouldBe` 200
            responseBody artifact `shouldBe` "private bytes"
    contractExample "public-integrity" "filters a hashless public version from the listing" $
        run agePolicy [] [old, other{releaseHasIntegrity = False}] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3"]
    contractExample "trusted-integrity" "applies the default integrity floor to trusted-private versions" $
        run agePolicy [old{releaseHasIntegrity = False}] [] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 403
    contractExample "artifact-integrity" "serves the exact artifact bytes and advertised integrity" $
        artifactIntegrityCase driver
    contractExample "conditional-coherence" "keeps GET, HEAD and conditional responses coherent" $
        run agePolicy [] [old] $ \session -> do
            initial <- requestMetadata session methodGet []
            responseStatus initial `shouldBe` 200
            tag <- maybe (fail "no ETag on admitted metadata") pure (lookup "ETag" (responseHeaders initial))
            headResponse <- requestMetadata session methodHead []
            responseStatus headResponse `shouldBe` 200
            responseBody headResponse `shouldBe` ""
            lookup "ETag" (responseHeaders headResponse) `shouldBe` Just tag
            lookup "Content-Length" (responseHeaders headResponse) `shouldBe` Just (show (LBS.length (responseBody initial)))
            for_ [methodGet, methodHead] $ \method -> do
                matched <- requestMetadata session method [("If-None-Match", tag)]
                responseStatus matched `shouldBe` 304
                responseBody matched `shouldBe` ""
  where
    contractExample identifier description = it (caseLabel ("integration.serving." <> provider <> "." <> identifier) description)
    run = withServing driver
    agePolicy = [AllowIfOlderThan (7 * nominalDay)]
    old = ServingRelease "1.2.3" 30 "release bytes" True
    young = old{releaseAgeDays = 1}
    other = old{releaseVersion = "2.3.4"}

-- | Require both the fixture bytes and the digest advertised by the native listing.
artifactIntegrityCase :: ServingDriver -> IO ()
artifactIntegrityCase driver =
    withServing driver [AllowIfOlderThan (7 * nominalDay)] [] [release] $ \session -> do
        response <- requestMetadata session methodGet []
        responseStatus response `shouldBe` 200
        Map.lookup "1.2.3" (responseDigests response) `shouldBe` Just [servingDigest driver (releaseBytes release)]
        artifact <- requestArtifact session "1.2.3"
        artifactDigestCase (servingDigest driver) (servingDigest driver (releaseBytes release)) artifact
        responseBody artifact `shouldBe` LBS.fromStrict (releaseBytes release)
        Map.lookup "1.2.3" (responseDigests response) `shouldBe` Just [servingDigest driver (toStrict (responseBody artifact))]
  where
    release = ServingRelease "1.2.3" 30 "release bytes" True

-- | Compare downloaded bytes with the digest advertised by an independently read listing.
artifactDigestCase :: (ByteString -> Text) -> Text -> ServingResponse -> Expectation
artifactDigestCase digest advertised response = do
    responseStatus response `shouldBe` 200
    digest (toStrict (responseBody response)) `shouldBe` advertised

-- | An admitted artifact HEAD advertises a nonempty representation without transferring it.
artifactHeadCase :: ServingResponse -> Expectation
artifactHeadCase response = do
    responseStatus response `shouldBe` 200
    responseBody response `shouldBe` ""
    let contentLength = lookup "Content-Length" (responseHeaders response) >>= parseLength
        parseLength bytes = readMaybe (toString (decodeUtf8 bytes :: Text))
    (contentLength :: Maybe Integer) `shouldSatisfy` maybe False (> 0)
