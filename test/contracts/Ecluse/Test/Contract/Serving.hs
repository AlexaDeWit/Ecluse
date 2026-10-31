-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RankNTypes #-}

-- | Admission, origin authority, HTTP representation and content-coding laws over native request drivers.
module Ecluse.Test.Contract.Serving (
    ServingDriver (..),
    ServingSession (..),
    ServingRelease (..),
    ServingResponse (..),
    OriginFault (..),
    ServingFaults (..),
    servingContract,
    artifactIntegrityCase,
    artifactDigestCase,
    artifactHeadCase,
    artifactHeadLengthCase,
) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Time (nominalDay)
import Network.HTTP.Types (Header, Method, methodGet, methodHead)
import Test.Hspec

import Ecluse.Core.Package (HashAlg (SHA512))
import Ecluse.Core.Package.Integrity (MinIntegrity, mkMinIntegrity)
import Ecluse.Core.Rules.Types (Rule (AllowByIdentity, AllowIfOlderThan, DenyByIdentity))
import Ecluse.Core.Server.Encoding (MetadataCompression (GzipWhenAccepted, IdentityOnly))
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
    -- ^ Native checksums, or hashes of gated artifact responses addressed by emitted coordinates.
    }
    deriving stock (Eq, Show)

-- | Requests to the one registry that serves authorised private and admitted public releases together.
data ServingSession = ServingSession
    { requestMetadata :: Method -> [Header] -> IO ServingResponse
    , requestArtifact :: Method -> [Header] -> Text -> IO ServingResponse
    }

-- | How a faulted origin answers every request.
data OriginFault
    = -- | This HTTP status with an empty body.
      FaultStatus Int
    | -- | A closed connection with no HTTP response.
      FaultClosed
    | -- | The mount configures no such origin. Only the private origin can be left out.
      FaultUnconfigured
    deriving stock (Eq, Show)

-- | The faults that replace a session's private and public origins.
data ServingFaults = ServingFaults
    { privateFault :: Maybe OriginFault
    , publicFault :: Maybe OriginFault
    }
    deriving stock (Eq, Show)

-- | Native fixture encoding, resource ownership, requests and digest spelling, under a mount's metadata coding.
data ServingDriver = ServingDriver
    { withServing :: forall a. MetadataCompression -> [Rule] -> [ServingRelease] -> [ServingRelease] -> (ServingSession -> IO a) -> IO a
    , withFaultedServing :: forall a. ServingFaults -> [Rule] -> [ServingRelease] -> [ServingRelease] -> (ServingSession -> IO a) -> IO a
    -- ^ 'withServing' under gzip, with either origin replaced by its fault.
    , withPublicFloor :: forall a. MinIntegrity -> [Rule] -> [ServingRelease] -> [ServingRelease] -> (ServingSession -> IO a) -> IO a
    -- ^ 'withServing' under gzip, with the public integrity floor raised to this one.
    , servingIdentity :: Text -> Text
    , servingDigest :: ByteString -> Text
    }

-- | Run the shared policy, authority, integrity and conditional response obligations.
servingContract :: Text -> ServingDriver -> Spec
servingContract provider driver = describe "shared serving contract" $ do
    contractExample "default-denial" "denies public versions by default, and answers 404 for a listing with nothing admitted beside any private origin" $
        -- A private origin that holds nothing, one that answers 404, and none at all.
        for_ [Nothing, Just (FaultStatus 404), Just FaultUnconfigured] $ \private ->
            withFaultedServing driver (ServingFaults private Nothing) [] [] [old] $ \session -> do
                for_ [methodGet, methodHead] $ \method -> do
                    response <- requestMetadata session method []
                    (responseStatus response, responseVersions response) `shouldBe` (404, [])
                artifact <- requestArtifact session methodGet [] "1.2.3"
                responseStatus artifact `shouldBe` 403
    contractExample "identity-precedence" "admits an explicit identity and lets denial take precedence" $ do
        let packageIdentity = servingIdentity driver "1.2.3"
        run [AllowByIdentity packageIdentity] [] [young] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3"]
        run [AllowByIdentity packageIdentity, DenyByIdentity packageIdentity] [] [old] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 404
            artifact <- requestArtifact session methodGet [] "1.2.3"
            responseStatus artifact `shouldBe` 403
    contractExample "authorised-union" "serves the union of trusted-private and gated-public versions" $
        -- The private origin alone holds 3.4.5 and its own copy of 1.2.3, so each origin's share is visible.
        run agePolicy [old{releaseVersion = "3.4.5"}, old{releaseBytes = "private bytes"}] [old, other] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3", "2.3.4", "3.4.5"]
            Map.lookup "1.2.3" (responseDigests response) `shouldBe` Just [servingDigest driver "private bytes"]
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
            artifact <- requestArtifact session methodGet [] "1.2.3"
            responseStatus artifact `shouldBe` 200
            responseBody artifact `shouldBe` "private bytes"
    contractExample "private-availability" "refuses on a failing private origin, serves after a private 404 or a public outage, and answers 404 when no configured origin holds the name" $ do
        -- An unavailable private origin answers 503, and an invalid private reply answers 502.
        for_ [(FaultStatus 429, 503), (FaultStatus 500, 503), (FaultStatus 503, 503), (FaultClosed, 503), (FaultStatus 302, 502), (FaultStatus 400, 502)] $ \(fault, refusal) ->
            faulted (ServingFaults (Just fault) Nothing) [] [old] $ \session -> do
                response <- requestMetadata session methodGet []
                (responseStatus response, responseVersions response) `shouldBe` (refusal, [])
                artifact <- requestArtifact session methodGet [] "1.2.3"
                responseStatus artifact `shouldBe` refusal
        faulted (ServingFaults (Just (FaultStatus 404)) Nothing) [] [old] $ \session -> do
            response <- requestMetadata session methodGet []
            (responseStatus response, responseVersions response) `shouldBe` (200, ["1.2.3"])
            artifact <- requestArtifact session methodGet [] "1.2.3"
            responseStatus artifact `shouldBe` 200
        -- A public 404 beside the private one settles the name as absent. A public outage leaves it open.
        for_ [(404, 404), (503, 503)] $ \(public, settled) ->
            faulted (ServingFaults (Just (FaultStatus 404)) (Just (FaultStatus public))) [] [] $ \session -> do
                response <- requestMetadata session methodGet []
                (responseStatus response, responseVersions response) `shouldBe` (settled, [])
                artifact <- requestArtifact session methodGet [] "1.2.3"
                responseStatus artifact `shouldBe` settled
        -- With no private origin configured, a public 404 settles the name alone.
        faulted (ServingFaults (Just FaultUnconfigured) (Just (FaultStatus 404))) [] [] $ \session -> do
            for_ [methodGet, methodHead] $ \method -> do
                response <- requestMetadata session method []
                (responseStatus response, responseVersions response) `shouldBe` (404, [])
            artifact <- requestArtifact session methodGet [] "1.2.3"
            responseStatus artifact `shouldBe` 404
        -- Each ecosystem's exact refusal status is pinned beside its native routes.
        for_ [401, 403] $ \refused ->
            faulted (ServingFaults (Just (FaultStatus refused)) Nothing) [] [old] $ \session -> do
                response <- requestMetadata session methodGet []
                responseStatus response `shouldSatisfy` (`elem` [401, 403])
                responseVersions response `shouldBe` []
                artifact <- requestArtifact session methodGet [] "1.2.3"
                responseStatus artifact `shouldSatisfy` (`elem` [401, 403])
        faulted (ServingFaults Nothing (Just (FaultStatus 503))) [other] [] $ \session -> do
            response <- requestMetadata session methodGet []
            (responseStatus response, responseVersions response) `shouldBe` (200, ["2.3.4"])
            artifact <- requestArtifact session methodGet [] "2.3.4"
            responseStatus artifact `shouldBe` 200
    contractExample "public-integrity" "filters a hashless public version from the listing" $ do
        run agePolicy [] [old, other{releaseHasIntegrity = False}] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["1.2.3"]
        -- Every fixture digest is SHA-256, so a SHA-512 public floor reaches each native format,
        -- including those whose parser already refuses a hashless row. The trusted floor is unchanged.
        sha512Floor <- either (fail . toString) pure (mkMinIntegrity SHA512)
        withPublicFloor driver sha512Floor agePolicy [old{releaseVersion = "3.4.5"}] [old] $ \session -> do
            response <- requestMetadata session methodGet []
            responseStatus response `shouldBe` 200
            responseVersions response `shouldBe` ["3.4.5"]
            artifact <- requestArtifact session methodGet [] "1.2.3"
            responseStatus artifact `shouldBe` 403
    contractExample "trusted-integrity" "applies the default integrity floor to trusted-private versions" $
        run agePolicy [old{releaseHasIntegrity = False}] [] $ \session -> do
            response <- requestMetadata session methodGet []
            -- npm and PyPI refuse the version at the trusted floor, so the listing admits nothing.
            -- Cargo and RubyGems drop a row with no checksum as malformed, so nothing is refused.
            (responseStatus response, responseVersions response) `shouldBe` (if provider `elem` ["cargo", "rubygems"] then 403 else 404, [])
    contractExample "artifact-integrity" "serves the exact artifact bytes and advertised integrity" $
        artifactIntegrityCase driver
    contractExample "artifact-head" "serves artifact HEAD without transferring bytes" $
        run agePolicy [] [old] $ \session ->
            requestArtifact session methodHead [] "1.2.3" >>= artifactHeadCase
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
    contractExample "metadata-gzip" "gzips the identity listing's exact bytes for a client that accepts gzip" $
        run agePolicy [] [old, other] $ \session -> do
            plain <- requestMetadata session methodGet []
            packed <- requestMetadata session methodGet [("Accept-Encoding", "gzip")]
            map responseStatus [plain, packed] `shouldBe` [200, 200]
            lookup "Content-Encoding" (responseHeaders plain) `shouldBe` Nothing
            lookup "Content-Encoding" (responseHeaders packed) `shouldBe` Just "gzip"
            map varies [plain, packed] `shouldBe` [True, True]
            GZip.decompress (responseBody packed) `shouldBe` responseBody plain
            lookup "Content-Length" (responseHeaders packed) `shouldBe` Just (show (LBS.length (responseBody packed)))
    contractExample "metadata-gzip-validators" "gives each coding its own validator for HEAD and conditional requests" $
        run agePolicy [] [old] $ \session -> do
            plain <- requestMetadata session methodGet []
            packed <- requestMetadata session methodGet gzipOffer
            plainTag <- validator plain
            gzipTag <- validator packed
            gzipTag `shouldSatisfy` BS.isPrefixOf "W/"
            gzipTag `shouldNotBe` plainTag
            headed <- requestMetadata session methodHead gzipOffer
            responseStatus headed `shouldBe` 200
            responseBody headed `shouldBe` ""
            lookup "ETag" (responseHeaders headed) `shouldBe` Just gzipTag
            lookup "Content-Length" (responseHeaders headed) `shouldBe` Just (show (LBS.length (responseBody packed)))
            for_ [methodGet, methodHead] $ \method -> do
                matched <- requestMetadata session method (("If-None-Match", gzipTag) : gzipOffer)
                responseStatus matched `shouldBe` 304
                responseBody matched `shouldBe` ""
                lookup "ETag" (responseHeaders matched) `shouldBe` Just gzipTag
                varies matched `shouldBe` True
            crossed <- requestMetadata session methodGet (("If-None-Match", plainTag) : gzipOffer)
            (responseStatus crossed, lookup "Content-Encoding" (responseHeaders crossed)) `shouldBe` (200, Just "gzip")
            unpacked <- requestMetadata session methodGet [("If-None-Match", gzipTag)]
            (responseStatus unpacked, lookup "Content-Encoding" (responseHeaders unpacked)) `shouldBe` (200, Nothing)
    contractExample "metadata-gzip-refusal" "answers 406 to a request that excludes both codings" $
        run agePolicy [] [old] $ \session -> do
            refused <- requestMetadata session methodGet [("Accept-Encoding", "gzip;q=0, identity;q=0")]
            responseStatus refused `shouldBe` 406
            varies refused `shouldBe` True
    contractExample "metadata-gzip-opt-out" "keeps the identity listing unchanged when the operator opts out" $
        withServing driver IdentityOnly agePolicy [] [old] $ \session -> do
            plain <- requestMetadata session methodGet []
            for_ [gzipOffer, [("Accept-Encoding", "*;q=0")]] $ \offered -> do
                response <- requestMetadata session methodGet offered
                response `shouldBe` plain
            varies plain `shouldBe` False
    contractExample "metadata-gzip-policy" "applies policy before a gzip validator can answer 304" $ do
        gzipTag <- run agePolicy [] [old] $ \session -> requestMetadata session methodGet gzipOffer >>= validator
        run [] [] [old] $ \session -> do
            refused <- requestMetadata session methodGet (("If-None-Match", gzipTag) : gzipOffer)
            responseStatus refused `shouldBe` 404
    contractExample "artifact-uncoded" "relays artifact GET and HEAD unchanged to a client that accepts gzip" $
        run agePolicy [] [old] $ \session ->
            for_ [methodGet, methodHead] $ \method -> do
                plain <- requestArtifact session method [] "1.2.3"
                offered <- requestArtifact session method gzipOffer "1.2.3"
                responseStatus offered `shouldBe` 200
                -- The upstream's own Date may tick between the two requests.
                let undated response = response{responseHeaders = filter ((/= "Date") . fst) (responseHeaders response)}
                undated offered `shouldBe` undated plain
                lookup "Content-Encoding" (responseHeaders offered) `shouldBe` Nothing
  where
    contractExample identifier description = it (caseLabel ("integration.serving." <> provider <> "." <> identifier) description)
    run = withServing driver GzipWhenAccepted
    faulted faults = withFaultedServing driver faults agePolicy
    gzipOffer = [("Accept-Encoding", "gzip")]
    varies response = maybe False (BS.isInfixOf "Accept-Encoding") (lookup "Vary" (responseHeaders response))
    validator response = maybe (fail "no ETag on admitted metadata") pure (lookup "ETag" (responseHeaders response))
    agePolicy = [AllowIfOlderThan (7 * nominalDay)]
    old = ServingRelease "1.2.3" 30 "release bytes" True
    young = old{releaseAgeDays = 1}
    other = old{releaseVersion = "2.3.4"}

-- | Require both the fixture bytes and the digest advertised by the native listing.
artifactIntegrityCase :: ServingDriver -> IO ()
artifactIntegrityCase driver =
    withServing driver GzipWhenAccepted [AllowIfOlderThan (7 * nominalDay)] [] [release] $ \session -> do
        response <- requestMetadata session methodGet []
        responseStatus response `shouldBe` 200
        Map.lookup "1.2.3" (responseDigests response) `shouldBe` Just [servingDigest driver (releaseBytes release)]
        artifact <- requestArtifact session methodGet [] "1.2.3"
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
    advertisedLength response `shouldSatisfy` maybe False (> 0)

-- | An admitted artifact HEAD advertises the exact length of the artifact's bytes.
artifactHeadLengthCase :: Integer -> ServingResponse -> Expectation
artifactHeadLengthCase expected response = do
    artifactHeadCase response
    advertisedLength response `shouldBe` Just expected

advertisedLength :: ServingResponse -> Maybe Integer
advertisedLength response = lookup "Content-Length" (responseHeaders response) >>= readMaybe . toString . decodeUtf8 @Text
