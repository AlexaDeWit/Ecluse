-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Deterministic controls distinguish unavailable live peers from invalid metadata and artifact responses.
module Ecluse.Test.Contract.RegistrySmokeSpec (spec) where

import Control.Exception (AsyncException (ThreadKilled))
import Control.Exception qualified as Exception
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldThrow)
import Test.Hspec.Core.Spec qualified as Hspec

import Ecluse.Core.Fault (TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable), transportFault)
import Ecluse.Core.Package (PackageInfo (infoVersions), renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchTransport), RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (..))
import Ecluse.Core.Registry.Metadata.Fetch (readManifest)
import Ecluse.Core.Registry.Metadata.Fetch.Types (ReadTerms (..))
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit, MirrorArtifactBodyLimit), LimitError (BodyTooLarge, TooDeeplyNested, TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxMetadataBytes, maxNestingDepth, maxVersionCount), boundedRead, defaultLimits)
import Ecluse.Test.Contract.RegistrySmoke (liveMetadata, liveRegistryResponse)
import Ecluse.Test.Corpus.Npm (benchPackageName, syntheticPackumentBytes)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody, heldManifest)
import Ecluse.Test.Support (expectRight)

-- | Broken provider facts cannot pass as live protocol evidence.
spec :: Spec
spec = describe "live registry verdicts" $ do
    responseSpec
    for_ [TransportTimeout, TransportUnreachable] $ \cause ->
        it ("reports " <> show cause <> " as pending") $
            liveMetadata "expected" (pure (Left (MetadataFetch (FetchTransport (transportFault cause "offline")))))
                `shouldThrow` isPendingResult
    for_ [TransportTls, TransportProtocol] $ \cause ->
        it ("fails " <> show cause <> " instead of reporting an unavailable upstream") $
            liveMetadata "expected" (pure (Left (MetadataFetch (FetchTransport (transportFault cause "request refused")))))
                `shouldThrow` isFailureException
    for_ [MetadataAbsent, MetadataAuthorisationFailure 401, MetadataAuthorisationFailure 403, MetadataHttpFailure 503] $ \fault ->
        it ("fails the status refusal " <> show fault) $
            liveMetadata "expected" (pure (Left fault)) `shouldThrow` isFailureException
    it "propagates cancellation from the fetch" $
        liveMetadata "expected" (Exception.throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)
    it "fails malformed JSON from the production read" $ do
        result <- readBody defaultLimits "{ not json"
        case result of
            Left fault -> fault `shouldBe` MetadataUndecodable
            Right _ -> expectationFailure "expected malformed JSON to fail decoding"
        liveMetadata expectedName (pure result) `shouldThrow` isFailureException
    it "fails a name mismatch from the production read" $
        do
            let body = syntheticPackumentBytes 1
                different = encodeUtf8 (T.replace expectedName "different" (decodeUtf8 body))
            _ <- liveMetadata expectedName (readBody defaultLimits body)
            result <- readBody defaultLimits different
            case result of
                Left fault -> fault `shouldBe` MetadataNameMismatch "different"
                Right _ -> expectationFailure "expected the package name mismatch refusal"
            liveMetadata expectedName (pure result) `shouldThrow` isFailureException
    for_
        [ ("body bytes", defaultLimits{maxMetadataBytes = 1}, MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 1))))
        , ("nesting depth", defaultLimits{maxNestingDepth = 1}, MetadataBoundExceeded (TooDeeplyNested 1))
        , ("version count", defaultLimits{maxVersionCount = 0}, MetadataBoundExceeded (TooManyVersions 1 0))
        , ("artifact count", defaultLimits{maxArtifactCount = 0}, MetadataBoundExceeded (TooManyArtifacts 1 0))
        ]
        $ \(label, limits, expected) ->
            it ("fails the configured " <> label <> " bound") $ do
                let body = syntheticPackumentBytes 1
                _ <- liveMetadata expectedName (readBody defaultLimits body)
                result <- readBody limits body
                case result of
                    Left fault -> fault `shouldBe` expected
                    Right _ -> expectationFailure "expected the named bound refusal"
                liveMetadata expectedName (pure result) `shouldThrow` isFailureException
    it "accepts a matching non-empty full read" $ do
        manifest <- sample
        info <- liveMetadata (renderPackageName benchPackageName) (pure (Right manifest))
        Map.size (infoVersions info) `shouldBe` 1
    it "rejects a provider that reports a different package" $ do
        manifest <- sample
        liveMetadata "different" (pure (Right manifest)) `shouldThrow` isFailureException
    it "rejects an empty provider result" $ do
        manifest <- sample
        let emptyManifest = manifest{manifestInfo = (manifestInfo manifest){infoVersions = Map.empty}}
        liveMetadata (renderPackageName benchPackageName) (pure (Right emptyManifest)) `shouldThrow` isFailureException
    it "fails a projection refusal instead of reporting successful coverage" $
        liveMetadata "expected" (pure (Left (MetadataNameMismatch "different"))) `shouldThrow` isFailureException

responseSpec :: Spec
responseSpec = describe "liveRegistryResponse" $ do
    for_ [TransportTimeout, TransportUnreachable] $ \cause ->
        it ("reports an artifact " <> show cause <> " as pending") $
            liveRegistryResponse "files" (pure (Left (FetchTransport (transportFault cause "offline")))) `shouldThrow` isPendingResult
    for_ [TransportTls, TransportProtocol] $ \cause ->
        it ("fails an artifact " <> show cause) $
            liveRegistryResponse "files" (pure (Left (FetchTransport (transportFault cause "request refused")))) `shouldThrow` isFailureException
    it "fails the actual artifact body bound" $ do
        source <- heldChunks ["gem"]
        bounded <- boundedRead (MirrorArtifactBodyLimit 1) source
        bounded `shouldBe` Left (BodyTooLarge (MirrorArtifactBodyLimit 1))
        let fetched = bimap FetchBoundExceeded (uncurry (RegistryResponse 200)) bounded
        liveRegistryResponse "files" (pure fetched) `shouldThrow` isFailureException
    it "fails a non-success HTTP status instead of declaring the registry unavailable" $
        liveRegistryResponse "files" (pure (Right (RegistryResponse 503 0 ""))) `shouldThrow` isFailureException
    it "propagates cancellation from the artifact exchange" $
        liveRegistryResponse "files" (Exception.throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)
    it "preserves the successful status and exact bounded bytes" $ do
        let response = RegistryResponse 200 3 "gem"
        liveRegistryResponse "files" (pure (Right response)) `shouldReturn` response

sample :: IO Manifest
sample = heldManifest npmRead defaultLimits "https://registry.npmjs.org" benchPackageName [syntheticPackumentBytes 1] >>= expectRight

expectedName :: Text
expectedName = renderPackageName benchPackageName

registry :: Text
registry = "https://registry.npmjs.org"

readBody :: Limits -> ByteString -> IO (Either MetadataError Manifest)
readBody limits bytes =
    readManifest
        npmRead
        passthroughTracingPort
        ReadTerms{rtLimits = limits, rtBaseUrl = registry, rtChargeFullRead = const (pure ())}
        benchPackageName
        (heldBody [bytes])

isFailureException :: SomeException -> Bool
isFailureException exception = case fromException exception of
    Just Hspec.Pending{} -> False
    Just Hspec.Failure{} -> True
    Just Hspec.Success -> False
    Nothing -> True

isPendingResult :: Hspec.ResultStatus -> Bool
isPendingResult = \case
    Hspec.Pending{} -> True
    _ -> False
