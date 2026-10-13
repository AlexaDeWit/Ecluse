-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Deterministic negative controls for the live metadata verdict.
module Ecluse.Test.Contract.RegistrySmokeSpec (spec) where

import Control.Exception (AsyncException (ThreadKilled))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldThrow)
import Test.Hspec.Core.Spec qualified as Hspec
import UnliftIO.Exception (fromException, throwIO)

import Ecluse.Core.Fault (TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable), transportFault)
import Ecluse.Core.Package (PackageInfo (infoVersions), renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchTransport))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (..))
import Ecluse.Core.Registry.Metadata.Fetch (readManifest)
import Ecluse.Core.Registry.Metadata.Fetch.Types (ReadTerms (..))
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooDeeplyNested, TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxMetadataBytes, maxNestingDepth, maxVersionCount), defaultLimits)
import Ecluse.Test.Contract.RegistrySmoke (liveMetadata)
import Ecluse.Test.Corpus.Npm (benchPackageName, syntheticPackumentBytes)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody, heldManifest)
import Ecluse.Test.Support (expectRight)

-- | Broken provider facts cannot pass as live protocol evidence.
spec :: Spec
spec = describe "liveMetadata" $ do
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
        liveMetadata "expected" (throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)
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
