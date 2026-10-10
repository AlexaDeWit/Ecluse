-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The single-version evaluation that public admission and the mirror worker share.
module Ecluse.Core.Registry.MetadataSpec (spec) where

import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (MetadataClient, fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataFetch, MetadataUndecodable),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    VersionRead,
    fetchVersionDetails,
 )
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (sampleDetails, thingName, v1_0_0)
import Ecluse.Test.Snapshot (versionDocOf, versionReadOf)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = describe "fetchVersionDetails: the shared single-version evaluation boundary" $ do
    -- The serve-time tarball gate and the worker both resolve a version through this one
    -- function, so these cases pin its classification directly.
    it "classifies a resolved version as present" $
        fetchVersionDetails (versionClient (Right (versionReadOf (Just theDetails) (Just otherVersion)))) thingName v1_0_0
            `shouldReturn` VersionPresent (versionDocOf theDetails) (Just otherVersion)

    it "carries the document's own latest onto the present verdict" $
        fetchVersionDetails (versionClient (Right (versionReadOf (Just theDetails) Nothing))) thingName v1_0_0
            `shouldReturn` VersionPresent (versionDocOf theDetails) Nothing

    it "classifies an absent version (resolved, but no such version) as missing" $
        fetchVersionDetails (versionClient (Right (versionReadOf Nothing Nothing))) thingName v1_0_0
            `shouldReturn` VersionMissing

    it "classifies a metadata error as unavailable (the transient degrade)" $
        fetchVersionDetails (versionClient (Left MetadataUndecodable)) thingName v1_0_0
            `shouldReturn` VersionMetadataUnavailable

    it "classifies an unreachable upstream as unavailable (transport in the typed channel)" $
        fetchVersionDetails (versionClient (Left (MetadataFetch (FetchTransport (transportFault TransportUnreachable "refused"))))) thingName v1_0_0
            `shouldReturn` VersionMetadataUnavailable

    it "propagates a client that escapes its total contract (the invariant channel)" $ do
        -- Contract escapes must reach supervision instead of becoming a transient fetch outcome.
        outcome <- try (fetchVersionDetails throwingVersionClient thingName v1_0_0) :: IO (Either SomeException VersionEvaluation)
        case outcome of
            Left escaped -> fromException escaped `shouldBe` Just (TestContractEscape "simulated contract escape")
            Right evaluation -> expectationFailure ("expected the client's throw to reach the caller, got " <> show evaluation)

-- | The release a resolved read carries. Nothing here decides from its contents.
theDetails :: PackageDetails
theDetails = sampleDetails thingName v1_0_0

{- | A different version of the same package, so a present verdict's own latest is
distinguishable from the version that was asked for.
-}
otherVersion :: Version
otherVersion = mkVersion Npm "0.9.0"

{- | A 'MetadataClient' whose single-version read returns a fixed result. The full-manifest read
is unused here and refuses loudly.
-}
versionClient :: Either MetadataError VersionRead -> MetadataClient
versionClient result =
    MetadataClient
        { fetchFullManifest = const (throwIO (TestContractEscape "versionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> pure result
        }

-- | Break the metadata handle's value-error contract, to pin exception propagation.
throwingVersionClient :: MetadataClient
throwingVersionClient =
    MetadataClient
        { fetchFullManifest = const (throwIO (TestContractEscape "throwingVersionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> throwIO (TestContractEscape "simulated contract escape")
        }
