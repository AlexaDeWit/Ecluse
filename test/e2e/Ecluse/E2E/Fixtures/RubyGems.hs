-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems families retain independent fixture metadata and group lifetimes.
module Ecluse.E2E.Fixtures.RubyGems (
    rubyFamilyMetadata,
    rubyReadFixture,
    rubyWriteFixture,
    rubyPublicationFixture,
    rubyNativeFixture,
    rubyArchiveRoute,
    rubyReadInfo,
) where

import Data.ByteString.Lazy qualified as LBS

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness.Docker (withDataPlane)
import Ecluse.E2E.Harness.Proxy (proxyGet)
import Ecluse.E2E.Harness.Types (E2E, Topology (topologyAws), rubyGemsTopology)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (releaseFilename))
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow, referenceInfo)

-- | Both pinned clients and their Ruby runtime remain required fixture prerequisites.
rubyFamilyMetadata :: Text -> Text -> [Text] -> FamilyMetadata
rubyFamilyMetadata family profile capabilities =
    FamilyMetadata
        ("rubygems/" <> family)
        ("rubygems/" <> profile)
        capabilities
        ["docker", "image", "openssl", "ruby", "bundler-2.6.9", "bundler-4.0.23"]
        (["proxy", "nginx", "rubygems-registry"] <> ["collector" | "telemetry" `elem` capabilities])
        ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]

-- | A whole canonical family owns its product and native fixture resources.
rubyReadFixture :: Text -> [Text] -> ConcreteFixture FamilyMetadata
rubyReadFixture family capabilities = ConcreteFixture (rubyFamilyMetadata family "read" capabilities) (withDataPlane rubyGemsTopology)

-- | Durable mirroring and advisory generations use the shared S3 and queue emulator.
rubyWriteFixture :: Text -> [Text] -> ConcreteFixture FamilyMetadata
rubyWriteFixture family capabilities = ConcreteFixture metadata{familyResources = familyResources metadata <> ["ministack"]} (withDataPlane rubyGemsTopology{topologyAws = True})
  where
    metadata = rubyFamilyMetadata family "write" capabilities

-- | Native publication needs its target and client tools without an unused AWS emulator.
rubyPublicationFixture :: ConcreteFixture FamilyMetadata
rubyPublicationFixture = ConcreteFixture (rubyFamilyMetadata "publish" "publication" ["read", "publication", "client-install"]) (withDataPlane rubyGemsTopology)

-- | Native client dimensions use fixed IDs that descriptions cannot change.
rubyNativeFixture :: ConcreteFixture Group
rubyNativeFixture =
    ConcreteFixture
        Group
            { groupId = "rubygems/native"
            , groupContractId = "rubygems/native"
            , groupProfileId = "rubygems/read"
            , groupTier = E2E
            , groupPartition = "rubygems"
            , groupCases = [Case ("rubygems/native/" <> name) ["rubygems/native"] | name <- ["minimum-client", "current-client", "compact-required", "info-required", "minimum-cache", "current-cache", "compact-replacement", "platforms", "checksum-failure", "prerelease", "ruby-requirement", "direct-denial", "native-surface", "metadata-no-archives", "private-authority", "native-extension"]]
            , groupCapabilities = ["read", "client-install"]
            , groupPrerequisites = familyPrerequisites metadata <> ["cc", "make"]
            , groupResources = familyResources metadata
            , groupIsolation = familyIsolation metadata
            }
        (withDataPlane rubyGemsTopology)
  where
    metadata = rubyFamilyMetadata "native" "read" ["read", "client-install"]

-- | Artifact identity comes from the native manifest rather than filename splitting.
rubyArchiveRoute :: RubyRelease -> Text
rubyArchiveRoute release = "/rubygems/gems/" <> releaseFilename release

-- | The info rows the proxy serves for a gem, read by the reference parser rather than production decoding.
rubyReadInfo :: E2E -> Text -> IO [RubyInfoRow]
rubyReadInfo proxy name = do
    (status, bytes) <- proxyGet proxy ("/rubygems/info/" <> name)
    unless (status == 200) (fail (toString ("RubyGems info returned " <> show status <> " for " <> name)))
    either (fail . toString) pure (referenceInfo (LBS.toStrict bytes))
