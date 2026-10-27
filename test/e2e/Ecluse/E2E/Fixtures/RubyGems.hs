-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems families retain independent fixture metadata and group lifetimes.
module Ecluse.E2E.Fixtures.RubyGems (
    rubyFamilyMetadata,
    rubyReadFixture,
    rubyNativeFixture,
    rubyArchiveRoute,
) where

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness.Docker (withDataPlane)
import Ecluse.E2E.Harness.Types (rubyGemsTopology)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (releaseFilename))

-- | Both pinned clients and their Ruby runtime remain required fixture prerequisites.
rubyFamilyMetadata :: Text -> Text -> [Text] -> FamilyMetadata
rubyFamilyMetadata family profile capabilities =
    FamilyMetadata
        ("rubygems/" <> family)
        ("rubygems/" <> profile)
        capabilities
        ["docker", "image", "openssl", "ruby", "bundler-2.6.9", "bundler-4.0.23"]
        ["proxy", "nginx", "rubygems-registry"]
        ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]

-- | A whole canonical family owns its product and native fixture resources.
rubyReadFixture :: Text -> [Text] -> ConcreteFixture FamilyMetadata
rubyReadFixture family capabilities = ConcreteFixture (rubyFamilyMetadata family "read" capabilities) (withDataPlane rubyGemsTopology)

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
            , groupCases = [Case ("rubygems/native/" <> name) ["rubygems/native"] | name <- ["minimum-client", "current-client", "platforms", "checksum-failure", "prerelease", "ruby-requirement", "direct-denial", "batch-limits"]]
            , groupCapabilities = ["read", "client-install"]
            , groupPrerequisites = familyPrerequisites metadata
            , groupResources = familyResources metadata
            , groupIsolation = familyIsolation metadata
            }
        (withDataPlane rubyGemsTopology)
  where
    metadata = rubyFamilyMetadata "native" "read" ["read", "client-install"]

-- | Artifact identity comes from the native manifest rather than filename splitting.
rubyArchiveRoute :: RubyRelease -> Text
rubyArchiveRoute release = "/rubygems/gems/" <> releaseFilename release
