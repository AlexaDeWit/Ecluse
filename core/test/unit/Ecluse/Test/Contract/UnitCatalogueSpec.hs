-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit the actual unit registrations without running examples or fixture hooks.
module Ecluse.Test.Contract.UnitCatalogueSpec (spec, unitCatalogue) where

import Ecluse.Core.CredentialSpec qualified as Credential
import Ecluse.Core.Queue.MemorySpec qualified as Memory
import Ecluse.Core.Registry.Cargo.MetadataSpec qualified as Cargo
import Ecluse.Core.Registry.Maintenance.ProtocolSpec qualified as Protocol
import Ecluse.Core.Registry.Metadata.FetchSpec qualified as Fetch
import Ecluse.Core.Registry.Npm.MetadataSpec qualified as Npm
import Ecluse.Core.Registry.PyPI.MetadataSpec qualified as PyPI
import Ecluse.Core.Registry.RubyGems.MetadataSpec qualified as RubyGems
import Ecluse.Test.Contract.Binding (labelledCaseIds)
import Ecluse.Test.Contract.Types (Catalogue, Group)
import Ecluse.Test.Contract.UnitCatalogue
import Test.Hspec

-- | The inspected specifications acquire their resources only inside examples and hooks.
spec :: Spec
spec =
    describe "core unit contract coverage" $
        unitCoverageSpec unitCatalogue

-- | The receipt hook shares the same independently audited registration source.
unitCatalogue :: IO Catalogue
unitCatalogue = coreUnitCatalogue <$> registrations

registrations :: IO [Group]
registrations =
    sequence
        [ unitRegistration "core/metadata/fetch" "metadata-fetch" "unit/metadata" ["metadata-read"] <$> labelledCaseIds Fetch.spec
        , unitRegistration "core/metadata/projection/npm" "metadata-projection" "unit/metadata" ["metadata-projection"] <$> labelledCaseIds Npm.spec
        , unitRegistration "core/metadata/projection/pypi" "metadata-projection" "unit/metadata" ["metadata-projection"] <$> labelledCaseIds PyPI.spec
        , unitRegistration "core/metadata/projection/cargo" "metadata-projection" "unit/metadata" ["metadata-projection"] <$> labelledCaseIds Cargo.spec
        , unitRegistration "core/metadata/projection/rubygems" "metadata-projection" "unit/metadata" ["metadata-projection"] <$> labelledCaseIds RubyGems.spec
        , unitRegistration "core/queue/memory" "queue" "unit/memory" ["payload", "ephemeral-delivery"] <$> labelledCaseIds Memory.spec
        , unitRegistration "core/credential/static" "credential" "unit/static" ["token"] <$> labelledCaseIds Credential.spec
        , unitRegistration "core/maintenance/protocol" "maintenance" "unit/protocol" ["maintenance-facts", "maintenance-consent", "maintenance-inventory", "maintenance-delete"] <$> labelledCaseIds Protocol.spec
        ]
