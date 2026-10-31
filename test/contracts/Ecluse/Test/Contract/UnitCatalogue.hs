-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent obligations for canonical unit contracts, separate from their Hspec registrations.
module Ecluse.Test.Contract.UnitCatalogue (
    coreUnitCatalogue,
    expectedUnitCatalogue,
    unitRegistration,
    unitObligation,
    metadataFetchLaws,
    metadataProjectionLaws,
    metadataFetchLawsFor,
    metadataProjectionLawsFor,
) where

import Data.Universe.Class (Universe (universe))
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMaintenance), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (maintenanceListing, maintenanceVersionDelete))
import Ecluse.Test.Contract.Types
import Prelude hiding (universe)

-- | Production registration determines supported ecosystems even when no fixture exists for one.
coreUnitCatalogue :: [Group] -> Catalogue
coreUnitCatalogue groups =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName (filter (isJust . adapterFor) universe)
        , catalogueProfiles =
            [ Profile "unit/metadata" ["npm", "pypi", "cargo", "rubygems"] [] ["metadata-read", "metadata-projection"]
            , Profile "unit/memory" [] ["memory"] ["payload", "ephemeral-delivery"]
            , Profile "unit/static" [] ["static"] ["token"]
            , Profile "unit/protocol" ["npm"] ["verdaccio"] protocolCapabilities
            ]
        , catalogueObligations =
            [ unitObligation
                "core/metadata/fetch"
                "metadata-fetch"
                "unit/metadata"
                ["metadata-read"]
                (["core.metadata.fetch." <> ecosystem <> "." <> suffix | ecosystem <- ["npm", "pypi", "cargo", "rubygems"], suffix <- metadataFetchLawsFor ecosystem] <> ["core.metadata.fetch.cargo.record-framing", "core.metadata.fetch.rubygems.record-framing"])
            , unitObligation "core/metadata/projection/npm" "metadata-projection" "unit/metadata" ["metadata-projection"] (projectionCases "npm")
            , unitObligation "core/metadata/projection/cargo" "metadata-projection" "unit/metadata" ["metadata-projection"] (projectionCases "cargo" <> map ("core.metadata.projection.cargo." <>) ["record-framing", "invalid-evidence-control", "record-siblings"])
            , unitObligation "core/metadata/projection/rubygems" "metadata-projection" "unit/metadata" ["metadata-projection"] (projectionCases "rubygems" <> map ("core.metadata.projection.rubygems." <>) ["record-framing", "invalid-evidence-control"])
            , unitObligation "core/metadata/projection/pypi" "metadata-projection" "unit/metadata" ["metadata-projection"] (projectionCases "pypi")
            , unitObligation
                "core/queue/memory"
                "queue"
                "unit/memory"
                ["payload", "ephemeral-delivery"]
                ["queue/memory/payload", "queue/memory/destructive-receive", "queue/memory/terminal-drop"]
            , unitObligation
                "core/credential/static"
                "credential"
                "unit/static"
                ["token"]
                ["credential/static/permanent/token", "credential/static/expiring/token"]
            , unitObligation
                "core/maintenance/protocol"
                "maintenance"
                "unit/protocol"
                ["maintenance-facts", "maintenance-consent", "maintenance-inventory", "maintenance-delete"]
                [ "maintenance/protocol/facts/identity-ceiling"
                , "maintenance/protocol/facts/completion-refill"
                , "maintenance/protocol/facts/inventory-capability"
                , "maintenance/protocol/consented/consent"
                , "maintenance/protocol/withheld/consent"
                , "maintenance/protocol/inventory/packages"
                , "maintenance/protocol/inventory/versions"
                , "maintenance/protocol/delete/removed"
                ]
            ]
        , catalogueGroups = groups
        }
  where
    maintenance = adapterMaintenance <$> adapterFor Npm
    protocolCapabilities =
        ["maintenance-facts", "maintenance-consent"]
            <> ["maintenance-inventory" | isJust (maintenance >>= maintenanceListing)]
            <> ["maintenance-delete" | isJust (maintenance >>= maintenanceVersionDelete)]

-- | Pure manifest expectations use reviewed obligations, never discovered examples or execution counts.
expectedUnitCatalogue :: ([Group] -> Catalogue) -> Catalogue
expectedUnitCatalogue catalogue =
    catalogue
        [ unitRegistration (obligationId obligation) (obligationContract obligation) (obligationProfile obligation) (obligationCapabilities obligation) (obligationCases obligation)
        | obligation <- catalogueObligations (catalogue [])
        , obligationSupport obligation == Supported
        ]

-- | Execution registration uses discovered IDs. Expected manifests use independently required IDs.
unitRegistration :: Text -> Text -> Text -> [Text] -> [Text] -> Group
unitRegistration identifier contract profile capabilities identifiers =
    Group
        { groupId = identifier
        , groupContractId = contract
        , groupProfileId = profile
        , groupTier = Unit
        , groupPartition = "unit"
        , groupCases = [Case caseIdentifier [identifier] | caseIdentifier <- identifiers]
        , groupCapabilities = capabilities
        , groupPrerequisites = []
        , groupResources = []
        , groupIsolation = []
        }

-- | Declare supported coverage independently of the specification used to satisfy it.
unitObligation :: Text -> Text -> Text -> [Text] -> [Text] -> Obligation
unitObligation identifier contract profile =
    Obligation identifier profile contract Supported

projectionCases :: Text -> [Text]
projectionCases ecosystem =
    map
        (("core.metadata.projection." <> ecosystem <> ".") <>)
        (metadataProjectionLawsFor ecosystem)

-- | Projection requirements stay independent of each ecosystem's registered examples.
metadataProjectionLaws :: [Text]
metadataProjectionLaws = ["malformed", "missing-name", "name-mismatch", "version-bound", "missing-version", "selected-parity"]

-- | Every registered ecosystem owes the same fetch transport and bound observations.
metadataFetchLaws :: [Text]
metadataFetchLaws =
    [ "status-200"
    , "status-201"
    , "status-299"
    , "status-401"
    , "status-403"
    , "status-404"
    , "status-301"
    , "status-304"
    , "status-400"
    , "status-408"
    , "status-410"
    , "status-429"
    , "status-500"
    , "status-503"
    , "status-599"
    , "full-charge"
    , "oversized-auth-401"
    , "oversized-auth-403"
    , "exact-byte-bound"
    , "gzip-bytes"
    , "gzip-charge"
    , "gzip-bound"
    , "decode-span-empty-url"
    , "decode-span-refused-connection"
    , "empty-url-fault"
    , "transport-fault"
    , "no-redirect"
    , "selected-worker-charge"
    , "selected-worker-refusal"
    , "held-parity"
    , "held-byte-bound"
    , "held-malformed"
    , "held-name-mismatch"
    , "held-version-bound"
    ]

-- | Caller-bound formats owe native name and origin evidence instead of absent wire identity fields.
metadataProjectionLawsFor :: Text -> [Text]
metadataProjectionLawsFor "rubygems" = map replace metadataProjectionLaws
  where
    replace "missing-name" = "name-binding"
    replace "name-mismatch" = "origin-binding"
    replace law = law
metadataProjectionLawsFor _ = metadataProjectionLaws

-- | The request origin supplies the identity boundary for named Compact Index reads.
metadataFetchLawsFor :: Text -> [Text]
metadataFetchLawsFor "rubygems" = map (\law -> if law == "held-name-mismatch" then "held-origin-mismatch" else law) metadataFetchLaws
metadataFetchLawsFor _ = metadataFetchLaws
