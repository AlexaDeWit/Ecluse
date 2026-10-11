-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Reviewed E2E obligations remain independent of concrete registrations and providers.
module Ecluse.E2E.Requirements (obligations) where

import Ecluse.Test.Contract.Types

-- | Required cases preserve the original examples and their negative controls.
obligations :: [Obligation]
obligations =
    [ Obligation
        "npm/install"
        "npm/registry"
        "npm/install"
        Supported
        ["read", "mirror", "client-install"]
        ["npm/install/default-denial", "npm/install/lifecycle-script-control", "npm/install/deprecation-relay", "npm/install/deprecation-negative-control", "npm/install/integrity-refusal", "npm/install/head-no-effect", "npm/install/metadata-preservation", "npm/install/latest-preservation", "npm/install/publication-unconfigured"]
    , Obligation
        "npm/mirror"
        "npm/registry"
        "npm/mirror"
        Supported
        ["read", "mirror", "client-install"]
        ["npm/mirror/dependency-graph", "npm/mirror/module-exports", "npm/mirror/linked-executable", "npm/mirror/origin-evidence", "npm/mirror/installed-tree", "npm/mirror/empty-store-control"]
    , Obligation
        "npm/publish"
        "npm/registry"
        "npm/publish"
        Supported
        ["read", "publication", "client-install"]
        ["npm/publish/publish-install", "npm/publish/scope-refusal"]
    , Obligation
        "npm/dredger"
        "npm/maintenance"
        "npm/dredger"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        ["npm/dredger/inventory", "npm/dredger/admission-only", "npm/dredger/denied-versions", "npm/dredger/dry-run", "npm/dredger/consent-refusal", "npm/dredger/first-party", "npm/dredger/two-stores", "npm/dredger/advisory-revocation", "npm/dredger/denied-install", "npm/dredger/delete-recovery", "npm/dredger/refill", "npm/dredger/missing-source", "npm/dredger/late-copy", "npm/dredger/old-worker", "npm/dredger/old-dredger-refill", "npm/dredger/old-dredger-loss"]
    , Obligation
        "pypi/install"
        "pypi/read"
        "pypi/install"
        Supported
        ["read", "client-install"]
        ["pypi/install/wheel", "pypi/install/yanked-pin", "pypi/install/yank-negative-control", "pypi/install/sdist", "pypi/install/outage"]
    , Obligation
        "pypi/advisories"
        "pypi/advisories"
        "pypi/advisories"
        Supported
        ["read", "advisories", "client-install"]
        ["pypi/advisories/exact-fix", "pypi/advisories/canonical-fix", "pypi/advisories/deny-cve", "pypi/advisories/deny-epss"]
    , Obligation
        "telemetry"
        "npm/telemetry"
        "telemetry"
        Supported
        ["read", "mirror", "telemetry", "client-install"]
        ["telemetry/signals", "telemetry/domain-spans", "telemetry/private-fetch", "telemetry/disabled", "telemetry/export-failure", "telemetry/datadog"]
    , Obligation
        "mixed"
        "mixed/registry"
        "mixed"
        Supported
        ["read", "mirror", "client-install"]
        ["mixed/alternating-clients"]
    , Obligation "pypi/mirror" "pypi/write" "mirror" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/765") ["mirror"] []
    , Obligation "pypi/publication" "pypi/write" "publication" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/1133") ["publication"] []
    , Obligation "s3/packages" "s3/packages" "publication" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/1239") ["publication"] []
    ]
