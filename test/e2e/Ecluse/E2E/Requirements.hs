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
        ["pypi/advisories/exact-fix", "pypi/advisories/deny-cve", "pypi/advisories/deny-epss"]
    , Obligation
        "npm/telemetry-read"
        "npm/telemetry"
        "npm/telemetry-read"
        Supported
        ["read", "mirror", "telemetry", "client-install"]
        ["npm/telemetry/signals", "npm/telemetry/disabled", "npm/telemetry/export-failure", "npm/telemetry/datadog"]
    , Obligation
        "mixed"
        "mixed/registry"
        "mixed"
        Supported
        ["read", "mirror", "client-install"]
        ["mixed/alternating-clients"]
    , Obligation
        "npm/installation"
        "npm/read"
        "npm/installation"
        Supported
        ["read", "client-install"]
        ["npm/installation/accepted-install", "npm/installation/default-denial", "npm/installation/artifact-integrity", "npm/installation/artifact-head"]
    , Obligation
        "pypi/installation"
        "pypi/read"
        "pypi/installation"
        Supported
        ["read", "client-install"]
        ["pypi/installation/accepted-install", "pypi/installation/default-denial", "pypi/installation/artifact-integrity", "pypi/installation/artifact-head"]
    , Obligation
        "npm/advisories"
        "npm/advisories"
        "npm/advisories"
        Supported
        ["read", "advisories", "client-install"]
        ["npm/advisories/deny-cve", "npm/advisories/deny-epss", "npm/advisories/exact-fix"]
    , Obligation
        "pypi/advisory-native"
        "pypi/advisories"
        "pypi/advisory-native"
        Supported
        ["read", "advisories", "client-install"]
        ["pypi/advisories/canonical-fix"]
    , Obligation
        "npm/telemetry-write"
        "npm/telemetry"
        "npm/telemetry-write"
        Supported
        ["read", "mirror", "telemetry", "client-install"]
        ["npm/telemetry/domain-spans", "npm/telemetry/private-fetch"]
    , Obligation
        "pypi/telemetry"
        "pypi/telemetry"
        "pypi/telemetry"
        Supported
        ["read", "telemetry", "client-install"]
        ["pypi/telemetry/signals", "pypi/telemetry/disabled", "pypi/telemetry/export-failure", "pypi/telemetry/datadog"]
    , Obligation "pypi/mirror" "pypi/write" "mirror" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/765") ["mirror"] []
    , Obligation "pypi/publication" "pypi/write" "publication" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/1133") ["publication"] []
    , Obligation "s3/packages" "s3/packages" "publication" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/1239") ["publication"] []
    ]
