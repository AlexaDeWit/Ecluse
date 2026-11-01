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
        ["npm/install/default-denial", "npm/install/external-dependency-denial", "npm/install/lifecycle-script-control", "npm/install/deprecation-relay", "npm/install/deprecation-negative-control", "npm/install/integrity-refusal", "npm/install/head-no-effect", "npm/install/metadata-preservation", "npm/install/latest-preservation", "npm/install/publication-unconfigured", "npm/install/mal-forced-source-required", "npm/install/mal-manual-source-required"]
    , Obligation
        "npm/mirror"
        "npm/registry"
        "npm/mirror"
        Supported
        ["read", "mirror", "client-install"]
        ["npm/mirror/cross-target-install", "npm/mirror/dependency-graph", "npm/mirror/module-exports", "npm/mirror/linked-executable", "npm/mirror/origin-evidence", "npm/mirror/installed-tree", "npm/mirror/empty-store-control"]
    , Obligation
        "npm/publish"
        "npm/registry"
        "npm/publish"
        Supported
        ["read", "publication", "client-install"]
        ["npm/publish/publish-install", "npm/publish/scope-refusal", "npm/publish/non-upload-refusal"]
    , Obligation
        "npm/dredger"
        "npm/maintenance"
        "npm/dredger"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        ["npm/dredger/skipped-admission-recovery", "npm/dredger/inventory", "npm/dredger/admission-only", "npm/dredger/denied-versions", "npm/dredger/dry-run", "npm/dredger/consent-refusal", "npm/dredger/first-party", "npm/dredger/two-stores", "npm/dredger/advisory-revocation", "npm/dredger/mal-revocation", "npm/dredger/denied-install", "npm/dredger/delete-recovery", "npm/dredger/refill", "npm/dredger/missing-source", "npm/dredger/late-copy", "npm/dredger/old-worker", "npm/dredger/old-dredger-refill", "npm/dredger/old-dredger-loss"]
    , Obligation
        "pypi/install"
        "pypi/read"
        "pypi/install"
        Supported
        ["read", "client-install"]
        ["pypi/install/wheel", "pypi/install/metadata-companion", "pypi/install/tampered-companion", "pypi/install/yanked-pin", "pypi/install/yank-negative-control", "pypi/install/sdist", "pypi/install/outage"]
    , Obligation
        "pypi/advisories"
        "pypi/advisories"
        "pypi/advisories"
        Supported
        ["read", "advisories", "client-install"]
        ["pypi/advisories/exact-fix", "pypi/advisories/partial-fix", "pypi/advisories/deny-cve", "pypi/advisories/deny-epss", "pypi/advisories/mal-denial"]
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
        ["npm/installation/accepted-install", "npm/installation/default-denial", "npm/installation/artifact-integrity", "npm/installation/artifact-head", "npm/installation/artifact-identity-coding"]
    , Obligation
        "pypi/installation"
        "pypi/read"
        "pypi/installation"
        Supported
        ["read", "client-install"]
        ["pypi/installation/accepted-install", "pypi/installation/default-denial", "pypi/installation/artifact-integrity", "pypi/installation/artifact-head", "pypi/installation/artifact-identity-coding"]
    , Obligation
        "npm/advisories"
        "npm/advisories"
        "npm/advisories"
        Supported
        ["read", "mirror", "advisories", "client-install"]
        ["npm/advisories/deny-cve", "npm/advisories/deny-epss", "npm/advisories/exact-fix", "npm/advisories/partial-fix", "npm/advisories/mal-denial"]
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
    , Obligation
        "pypi/mirror"
        "pypi/write"
        "pypi/mirror"
        Supported
        ["read", "mirror", "client-install", "telemetry"]
        ["pypi/mirror/cross-target-install", "pypi/mirror/dependency-graph", "pypi/mirror/module-exports", "pypi/mirror/linked-executable", "pypi/mirror/origin-evidence", "pypi/mirror/installed-tree", "pypi/mirror/empty-store-control"]
    , Obligation
        "pypi/mirror-native"
        "pypi/write"
        "pypi/mirror-native"
        Supported
        ["read", "mirror", "client-install", "telemetry"]
        ["pypi/mirror-native/read-roundtrip", "pypi/mirror-native/lost-reply", "pypi/mirror-native/interruption", "pypi/mirror-native/yanked-greatest", "pypi/mirror-native/yank-negative-control"]
    , Obligation
        "pypi/publish"
        "pypi/publication"
        "pypi/publish"
        Supported
        ["read", "publication", "client-install"]
        ["pypi/publish/publish-install", "pypi/publish/scope-refusal", "pypi/publish/non-upload-refusal"]
    , Obligation
        "pypi/publish-native"
        "pypi/publication"
        "pypi/publish-native"
        Supported
        ["read", "publication", "client-install"]
        ["pypi/publish-native/conflicting-names", "pypi/publish-native/hidden-parts", "pypi/publish-native/unconfigured"]
    , Obligation
        "pypi/telemetry-write"
        "pypi/write-telemetry"
        "pypi/telemetry-write"
        Supported
        ["read", "mirror", "client-install", "telemetry"]
        ["pypi/telemetry/domain-spans", "pypi/telemetry/private-fetch"]
    , Obligation
        "pypi/dredger"
        "pypi/maintenance"
        "pypi/dredger"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        (map ("pypi/dredger/" <>) ["inventory", "admission-only", "denied-versions", "dry-run", "consent-refusal", "first-party", "two-stores", "advisory-revocation", "mal-revocation", "denied-install", "delete-recovery", "refill", "missing-source", "late-copy", "old-worker", "old-dredger-refill", "old-dredger-loss"])
    , Obligation
        "cargo/installation"
        "cargo/read"
        "cargo/installation"
        Supported
        ["read", "client-install"]
        (map ("cargo/installation/" <>) ["accepted-install", "default-denial", "artifact-integrity", "artifact-head", "artifact-identity-coding"])
    , Obligation
        "cargo/advisories"
        "cargo/write"
        "cargo/advisories"
        Supported
        ["read", "advisories", "client-install"]
        (map ("cargo/advisories/" <>) ["exact-fix", "partial-fix", "deny-cve", "deny-epss", "mal-denial"])
    , Obligation
        "cargo/mirror"
        "cargo/write"
        "cargo/mirror"
        Supported
        ["read", "mirror", "client-install"]
        (map ("cargo/mirror/" <>) ["cross-target-install", "dependency-graph", "module-exports", "linked-executable", "origin-evidence", "installed-tree", "empty-store-control"])
    , Obligation
        "cargo/publish"
        "cargo/write"
        "cargo/publish"
        Supported
        ["read", "publication", "client-install"]
        (map ("cargo/publish/" <>) ["publish-install", "scope-refusal", "non-upload-refusal"])
    , Obligation
        "cargo/dredger"
        "cargo/maintenance"
        "cargo/dredger"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        (map ("cargo/dredger/" <>) ["inventory", "admission-only", "denied-versions", "dry-run", "consent-refusal", "first-party", "two-stores", "advisory-revocation", "mal-revocation", "denied-install", "delete-recovery", "refill", "missing-source", "late-copy", "old-worker", "old-dredger-refill", "old-dredger-loss"])
    , Obligation
        "cargo/telemetry"
        "cargo/read"
        "cargo/telemetry"
        Supported
        ["read", "telemetry", "client-install"]
        (map ("cargo/telemetry/" <>) ["signals", "disabled", "export-failure", "datadog"])
    , Obligation
        "cargo/telemetry-write"
        "cargo/write"
        "cargo/telemetry-write"
        Supported
        ["read", "mirror", "client-install", "telemetry"]
        (map ("cargo/telemetry/" <>) ["domain-spans", "private-fetch"])
    , Obligation
        "cargo/native"
        "cargo/write"
        "cargo/native"
        Supported
        ["read", "mirror", "publication", "client-install"]
        (map ("cargo/native/" <>) ["current-client", "floor-client", "asset-routing", "metadata-no-archives", "denial-control", "features", "targets", "lock-checksums", "msrv", "local-discovery", "merged-private", "raw-auth", "private-auth-required", "public-token-control", "placeholder-token", "publication-framing", "dependency-sources", "publisher-configuration"])
    , Obligation
        "cargo/mirror-proof"
        "cargo/write"
        "cargo/mirror-proof"
        Supported
        ["read", "mirror", "client-install"]
        (map ("cargo/mirror-proof/" <>) ["source-roundtrip", "pinned-yank", "yank-negative-control", "lost-reply", "missing-archive", "checksum-mutation", "missing-state", "corrupt-state", "private-auth"])
    , Obligation
        "cargo/dredger-native-safety"
        "cargo/maintenance"
        "cargo/native-safety"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        (map ("cargo/dredger-native-safety/" <>) ["deletion-cap", "unsafe-upstream", "native-yank-not-deletion", "authorisation"])
    , Obligation
        "rubygems/installation"
        "rubygems/read"
        "rubygems/installation"
        Supported
        ["read", "client-install"]
        (map ("rubygems/installation/" <>) ["accepted-install", "default-denial", "artifact-integrity", "artifact-head", "artifact-identity-coding"])
    , Obligation
        "rubygems/mirror"
        "rubygems/write"
        "rubygems/mirror"
        Supported
        ["read", "mirror", "client-install"]
        (map ("rubygems/mirror/" <>) ["cross-target-install", "dependency-graph", "module-exports", "linked-executable", "origin-evidence", "installed-tree", "empty-store-control"])
    , Obligation
        "rubygems/publish"
        "rubygems/publication"
        "rubygems/publish"
        Supported
        ["read", "publication", "client-install"]
        (map ("rubygems/publish/" <>) ["publish-install", "scope-refusal", "non-upload-refusal"])
    , Obligation
        "rubygems/advisories"
        "rubygems/write"
        "rubygems/advisories"
        Supported
        ["read", "advisories", "client-install"]
        (map ("rubygems/advisories/" <>) ["deny-cve", "deny-epss", "mal-denial", "exact-fix", "partial-fix"])
    , Obligation
        "rubygems/dredger"
        "rubygems/maintenance"
        "rubygems/dredger"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        (map ("rubygems/dredger/" <>) ["inventory", "admission-only", "denied-versions", "dry-run", "consent-refusal", "first-party", "two-stores", "advisory-revocation", "mal-revocation", "denied-install", "delete-recovery", "refill", "missing-source", "late-copy", "old-worker", "old-dredger-refill", "old-dredger-loss"])
    , Obligation
        "rubygems/telemetry"
        "rubygems/read"
        "rubygems/telemetry"
        Supported
        ["read", "telemetry", "client-install"]
        (map ("rubygems/telemetry/" <>) ["signals", "disabled", "export-failure", "datadog"])
    , Obligation
        "rubygems/telemetry-write"
        "rubygems/write"
        "rubygems/telemetry-write"
        Supported
        ["read", "mirror", "client-install", "telemetry"]
        (map ("rubygems/telemetry/" <>) ["domain-spans", "private-fetch"])
    , Obligation
        "rubygems/native"
        "rubygems/read"
        "rubygems/native"
        Supported
        ["read", "client-install"]
        (map ("rubygems/native/" <>) ["minimum-client", "current-client", "compact-required", "info-required", "minimum-cache", "current-cache", "compact-replacement", "platforms", "checksum-failure", "prerelease", "ruby-requirement", "direct-denial", "native-surface", "metadata-no-archives", "private-authority", "native-extension"])
    , Obligation
        "rubygems/mirror-proof"
        "rubygems/write"
        "rubygems/mirror-proof"
        Supported
        ["read", "mirror", "client-install"]
        (map ("rubygems/mirror-proof/" <>) ["source-roundtrip", "lost-reply", "interruption", "upstream-removal", "conclusive-absence", "missing-archive", "checksum-mutation", "missing-state", "missing-compact", "corrupt-state", "private-auth"])
    , Obligation
        "rubygems/dredger-native-safety"
        "rubygems/maintenance"
        "rubygems/native-safety"
        Supported
        ["read", "mirror", "maintenance", "advisories"]
        (map ("rubygems/dredger-native-safety/" <>) ["deletion-cap", "unsafe-upstream", "native-yank-not-deletion", "authorisation", "multi-platform-deletion"])
    , Obligation
        "rubygems/relay"
        "rubygems/read"
        "rubygems/relay"
        Supported
        ["read", "client-install"]
        (map ("rubygems/relay/" <>) ["initial-digest", "private-401", "private-403", "private-digest", "private-miss", "known-length", "chunked", "bound-known", "bound-chunked", "origin-304", "origin-206", "conditional-fetch", "conditional-mismatch", "conditional-revision", "range", "head", "head-conditional", "head-private-auth"])
    , Obligation
        "rubygems/registration"
        "rubygems/contracts"
        "rubygems/registration"
        Supported
        ["contract-registration"]
        (map ("rubygems/registration/" <>) ["provider-omission", "case-omission", "capability-omission"])
    , Obligation "s3/packages" "s3/packages" "publication" (Planned "https://github.com/AlexaDeWit/Ecluse/issues/1239") ["publication"] []
    ]
