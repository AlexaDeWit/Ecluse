+++
title = "Operate and upgrade Écluse"
description = "Monitor package serving and background work separately. Keep configuration and advisory requirements aligned during upgrades."

weight = 35

[extra]
nav_title = "Operate and upgrade"
group = "Operate and troubleshoot"
kind = "Guide"
reference = "/docs/operations/"
+++

## Monitor outcomes {#monitor}

| Area | Watch for |
| --- | --- |
| Package serving | Request failures and policy denials by ecosystem. Distinguish an intentional deny from unavailable evidence. |
| Advisories | Publication age, failed downloads, rejected artifacts, and feed outages. |
| Mirroring | Queue depth, retry activity, retired jobs, and a stalled consume loop. |
| Cleanup | Incomplete per-target results, refused deletions, and a latched deletion-cap halt. |

Logs use JSON by default. OpenTelemetry export is opt-in. Use `/livez` and `/readyz` for orchestration, with separate freshness and progress alerts.

## Prepare an upgrade {#upgrade}

1. Read the release notes for configuration, advisory-schema, and queue compatibility changes.
2. Verify the release image and pin its digest. Rebuild your derived configuration image.
3. Run `check-config` with the deployed settings and inspect every role's warnings.
4. Prepare any new advisory artifacts before consumers need them.
5. Roll out compatible readers and workers in the order required by the release.
6. Repeat the [deployment checks](@/docs/verify.md).

## Compatibility changes to check {#compatibility}

Advisory schema 5 uses ecosystem-specific keys such as `npm-osv-schema5.db`. Consumers do not migrate or alias schema 4 files.

Queue schema 2 removes source URLs from jobs. Deploy compatible workers first and stop old workers before new producers enqueue schema 2 jobs.

A PyPI `publicationTarget` now reads from `url` and writes to `uploadUrl`. The [endpoint checks](@/docs/configuration.md#endpoint-collisions) now compare that `url`, so `ecluse dredger` refuses a publication target that names the same registry as its mount's `privateUpstream`.

An npm or RubyGems `mirrorTarget` or `publicationTarget` that sets `uploadUrl` now refuses at load, because those protocols derive the upload address from `url`. Remove the key, and check that `url` names the registry you write to.

`limits.maxVersionCount` now counts every file of a PyPI listing and every row of a RubyGems `/info` document, so a pin below a package's file or row count refuses that package. A PyPI listing refuses as soon as its read passes the pin, rather than after reading the whole document.

`limits.maxArtifactCount` is removed, and no key replaces it. A config document or an `ECLUSE_LIMITS__MAX_ARTIFACT_COUNT` environment variable that still sets it refuses at load, so remove both ([Upgrade existing limits and admission pins](@/docs/operations.md#upgrade-existing-limits-and-admission-pins)).

The npm mount ships a second admission-only default, `deny-external-dependencies`. A public npm version whose `dependencies`, `optionalDependencies` or `peerDependencies` name a git source or a remote tarball URL stops resolving, and npm publication refuses such a release with `400`. Copies your stores already hold keep serving. Pin a package you still need with an `AllowByIdentity` rule at precedence above 300, or disable the default under `mounts.npm.rules` ([Rule policy](@/docs/configuration.md#rule-policy)).

A rollback must account for queued messages and advisory data, as well as the container image.

## Operational references {#details}

{{ task_link(path="@/docs/operations.md#memory-plan-and-runtime-sizing", title="Memory and runtime sizing", description="Budgets, admission controls, capacity, and detailed arithmetic.") }}

{{ task_link(path="@/docs/operations.md#advisory-publication-guard", title="Advisory publication recovery", description="Publication guards, rebaselining, and restoring artifacts.") }}

{{ task_link(path="@/docs/operations.md#mirror-receipts-and-their-visibility", title="Mirror retries and poison jobs", description="Visibility renewal, redelivery, and retired work.") }}

{{ task_link(path="@/docs/remove.md", title="Remove blocked packages", description="Preview, consent, deletion, and verification.") }}
