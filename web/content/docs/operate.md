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

A PyPI `publicationTarget` now names its registry root in `url` and its upload endpoint in `uploadUrl`, as a PyPI `mirrorTarget` already does. If your `url` names the upload endpoint, such as `https://first-party.internal/legacy/`, move that address to `uploadUrl` and set `url` to the registry root. On CodeArtifact, both can name `/pypi/{repository}/`. `ecluse proxy` refuses to boot until `uploadUrl` is set, and `check-config` names the refusal.

An npm or RubyGems `mirrorTarget` or `publicationTarget` that sets `uploadUrl` now refuses at load, because those protocols derive the upload address from `url`. Remove the key, and check that `url` names the registry you write to.

A rollback must account for queued messages and advisory data, as well as the container image.

## Operational references {#details}

{{ task_link(path="@/docs/operations.md#memory-plan-and-runtime-sizing", title="Memory and runtime sizing", description="Budgets, admission controls, capacity, and detailed arithmetic.") }}

{{ task_link(path="@/docs/operations.md#advisory-publication-guard", title="Advisory publication recovery", description="Publication guards, rebaselining, and restoring artifacts.") }}

{{ task_link(path="@/docs/operations.md#mirror-receipts-and-their-visibility", title="Mirror retries and poison jobs", description="Visibility renewal, redelivery, and retired work.") }}

{{ task_link(path="@/docs/remove.md", title="Remove blocked packages", description="Preview, consent, deletion, and verification.") }}
