---
id: S03
title: Config model & fail-fast loader
milestone: M0 — Shell, seams & foundations
status: not-started
depends-on: [S02]
test-tier: [unit]
arch-refs:
  - docs/architecture/configuration.md#configuration
  - docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown
  - docs/architecture/configuration.md#rule-configuration-format
  - docs/architecture/hosting.md#mounts
pr: null
---

# S03 — Config model & fail-fast loader

> Milestone **M0** · depends on: [S02](S02-seam-interfaces.md) · tier: unit

**Goal.** Parse process configuration into a validated, structured value the
composition root consumes: environment variables (via `envparse`, aggregating
errors) plus the structured **mount map** (JSON, strict decoders), with the
single-mount env vars desugaring to a one-entry mount map. Fail fast and reject the
unknown.

**Acceptance criteria.**
- [ ] Env-var layer parses every variable in the table (`PROXY_PORT`,
  `PRIVATE_UPSTREAM_URL`, `PUBLIC_UPSTREAM_URL`, `MIRROR_TARGET_URL`,
  `MIRROR_QUEUE_PROVIDER`, `MIRROR_QUEUE_URL`, `AWS_REGION`, `PROXY_AUTH_TOKEN`,
  `PROXY_RULES`, `PROXY_HELP_MESSAGE`, `CVE_SYNC_INTERVAL_SECONDS`, …) with
  documented defaults; **errors aggregate** (all problems reported in one run). —
  _configuration.md#configuration_
- [ ] Structured **mount map** decoded from a JSON file *or* a `PROXY_CONFIG` env
  blob (same schema): each mount = prefix + external base URL + three-registry
  tuple (with per-endpoint credential provider selection) + queue backend + rule
  set. — _configuration.md#configuration, hosting.md#mounts_
- [ ] Single-mount env vars **desugar** to a one-entry mount map (the launch common
  case). — _configuration.md#configuration_
- [ ] **Strict** decoders: unknown rule `type`, unknown JSON keys, and malformed
  values (bad URL, non-integer precedence, unparseable JSON) are **rejected**, not
  skipped. A typo'd deny rule must fail startup, not vanish. — _configuration.md#validation-fail-fast-reject-the-unknown_
- [ ] Rule config JSON → `[Rule]` with per-rule `precedence` (default per type when
  omitted), consuming the S05 precedence model. — _configuration.md#rule-configuration-format_
- [ ] **Secrets never in structured config** — tokens only via env; assert the
  decoder rejects a token field inside the mount JSON.

**File fence.**
- `src/Ecluse/Config.hs` — config types, env parser, JSON schema + strict decoders, validation aggregation.
- `ecluse.cabal` — add `aeson`, `envparse`.
- `test/unit/Ecluse/ConfigSpec.hs` — present/absent/malformed env; strict-decode rejections; single-mount desugaring; error aggregation.

**Test tier.** Unit — table-driven over valid/invalid configs; assert *which* errors
surface and that all surface at once.

**Notes / risks.** Coordinate the rule-config decoder with **S05** (precedence
field) — land S05 first or stub the precedence default behind it. "Reject the
unknown" via aeson means turning off permissive defaults (no
`rejectUnknownFields`-style escape); pick the explicit-key-set approach and test it.
The mount-map shape must match what S20's composition root needs to select backends
— keep the backend-selection enum here (`sqs`/`pubsub`, `codeartifact`/`static`/`adc`).
