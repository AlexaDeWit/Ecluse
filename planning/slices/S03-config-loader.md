---
id: S03
title: Config model & fail-fast loader
milestone: M0 — Shell, handles & foundations
status: in-review
depends-on: [S02, S05]
test-tier: [unit]
arch-refs:
  - docs/architecture/configuration.md#configuration
  - docs/architecture/configuration.md#rule-policy
  - docs/architecture/configuration.md#the-default-policy
  - docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown
  - docs/architecture/hosting.md#mounts
pr: null
---

# S03 — Config model & fail-fast loader

> Milestone **M0** · depends on: [S02](S02-handle-interfaces.md), [S05](S05-rules-precedence.md) · tier: unit

**Goal.** Parse process configuration into a validated, structured value the
composition root consumes: environment variables (via `envparse`, aggregating
errors) plus the structured **config document** (JSON, strict decoders) carrying
the **mount map** and the **rule policy**. The rule policy is a **named map merged
over a built-in default**; single-mount env vars desugar to a one-entry mount map,
and an env-only deployment with no document still runs on the default policy. Fail
fast and reject the unknown — including unresolvable merge references.

**Acceptance criteria.**
- [ ] Env-var layer parses every variable in the table (`PROXY_PORT`,
  `PRIVATE_UPSTREAM_URL`, `PUBLIC_UPSTREAM_URL`, `MIRROR_TARGET_URL`,
  `MIRROR_QUEUE_PROVIDER`, `MIRROR_QUEUE_URL`, `AWS_REGION`, `PROXY_AUTH_TOKEN`,
  `PROXY_HELP_MESSAGE`, `CVE_SYNC_INTERVAL_SECONDS`, …) with documented defaults;
  **errors aggregate** (all problems reported in one run). _(`PROXY_RULES` is
  retired — rules live in the structured document.)_ — _configuration.md#configuration_
- [ ] Structured **config document** decoded from a JSON file *or* a `PROXY_CONFIG`
  env blob (same schema), carrying both the **mount map** (each mount = prefix +
  external base URL + three-registry tuple with per-endpoint credential provider +
  queue backend, plus an optional per-mount rule refinement) and the top-level
  **rule policy**. — _configuration.md#configuration, hosting.md#mounts_
- [ ] Single-mount env vars **desugar** to a one-entry mount map; with no document
  supplied, the built-in **default rule policy** still applies (env-only launch
  case). — _configuration.md#configuration_
- [ ] **Rule policy is a named map merged over a built-in default**: a known default
  name takes a **partial patch** (override precedence/values); a new name must carry
  a full `type` (**add**); `"enabled": false` **suppresses** a default. — _configuration.md#rule-policy, #the-default-policy_
- [ ] **Strict + fail-loud**: unknown rule `type`, unknown JSON keys, malformed
  values (bad URL, non-integer precedence, unparseable JSON), **and an unresolvable
  merge reference** (typo'd default name, suppress/patch of a non-existent rule, an
  add missing its `type`) are all **rejected**, not skipped. A typo'd rule must fail
  startup, not vanish. — _configuration.md#validation-fail-fast-reject-the-unknown_
- [ ] Rule entries decode to the precedence-carrying model (default precedence per
  type when omitted), consuming the **S05** precedence model. — _configuration.md#rule-policy_
- [ ] **Secrets never in structured config** — tokens only via env; assert the
  decoder rejects a token field inside the document.

**File scope.**
- `src/Ecluse/Config.hs` — config types, env parser, JSON schema + strict decoders, validation aggregation.
- `ecluse.cabal` — add `aeson`, `envparse`.
- `test/unit/Ecluse/ConfigSpec.hs` — present/absent/malformed env; strict-decode rejections; single-mount desugaring; error aggregation.

**Test tier.** Unit — table-driven over valid/invalid configs; assert *which* errors
surface and that all surface at once.

**Notes / risks.** S03 now formally **depends on S05** (the precedence-carrying rule
model). Keep the **merge** here (named-map patch over the default) but source the
**default policy value** from the rules layer so config doesn't hard-code rule
semantics; the launch default is just `min-age` (`AllowIfPublishedBefore`), and the
effectful `AllowIfRemediatesCve` member joins the default only once **S23** lands —
guard the decoder so naming an effectful rule before then is a clean "unknown type",
not a crash. "Reject the unknown" via aeson means turning off permissive defaults
(no `rejectUnknownFields`-style escape); pick the explicit-key-set approach, and test
the merge's fail-loud references too. The mount-map shape must match what S20's
composition root needs — keep the backend-selection enum here (`sqs`/`pubsub`,
`codeartifact`/`static`/`adc`).
