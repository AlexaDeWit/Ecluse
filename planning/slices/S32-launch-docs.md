---
id: S32
title: Launch docs & deployment runbook
milestone: M8, Release hardening
status: not-started
depends-on: [S20]
test-tier: []
arch-refs:
  - docs/architecture/configuration.md
  - docs/architecture/observability.md#datadog-deployment-operator
  - docs/architecture/release-supply-chain.md#releases--container-image
pr: null
---

# S32, Launch docs & deployment runbook

> Milestone **M8** · depends on: [S20](S20-aws-composition.md) · tier: n/a (docs)

**Goal.** The operator-facing documentation that makes the launched proxy
deployable: a complete env/mount-config reference with examples, the health/readiness
contract, the Datadog Operator recipe (optional), and the `release` environment +
`DOCKERHUB_*` secret setup that the publish workflow needs.

**Acceptance criteria.**
- [ ] A deployment guide: full env-var reference + a worked mount-config JSON
  example; the three client-auth modes; the credential-flow authority model
  summarised.  _configuration.md_
- [ ] Health/readiness/liveness probe documentation (incl. CVE first-sync gating). 
- [ ] The optional Datadog Operator deployment recipe (OTLP receiver + node-local
  Agent endpoint + JSONL logs), clearly marked optional.  _observability.md#datadog-deployment-operator_
- [ ] The `release` GitHub Environment + `DOCKERHUB_USERNAME`/`DOCKERHUB_TOKEN`
  machine-account setup documented so the publish workflow can succeed.   _docs/architecture/release-supply-chain.md#releases--container-image_
- [ ] `README.md` updated to reflect launch status.

**File scope.**
- `README.md`, `CONTRIBUTING.md`, deployment + release-secret docs.
- `docs/`, a deployment/runbook doc if it earns its own page.

**Test tier.** None (docs), validated by a clean deploy following the guide.

**Notes / risks.** This is the "make it usable by someone else" slice. The README is
the *current-architecture* doc (per AGENTS.md), update it to present-tense launched
state; keep `docs/architecture.md` in end-state voice. Until the `release`
environment + secrets exist, the publish workflow is expected to fail at the push
step by design, document that explicitly.
