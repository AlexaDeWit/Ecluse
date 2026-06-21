---
id: S02
title: Seam interfaces + in-memory doubles
milestone: M0 — Shell, seams & foundations
status: not-started
depends-on: []
test-tier: [unit]
arch-refs:
  - docs/architecture/cloud-backends.md#seams-records-of-functions
  - docs/architecture/registry-model.md#registry-abstraction
  - docs/architecture/cloud-backends.md#queue-abstraction
  - docs/architecture/cloud-backends.md#credential-provider
  - docs/architecture/technology-stack.md#key-decisions
pr: null
---

# S02 — Seam interfaces + in-memory doubles

> Milestone **M0** · depends on: — (root) · tier: unit

**Goal.** Define the three swappable backends as records of functions (the Handle
pattern), each returning `IO` (never `App`), plus their payload types and
in-memory test doubles. This is "seams before consumers": it unblocks the npm
adapter, the web layer, the cloud backends, and the worker to be built in parallel
against stable interfaces.

**Acceptance criteria.**
- [ ] `RegistryClient` record defined with the exact field set from the spec
  (`fetchMetadata`, `fetchArtifact`, `publishArtifact`, `parsePackageInfo`,
  `parseVersionDetails`, `parseVersionList`); effectful fields return `IO`, `parse*`
  fields are pure. — _registry-model.md#registry-abstraction_
- [ ] Supporting types: `RegistryResponse`, `ParseError`, `PublishError`. (`PackageInfo`
  is introduced in S07; reference it here only via an `hs-boot`-free ordering — keep
  the seam in a module that can import the domain types.) — _registry-model.md_
- [ ] `MirrorQueue` record (`enqueue`/`receive`/`ack`/`extendVisibility`) with
  `MirrorJob`, `QueueMessage`, opaque `ReceiptHandle`, `Seconds`. Conventions
  documented in Haddock: `enqueue` best-effort, retry-is-don't-ack, no `nack`. —
  _cloud-backends.md#queue-abstraction_
- [ ] `CredentialProvider` (`newtype` over `currentToken :: IO AuthToken`) with
  `AuthToken { secret, expiresAt }` and an opaque `Secret` whose `Show` is redacted
  (never prints the token). — _cloud-backends.md#credential-provider_
- [ ] In-memory doubles: `newInMemoryQueue :: IO MirrorQueue` (STM-backed), a
  `staticToken`-style `CredentialProvider`, and a `RegistryClient` test double
  driven by fixtures — usable by every downstream slice's tests.
- [ ] Every type/field has Haddock; seams carry the `IO`-not-`App` rationale.

**File fence.**
- `src/Ecluse/Registry.hs` — `RegistryClient`, `RegistryResponse`, `ParseError`, `PublishError`.
- `src/Ecluse/Queue.hs` — `MirrorQueue`, `MirrorJob`, `QueueMessage`, `ReceiptHandle`, `Seconds`, `newInMemoryQueue`.
- `src/Ecluse/Credential.hs` — `CredentialProvider`, `AuthToken`, `Secret`, in-memory provider.
- `ecluse.cabal` — add the three modules (no new external deps; `stm` via relude).
- `test/unit/Ecluse/QueueSpec.hs`, `test/unit/Ecluse/CredentialSpec.hs` — double behaviour + `Secret` redaction.

**Test tier.** Unit — the in-memory queue's FIFO/ack/redeliver-on-no-ack semantics
and `Secret` redaction are asserted; the doubles are the substrate later slices test against.

**Notes / risks.** `Secret` redaction is load-bearing (no token in any signal —
see observability.md#cardinality-and-attributes); pin it with a test now. Keep the
seam modules free of any `Env`/`App` import so backends never couple to the core
(technology-stack.md#key-decisions). `PackageInfo`/`PackageDetails` ordering: the
seam references domain types from `Ecluse.Package`/`Ecluse.Version`/(new) `PackageInfo`
— coordinate the `PackageInfo` introduction with S07 to avoid an import cycle (prefer
defining `PackageInfo` in `Ecluse.Package` or a sibling, never an `.hs-boot`).
