---
id: S09
title: URL rewrite + packument filtering
milestone: M1 — npm protocol adapter
status: merged
depends-on: [S05, S07]
test-tier: [unit]
arch-refs:
  - docs/architecture/rules-engine.md#applying-verdicts-to-a-packument
  - docs/architecture/hosting.md#the-load-bearing-requirement-url-rewriting
  - docs/research/reverse-engineering/npm.md#8-version--availability-resolution
pr: 89
---

# S09 — URL rewrite + packument filtering

> Milestone **M1** · depends on: [S05](S05-rules-precedence.md), [S07](S07-npm-projection.md) · tier: unit

**Goal.** The two pure transforms a public-upstream packument needs before it is
served: rewrite embedded artifact URLs under the mount's prefix, and apply rule
verdicts across all versions (the deny-by-default filtered projection).

**Acceptance criteria.**
- [x] **URL rewriting**: `dist.tarball` rewritten to `{mount-base}/{pkg}/-/{file}`
  so artifacts flow through the proxy (and same-host auth is preserved). The mount's
  external base URL is supplied (config, S03). — _hosting.md#the-load-bearing-requirement-url-rewriting_
- [x] **Filter `versions`**: every version evaluated; denied (and, later,
  undecidable — S21) versions removed from `versions` **and** `time`. —
  _rules-engine.md#applying-verdicts-to-a-packument_
- [x] **Repoint `dist-tags`**: `latest` resolved by `Ecluse.Version.selectLatest`
  (keep-unless-denied, stable-preferring) — the upstream `latest` is kept while it
  survives and only repointed, to the highest *stable* survivor, when itself denied;
  other tags pointing at a removed version are **dropped**, not repointed. —
  _rules-engine.md#applying-verdicts-to-a-packument_
- [x] **No survivors** → return `NoSurvivors [Decision]` (the `FilterResult` sum) for
  the serve layer to map to a status (`403` all-by-policy; `503` for a transient
  cause once S21 lands). This slice does not choose the status. —
  _rules-engine.md#applying-verdicts-to-a-packument_
- [x] Coherence preserved: `dist-tags.latest` is always a key of `versions`; `time`
  has an entry for every surviving version. — _npm.md#8-version--availability-resolution_
- [x] **Lossless passthrough of unmodeled fields** (PR #23). Filtering/rewriting must
  not drop wire keys Écluse does not model (the synthesized-packument
  `additionalProperties: true` passthrough): operate **structurally over the raw
  `Value`** (or carry a `KeyMap Value` remainder), removing denied versions and
  rewriting `dist.tarball` in place — never rebuild the served body from a lossy typed
  model. — _api-surface.md#the-synthesized-packument-schema--the-trust-boundary_

**File scope.**
- `src/Ecluse/Registry/Npm/Filter.hs` — `rewriteTarballUrls`, `filterPackument` (verdict application), result type for the no-survivors case.
- `test/unit/Ecluse/Registry/Npm/FilterSpec.hs` — filtering (drop denied, repoint latest, drop stale tags, no-survivors), rewriting, coherence properties.

**Test tier.** Unit — properties: filtered packument never references a denied
version; `latest` always present-and-surviving; rewriting is idempotent and
prefix-correct.

**Notes / risks.** Repointing `latest` to an older surviving version is the intended
resilience downgrade — document it. Because the served body differs from upstream,
the **own-ETag** computation is the web layer's job (S13) — do not relay upstream's
validator for a filtered body. Keep the `Unavailable` filtering path stubbed to the
deny path until S21 lands (note it explicitly; do not fake the transient status).

**As built.** `latest` uses the shared `Ecluse.Version.selectLatest`
(keep-unless-denied, stable-preferring), not `compareVersions` — aligning with the
updated [rules-engine.md](../../docs/architecture/rules-engine.md#applying-verdicts-to-a-packument).
A `dist-tags` that is absent *or* present-but-`null` is treated as empty, so the
coherence promise (a resolvable `latest`) holds on that malformed-upstream edge.
`time` is pruned by *removal* of the denied version keys rather than *retention* of
the survivors, so its unmodeled bookkeeping keys (`created`/`modified`) are relayed
unchanged (lossless passthrough). The undecidable path is dropped like a denial
until S21.

**Scope boundary.** This slice filters/rewrites a **single (public) packument** —
the gated set. Combining that filtered set with the **trusted private** set is the
cross-upstream **merge**, a separate ecosystem-agnostic core slice
([S33](S33-packument-merge.md)); do **not** fold merging in here. `latest`-repointing
within the public set still happens here; the final `latest` over the merged union
is S33's.
