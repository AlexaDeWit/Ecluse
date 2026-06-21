# Rules Engine & Responses

> Part of the [Écluse architecture overview](../architecture.md).

## Rules Engine

**Deny by default; precedence decides.** Each rule carries a configurable
integer **precedence**. Every rule yields *allow*, *deny*, or *abstain* for a
given version, and the **highest-precedence rule that does not abstain wins**; at
equal precedence, a deny beats an allow. If every rule abstains, the package is
denied by default. Built-in deny rules default to a higher precedence than allow
rules, so out of the box "any deny overrides any allow" holds — but an operator
can rank a specific allow above a specific deny (e.g. to let a trusted internal
scope through an install-script deny).

Rules evaluate a single `PackageDetails` snapshot — the ecosystem-agnostic
per-version view produced by a registry adapter. A rule never sees registry wire
formats.

Rules are evaluated in two tiers:

1. **Pure rules** — evaluated against `PackageDetails` with no IO. Fast and
   deterministic. Evaluated first. This is the tier implemented today
   ([`src/Ecluse/Rules.hs`](../../src/Ecluse/Rules.hs)).
2. **Effectful rules** — may perform IO (advisory lookups, external policy
   checks). A later phase, layered on top of the pure tier.

The two tiers are a **performance ordering, not a precedence ordering**: pure
rules run first because they are cheap, then effectful rules run only where they
could still change the outcome. Once the pure tier yields a winning position at
precedence *P*, any effectful rule ranked below *P* is skipped (it cannot
outrank), and the effectful tier is skipped entirely when no effectful rule is
ranked at or above *P*. Precedence — not tier — decides who wins.

**A rule's tier is determined by where its signal lives, not only by whether it
"feels" like IO.** Many inputs are already present in the metadata an adapter
fetches for resolution — publish age, declared scope, npm's `hasInstallScript`,
a PyPI file's `packagetype == sdist` — and support **pure** rules. Others are
*not* exposed in any metadata response and must be fetched and parsed per
version. RubyGems is the motivating case: a gem's native `extensions` — its
install-time code-execution signal, the analog of npm's install scripts — appears
only in the gemspec inside the `.gem` (or the legacy `quick` Marshal spec), never
in the Compact Index or the JSON API (see
[`research/reverse-engineering/rubygems.md`](../research/reverse-engineering/rubygems.md)).
A rule over such a signal is necessarily **effectful**, even though it is
conceptually a simple per-version predicate. Guidance: `parseVersionDetails`
populates `PackageDetails` from the cheap metadata path; a signal that needs an
extra fetch belongs in the effectful tier alongside advisory lookups, and the
same logical rule (e.g. `DenyHasInstallScripts`) may therefore land in different
tiers for different ecosystems.

### Evaluation model

Each rule, applied to a `PackageDetails`, yields a `RuleOutcome`:

- **`Allow reason`** — the rule takes the position that this version is admissible.
- **`Deny reason`** — the rule takes the position that it must be blocked.
- **`Abstain reason`** — the rule has no opinion. The reason is retained for the
  audit trail.

Each rule also carries a **precedence** (an integer; higher wins). `evalRules`
selects the **highest-precedence non-abstaining rule** and takes its position —
`Approved rule reason` or `Denied rule reason`. At equal precedence a `Deny`
beats an `Allow`. If every rule abstains, the result is `DeniedByDefault reasons`
— deny-by-default, with each abstaining rule's reason collected (in order) so the
denial response can explain what was considered.

A rule that does not fire **abstains rather than deciding**, yielding the floor to
others rather than admitting or blocking on its own. Because **precedence — not
list order — decides**, the rule set is order-independent except for the
equal-precedence deny tiebreak and the order in which abstain reasons are gathered
for the audit trail.

Precedence is a **field, not an `Ord Rule` instance**: selection is a `maximumBy`
over a `(precedence, deny-before-allow)` comparator. Equal precedence is allowed
(it is the tiebreak), so a derived total `Ord` would be unlawful (non-antisymmetric)
and misleading — the same reason `Version` carries no derived `Ord` (see
[Internal Domain Model](domain-model.md)).

### Effectful-rule failure

An effectful rule does IO that can fail or hang (advisory source down, rate
limited, timeout). Each effectful rule has a short **timeout budget** (a couple of
seconds) with bounded **retry + backoff**, and a per-source **circuit breaker**:
after repeated failures the breaker trips and the rule fast-fails for a cooldown
(with periodic half-open probes), so a sustained outage neither adds latency to
every request nor hammers a down service.

When a rule the evaluator *needed* to consult cannot be evaluated, it yields
**`Unavailable`** — a fourth outcome alongside allow/deny/abstain — carrying its
**transience** (will-resolve vs not). This is **fail-closed**: an `Unavailable`
version is not admitted, because a never-vetted package should not be let in just
because the scanner is down. The blast radius is small — only packages *not yet in
the private mirror* reach this path; already-approved versions are served from the
private upstream with no rules.

How `Unavailable` surfaces depends on the request shape:

- **Packument (metadata)** — the version is simply **filtered out**, exactly like a
  denied one (see [Applying verdicts to a packument](#applying-verdicts-to-a-packument)).
  The client's resolver just picks an admitted version; no error is raised unless
  *nothing* survives.
- **Concrete artifact** — there is one specific version, so it surfaces as an error
  via the serve [Error model](web-layer.md#error-model): `503` (+`Retry-After`) when
  transient/will-resolve, `500` when not.

Every `Unavailable` and breaker trip emits an ERROR log + metric so infra can
detect and respond. The default is fail-closed; a rule may set `onError: abstain`
where availability must beat safety.

### Applying verdicts to a packument

`evalRules` decides a single version, but a metadata request returns a whole
**packument** with many versions, so verdicts are applied across it — on the
**public-upstream path only** (a private-upstream hit is served unfiltered, as
already vetted):

- **Filter the `versions` map.** Every version is evaluated; versions that are
  denied **or [undecidable](#effectful-rule-failure)** are removed from `versions`
  and from the `time` map, so a client's resolver only ever sees admitted versions.
- **Repoint `dist-tags`.** `latest` is repointed to the highest *surviving*
  version, so `npm install <pkg>` keeps resolving to the last good release. Other
  tags (`next`, `beta`, …) that point at a denied or undecidable version are
  **dropped** rather than repointed — aiming `beta` at a stable release would
  misrepresent it.
- **No survivors → 403 or 503.** If no version is admitted, the status follows the
  cause: **403** with the collected denial reasons when every rejection is by
  policy; **503** (+`Retry-After`) when any rejection was transient/undecidable (it
  may resolve on retry). Never 404 — the package exists; its versions were
  withheld. See the serve [Error model](web-layer.md#error-model).

Repointing `latest` to an older surviving version is a deliberate downgrade, and
it is the resilience posture — a brand-new, not-yet-cleared (or actively bad)
release does not silently become the default install while older, admitted
versions remain available. Because the filtered body differs from upstream's, the
proxy computes its **own** response validators (`ETag`) over the filtered body
rather than relaying upstream's (see [Web Layer](web-layer.md#web-layer)).

### Initial Rule Set

| Rule | Type | Description |
|------|------|-------------|
| `AllowIfPublishedBefore ageSeconds` | Pure | Allows a package version if it was published more than `ageSeconds` seconds ago. Default: 604800 (7 days). Guards against typosquatting and dependency confusion attacks where attackers race to publish before detection. |
| `AllowScope scope` | Pure | Unconditionally allows all packages under a given npm scope (e.g. `@myorg`). Use for internal scopes that bypass public-registry rules. |
| `DenyHasInstallScripts` | Pure | Denies any version whose metadata flags install scripts (npm's `hasInstallScript`) — a common arbitrary-code-execution vector at install time. Abstains otherwise. As a deny rule it overrides any allow. |

Further rules — e.g. `DenyIfCVE`, or effectful per-version checks like RubyGems
native `extensions` (see [above](#rules-engine)) — are added as subsequent
phases.

## CVE Subsystem

The CVE subsystem provides an interface for effectful rules to query advisory
databases. The `CVELookup` abstraction allows handlers to be backed by different
sources or caching layers.

**Recommended sources at launch:**

- **npm security advisory endpoint** (`registry.npmjs.org/-/npm/v1/security/advisories/bulk`)
  — the most direct source for npm, no API key required, returns advisories for
  requested packages in bulk.
- **OSV.dev API** — secondary source; broader coverage, also free, useful for
  cross-referencing.

Results should be cached locally in memory (with a configurable TTL) to avoid
per-request latency on advisory lookups.

## Denial Responses

When a request is denied (no allow rule matched, or a deny rule fired):

- HTTP status follows npm protocol conventions (403 for policy denials).
- The response body is a JSON object matching the npm error format:
  ```json
  {
    "error": "Package @evil/pkg@1.0.0 was denied: AllowIfPublishedBefore — published 3 hours ago, minimum age is 7 days. Contact #platform-eng on Slack for assistance."
  }
  ```
- The denial reason (which rule decided, and why) is always included.
- `PROXY_HELP_MESSAGE`, if configured, is appended to every denial.
