# Rules Engine & Responses

> Part of the [Écluse architecture overview](../architecture.md).

## Rules Engine

**Deny by default, and deny wins.** A package is admitted only if some rule
explicitly allows it *and* no rule denies it. A single matching deny rule
overrides every allow.

Rules evaluate a single `PackageDetails` snapshot — the ecosystem-agnostic
per-version view produced by a registry adapter. A rule never sees registry wire
formats.

Rules are evaluated in two tiers:

1. **Pure rules** — evaluated against `PackageDetails` with no IO. Fast and
   deterministic. Evaluated first. This is the tier implemented today
   ([`src/Ecluse/Rules.hs`](../../src/Ecluse/Rules.hs)).
2. **Effectful rules** — may perform IO (advisory lookups, external policy
   checks). Only evaluated if no pure rule has produced a decision. A later
   phase, layered on top of the pure tier.

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

- **`Allow reason`** — the rule explicitly allows the package.
- **`Deny reason`** — the rule explicitly denies it. A single `Deny` overrides
  any `Allow`.
- **`Abstain reason`** — the rule has no opinion. The reason is retained for the
  audit trail.

`evalRules` evaluates the whole rule set with **deny precedence**: the first rule
to `Deny` wins outright — producing `Denied rule reason` even if an earlier rule
allowed — and ends evaluation. Absent any deny, the **first `Allow`** wins,
producing `Approved rule reason`. If no rule is decisive, the result is
`DeniedByDefault reasons` — deny-by-default, with each abstaining rule's reason
collected (in order) so the denial response can explain what was considered.

Crucially, a rule that does not fire **abstains rather than deciding**: an
allow-rule that does not match abstains (so a later rule may still allow), and a
deny-rule whose condition is absent abstains (so it never forces a denial on its
own). Only an actual `Deny` blocks — and it does so regardless of its position in
the set.

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
