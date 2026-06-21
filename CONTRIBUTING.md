# Contributing

This guide covers how we work on `npm-secure-proxy`: local development, codebase
conventions, testing, and CI / repository requirements. For the systems design
— the three-registry model, rules engine, mirror queue, and configuration — see
[`docs/architecture.md`](docs/architecture.md). Agent-specific instructions live
in [`AGENTS.md`](AGENTS.md).

## Local development

All tooling comes from the Nix dev shell — run `nix develop` (or `direnv allow`)
before anything else; do not assume a system-level GHC/Cabal. Builds use
**Cabal** (not Stack); Nix and `flake.lock` provide reproducibility.

| Task | Command (inside `nix develop`) |
|------|--------------------------------|
| Build | `cabal build all --enable-tests` |
| Test | `cabal test all --test-show-details=direct` |
| Format | `fourmolu --mode inplace $(git ls-files '*.hs')` |
| Lint | `hlint $(git ls-files '*.hs')` |
| Static analysis | `semgrep scan --config auto --severity ERROR --severity WARNING --error .` |

**Before you push,** all of these must be clean: build (no warnings), tests,
`fourmolu --mode check`, `hlint`, and Semgrep (zero findings). CI enforces the
same set via the `gate` job (see below), so a clean local run predicts a green
CI.

---

## Codebase Layout

Modules are fit-to-purpose and follow idiomatic Haskell structure: each area of
the application gets its own namespace directly under `NpmSecureProxy`, and types
are split from implementation where that split earns its keep.

- **One namespace per area.** Each concern lives under its own
  `NpmSecureProxy.<Area>` namespace (`Rules` today; `Registry`, `Config`,
  `Server`, … later) rather than being appended to a grab-bag module.
- **Types split from implementation — when it helps.** Where an area carries
  non-trivial logic, its data types live in a `.Types` leaf module and the
  functions live in the sibling module (e.g. `NpmSecureProxy.Rules.Types` +
  `NpmSecureProxy.Rules`). Where an area is essentially a cohesive set of types
  with their constructors and renderers (the package model), a single module is
  clearer than a forced split.

Current layout:

| Module | Holds |
|--------|-------|
| `NpmSecureProxy.Package` | The ecosystem-agnostic package vocabulary: `Scope`, `PackageName`, `Version`, `Dist`, `Maintainer`, `PackageDetails`, with their smart constructors and renderers. |
| `NpmSecureProxy.Rules.Types` | Rule data types: `Rule`, `EvalContext`, `RuleOutcome`, `Decision`. |
| `NpmSecureProxy.Rules` | Rule evaluation and decision rendering: `evalRule`, `evalRules`, `renderDecision`, `renderDuration`. |

Tests mirror this hierarchy under `test/` (e.g. the spec for
`NpmSecureProxy.Rules` is `test/NpmSecureProxy/RulesSpec.hs`).

---

## Testing Strategy

Tests are layered so the fast, deterministic majority run everywhere with no
external dependencies, and the heavier integration tests stay hermetic and
reproducible.

1. **Unit & property tests** (`hspec` + `hedgehog`). Cover all pure logic — the
   rules engine, response parsers, and configuration parsing. No IO, no Docker;
   they run on every push and locally in milliseconds. The rules engine in
   particular is exercised with property tests: deny-by-default invariants,
   first-decisive-wins ordering, and per-rule predicates.
2. **Integration tests** (`hspec` + `testcontainers` + `ministack`). The mirror
   queue and other AWS-backed code are tested against a real endpoint by spinning
   up `ministack` (a lightweight LocalStack alternative) in an ephemeral
   container. `amazonka` is pointed at `http://<container>:4566` with throwaway
   credentials; SQS enqueue/consume and STS token flows are validated end to end
   without touching real AWS.
3. **Stub upstream registries.** Proxy request-lifecycle tests run against an
   in-process WAI stub (or a container) standing in for the private/public
   upstreams, so the full fetch → parse → rules → mirror path can be asserted.

**CodeArtifact caveat.** `ministack` emulates SQS and STS but not the
CodeArtifact API (`GetAuthorizationToken`). CodeArtifact's npm-protocol surface
is covered through the `RegistryClient` seam (a stub registry), and the
token-refresh call is covered by mocking at that same seam — a deliberate benefit
of the registry abstraction.

**Prerequisite.** Integration tests require a running Docker daemon. CI
(GitHub Actions `ubuntu-latest`) provides one; locally, developers need Docker
installed. Nix provides the toolchain but not the Docker daemon (a host concern).

**References:** [testcontainers](https://hackage.haskell.org/package/testcontainers) (Haskell, GHC 9.6-compatible) · [ministack](https://github.com/ministackorg/ministack) (local AWS emulator, image `ministackorg/ministack`, port 4566).

---

## Continuous Integration

CI is a **single unified workflow graph** ([`.github/workflows/ci.yml`](.github/workflows/ci.yml)).
Every check is a job in that one graph — build & test, format & lint, and Semgrep
static analysis — and they all feed a terminal **gate** job that succeeds only
when every upstream job has succeeded.

- **One required check.** Branch-protection rulesets mark only the `gate` job as
  `Required`. Adding or removing checks never means editing the ruleset: a new
  job simply becomes another dependency of the gate.
- **SHA-pinned actions.** Every `uses:` reference is pinned to a full commit SHA
  (never a tag or branch), with the human-readable version in a trailing
  comment. Dependabot keeps the SHAs current. This stops a re-tagged or
  compromised action from silently entering CI — directly relevant for a
  supply-chain security tool.
- **Semgrep via Nix.** Semgrep runs from the pinned Nix dev shell
  (`nix develop --command semgrep ...`), exactly as developers run it locally,
  rather than from a third-party container image — one fewer unpinned
  supply-chain input.

---

## Repository requirements

- **Workflows stay injection-free.** Never interpolate untrusted
  `${{ github.event.* }}` / `${{ github.head_ref }}` values directly into `run:`
  shell blocks; pass them via `env:` or intermediate files instead.
- **Semgrep ignores require the repo owner's approval.** Do not add
  `.semgrepignore` entries or `nosemgrep` comments unilaterally.
- **Use [Conventional Commits](https://www.conventionalcommits.org/).** Write
  commit subjects as `type(scope): summary`, where `type` is one of `feat`,
  `fix`, `docs`, `chore`, `ci`, `refactor`, `test`, `build`, or `perf` (scope
  optional). Keep the summary short and imperative; put detail in the body.
- **Commits are GPG-signed.** Keep history verifiable.
