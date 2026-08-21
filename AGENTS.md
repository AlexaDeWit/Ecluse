# Agent Instructions

This file is the small, always-loaded constitution for agents working on Écluse. The repository is
durable memory: load detailed guidance only when the task needs it. See
[`.agents/context-management.md`](.agents/context-management.md) for the context-routing procedure.

## Start here

- **Always read [`README.md`](README.md) first.** It describes the current architecture, design
  decisions, and module responsibilities.
- Identify the task type and its authoritative source before reading further. Do not preload the
  whole process and design canon.
- The design documents describe the target. Git, the implementation, and the per-slice status
  describe what has shipped. Reconcile the two. Neither is interchangeable with the other.
- **Escalate, don't guess.** Stop on an ambiguous, missing, or contradictory requirement instead of
  inventing a way through it.
- **Review the plan before you build.** For any non-trivial change, put the approach (strategy,
  files, notable trade-offs) to the repo owner and get agreement before you write code. A trivial or
  already-scoped change is exempt. When you are unsure, surface the plan and wait.

| Work | Read next |
|---|---|
| Implement or change Haskell | Active slice or issue, relevant architecture section, [`docs/style.md`](docs/style.md), then applicable sections of [`docs/haddock.md`](docs/haddock.md) |
| Change architecture or module boundaries | [`docs/architecture.md`](docs/architecture.md) and only the linked concern documents affected |
| Change operator behaviour or configuration | [`USAGE.md`](USAGE.md) and the relevant architecture document |
| Add or change tests | Applicable sections of [`docs/testing.md`](docs/testing.md) |
| Build, debug, or navigate Haskell | Applicable sections of [`docs/getting-started.md`](docs/getting-started.md) |
| Change CI, releases, supply chain, or security tooling | [`CONTRIBUTING.md`](CONTRIBUTING.md) and the relevant testing or release-supply-chain sections |
| Coordinate implementation slices | the `orchestrate-implementation` skill and [`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md) |
| Commit or open a PR | [`CONTRIBUTING.md`](CONTRIBUTING.md), the PR template, and the `open-pull-request` skill |
| Run in a hosted/web execution environment | [`.agents/remote-execution.md`](.agents/remote-execution.md) |

## Documentation policy

- Read architecture documents before structural changes, and update documentation in the same PR
  whenever behaviour, architecture, public interfaces, or configuration changes. Do not defer it.
- [`USAGE.md`](USAGE.md) is the operator manual. Update it for anything an operator configures, or
  must do, to run Écluse safely. That covers env vars, config schema, egress, client auth, and the
  health and observability endpoints. `docs/architecture/` owns the *why* and `USAGE.md` owns the
  *how*. Keep them aligned.
- Update the architecture section of `README.md` when adding a module or significantly changing a
  module's responsibility.

## Implementation coordination

The repo owner is the **principal architect** and owns the design and the requirements. The lead
agent coordinates PR-sized work, independent evaluation, and handoff. The full workflow lives in
[`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md).

- One isolated worktree per implementation agent. Never let concurrent agents edit the same
  checkout.
- Dispatch implementation only after explicit architect kickoff. The lead agent never merges or
  pushes to `main`.

## Project structure and code conventions

```text
core/     ecluse-core: pure, ecosystem-agnostic capability core (Ecluse.Core.*)
runtime/  ecluse-runtime: effectful edge: OTel SDK, warp, scribes, cloud adapters (Ecluse.Runtime.*)
src/      ecluse: composition shell that assembles and runs the tiers (Ecluse.*)
app/      executable entry point (keep Main.hs thin)
test/     unit and integration tests mirroring the library hierarchy
docs/     architecture and design documents
```

- Follow [`docs/style.md`](docs/style.md) for Haskell and [`docs/haddock.md`](docs/haddock.md) for
  Haddock.
- Organise vertically by capability. Keep effects at the application edge. Avoid a generic `.Types`
  or `.Helpers` module unless the split earns its place.
- Tests mirror library paths (`src/Foo/Bar.hs` -> `test/Foo/BarSpec.hs`).
- Write prose, comments, commits, and PRs in Canadian English (`behaviour`, `colour`, `licence` as a
  noun, `-ise` endings). Do not rewrite a human contributor's spelling.
- Repository diagrams are Mermaid, never ASCII art.
- **Workspace hygiene:** keep temporary scratch files in `scratchpad/` or `ai-notepad/`, never loose
  in the tree, and review staged files before committing rather than running `git add -A`.

## Build and tooling

- Nix with flakes is mandatory in every environment, hosted agents included. There is no Nix-less
  path ([README → Development](README.md#development) is the official statement). Use the pinned
  dev shell. A system GHC or Cabal is unsupported. Run work through `task` inside the flake, and
  when the ambient shell may be stale, invoke it as:

  ```bash
  env -u IN_NIX_SHELL nix develop --command task <target>
  ```

- Run `task --list` to discover targets. `task check` is the pre-push suite: a `-Werror` build of
  every component, not a quick check. **Run `task format` before it** so the format-check tier
  passes, and run **`task sast` before pushing**. Never ignore a failing exit code. Re-run the
  command and see `0` before you trust a fix. CI is the authoritative gate. In the CI-verified batch
  mode the PR's own CI run is the verification loop. See
  [`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md).
- Use one worktree per agent. Create it with `task new-worktree BRANCH=<branch>`, which warms the
  local HLS index. Retire it with `task rm-worktree BRANCH=<branch>`, which reclaims the roughly
  1 GB HLS cache a bare `git worktree remove` strands. `task worktree-clean` sweeps up after a
  hand-removed worktree. The lifecycle detail lives in
  [`.agents/orchestration-strategy.md`](.agents/orchestration-strategy.md).
- The `task test-integration` and `task test-e2e` suites run Docker containers scoped to your
  worktree by the `com.ecluse.test.scope` label. If you kill a suite, run `task test-clean`
  (worktree-scoped, safe while others run) and confirm none remain with
  `docker ps --filter label=com.ecluse.test`. `task test-clean-all` reaps every worktree's, so use it
  only when no other suite runs. See [`docs/testing.md`](docs/testing.md).
- Automation scripts are Bash in `scripts/`, with `#!/usr/bin/env bash` and `set -euo pipefail`.
  Keep workflow `run:` blocks trivial and the scripts shellcheck-clean. A new Python or Node
  build-time dependency needs explicit justification.
- Use `hoogle`, HLS, `cabal-plan`, and `ghcid` to discover types and behaviour instead of guessing.
  Start the HLS MCP bridge with the worktree root before semantic requests.

## CI, security, and repository gates

- The CI workflow and tier semantics live in [`docs/testing.md`](docs/testing.md). The terminal
  `gate` job is the branch-protection authority, and the weeder and Stan jobs gate through it.
  Smoke, vulnerability scanning, and Codecov's server-side statuses are non-gating.
- Pin every GitHub Action to a full commit SHA ([`CONTRIBUTING.md`](CONTRIBUTING.md)). The
  `setup-toolchain` action and `ci.yml` document the CI cache behaviour beside the code.
- Do not add `.semgrepignore` or `nosemgrep` entries, or Stan `[[ignore]]` entries, without
  repo-owner approval. Prefer fixing the finding.
- Keep the threat model in `threat-modelling/ecluse.json`. Do not create a competing prose risk
  register.
- The version authority is `ecluse.cabal`'s `version:` field. Release and supply-chain procedures
  live in [`docs/architecture/release-supply-chain.md`](docs/architecture/release-supply-chain.md).
- Every commit is Conventional-Commit formatted, GPG-signed, DCO-signed off as the human author, and
  discloses non-trivial AI help with `Assisted-by:` (not `Co-Authored-By:`). The `open-pull-request`
  skill is the recipe.

## Skills

- Reusable procedures live in [`.agents/skills/`](.agents/skills/), one directory per skill with a
  `SKILL.md`, following the [Agent Skills](https://agentskills.io/specification) standard. Codex and
  GitHub Copilot discover this location natively.
- Claude Code discovers project skills only from `.claude/skills/`, so a tracked relative symlink
  bridges each skill there. Update the matching symlink in the same commit as any skill add,
  rename, or removal.
- `CLAUDE.md` (an `@AGENTS.md` import) exists only to point that harness at this file. Keep shared
  guidance here, never in per-agent files.

## Context discipline

- Keep stable rules in files and volatile decisions in the current task or compaction summary. Read
  the precise sections the task needs. Avoid a whole-canon reread, and keep command output bounded.
- Use a fresh thread for a bounded implementation or review. Keep the orchestration thread on
  requirements, decisions, PR state, and handoff.
- Use `orientation` for a cold task session and `resume-orchestration` for the team-lead seat after
  compaction or restart, not both. Full procedure in
  [`.agents/context-management.md`](.agents/context-management.md).
