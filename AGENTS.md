# Agent Instructions

This file is the small, always-loaded constitution for agents working on Écluse. The repository is
durable memory: load detailed guidance only when the task needs it. See
[`.agents/context-management.md`](.agents/context-management.md) for the context-routing procedure.

## Start here

- **Always read [`README.md`](README.md) first.** It describes the current architecture, design
  decisions, and module responsibilities.
- Identify the task type and its authoritative source before reading further. Do not preload the
  whole process and design canon.
- The design documents describe the target; git, the implementation, and per-slice status describe
  what has shipped. Reconcile them; treat neither as interchangeable.
- **Escalate, don't guess.** Stop on ambiguous, missing, or contradictory requirements rather than
  inventing a way through them.
- **Review the plan before you build.** For any non-trivial change, put the approach (strategy,
  files, notable trade-offs) to the repo owner and get agreement before writing code. A trivial or
  already-scoped change is exempt; when unsure, surface the plan and wait.

| Work | Read next |
|---|---|
| Implement or change Haskell | Active slice or issue, relevant architecture section, [`STYLE.md`](STYLE.md), then applicable sections of [`HADDOCK.md`](HADDOCK.md) |
| Change architecture or module boundaries | [`docs/architecture.md`](docs/architecture.md) and only the linked concern documents affected |
| Change operator behaviour or configuration | [`USAGE.md`](USAGE.md) and the relevant architecture document |
| Add or change tests | Applicable sections of [`docs/testing.md`](docs/testing.md) |
| Build, debug, or navigate Haskell | Applicable sections of [`docs/getting-started.md`](docs/getting-started.md) |
| Change CI, releases, supply chain, or security tooling | [`CONTRIBUTING.md`](CONTRIBUTING.md) and the relevant testing or release-supply-chain sections |
| Coordinate implementation slices | [`planning/orchestration-strategy.md`](planning/orchestration-strategy.md) |
| Commit or open a PR | [`CONTRIBUTING.md`](CONTRIBUTING.md), the PR template, and the `open-pull-request` skill |
| Run in a hosted/web execution environment | [`.agents/remote-execution.md`](.agents/remote-execution.md) |

## Documentation policy

- Read architecture documents before structural changes, and update documentation in the same PR
  whenever behaviour, architecture, public interfaces, or configuration changes. Do not defer it.
- [`USAGE.md`](USAGE.md) is the operator manual: update it for anything an operator configures or
  must do to run Écluse safely (env vars, config schema, egress, client auth, health/observability
  endpoints). `docs/architecture/` owns the *why*, `USAGE.md` the *how*; keep them aligned.
- Update the architecture section of `README.md` when adding a module or significantly changing a
  module's responsibility.

## Implementation coordination

The repo owner is the **principal architect** and owns design and requirements. The lead agent
coordinates PR-sized work, independent evaluation, and handoff; the full workflow lives in
[`planning/orchestration-strategy.md`](planning/orchestration-strategy.md).

- One isolated worktree per implementation agent; never let concurrent agents edit the same
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

- Follow [`STYLE.md`](STYLE.md) for Haskell and [`HADDOCK.md`](HADDOCK.md) for documentation.
- Organise vertically by capability. Keep effects at the application edge; avoid generic `.Types` or
  `.Helpers` modules unless the split is earned.
- Tests mirror library paths (`src/Foo/Bar.hs` -> `test/Foo/BarSpec.hs`).
- Write prose, comments, commits, and PRs in Canadian English (`behaviour`, `colour`, `licence` as a
  noun, `-ise` endings). Do not rewrite a human contributor's spelling.
- Repository diagrams are Mermaid, never ASCII art.
- **Workspace hygiene:** keep temporary scratch files in `scratchpad/` or `ai-notepad/`, never loose
  in the tree, and review staged files before committing rather than running `git add -A`.

## Build and tooling

- Nix with flakes is mandatory. Use the pinned dev shell; system GHC/Cabal is unsupported. Run work
  through `task` inside the flake, and when the ambient shell may be stale, invoke it as:

  ```bash
  env -u IN_NIX_SHELL nix develop --command task <target>
  ```

- Run `task --list` to discover targets. `task check` is the pre-push suite (a `-Werror` build of
  every component, not a quick check). **Run `task format` before it** so the format-check tier
  passes, and **`task sast` before pushing**. Never ignore a failing exit code; re-run the command
  and see `0` before trusting a fix. CI is the authoritative gate. In the CI-verified batch mode the
  PR's own CI run is the verification loop; see
  [`planning/orchestration-strategy.md`](planning/orchestration-strategy.md).
- Nix-less web agents must still verify locally: bootstrap with `scripts/setup-jules.sh`.
- Use one worktree per agent. Create it with `task new-worktree BRANCH=<branch>` (warms the local
  HLS index) and retire it with `task rm-worktree BRANCH=<branch>`, which reclaims the roughly 1 GB
  HLS cache a bare `git worktree remove` strands; `task worktree-clean` sweeps up after
  hand-removed worktrees. The lifecycle detail lives in
  [`planning/orchestration-strategy.md`](planning/orchestration-strategy.md).
- The `task test-integration` and `task test-e2e` suites run Docker containers scoped to your
  worktree by the `com.ecluse.test.scope` label. If you kill a suite, run `task test-clean`
  (worktree-scoped, safe while others run) and confirm none remain with
  `docker ps --filter label=com.ecluse.test`. `task test-clean-all` reaps every worktree's, so use it
  only when no other suite runs. See [`docs/testing.md`](docs/testing.md).
- Automation scripts are Bash in `scripts/` with `#!/usr/bin/env bash` and `set -euo pipefail`; keep
  workflow `run:` blocks trivial and scripts shellcheck-clean. A new Python or Node build-time
  dependency needs explicit justification.
- Use `hoogle`, HLS, `cabal-plan`, and `ghcid` to discover types and behaviour instead of guessing.
  Start the HLS MCP bridge with the worktree root before semantic requests.

## CI, security, and repository gates

- The CI workflow and tier semantics live in [`docs/testing.md`](docs/testing.md). The terminal
  `gate` job is the branch-protection authority; the weeder and Stan jobs gate through it, while
  smoke, vulnerability scanning, and Codecov's server-side statuses are non-gating.
- Pin every GitHub Action to a full commit SHA and follow the cache rules in
  [`CONTRIBUTING.md`](CONTRIBUTING.md).
- Do not add `.semgrepignore` or `nosemgrep` entries, or Stan `[[ignore]]` entries, without
  repo-owner approval. Prefer fixing the finding.
- Keep the threat model in `threat-modelling/ecluse.json`; do not create a competing prose risk
  register.
- The version authority is `ecluse.cabal`'s `version:` field. Release and supply-chain procedures
  live in [`docs/architecture/release-supply-chain.md`](docs/architecture/release-supply-chain.md).
- Every commit is Conventional-Commit formatted, GPG-signed, DCO-signed off as the human author, and
  discloses non-trivial AI help with `Assisted-by:` (not `Co-Authored-By:`). The `open-pull-request`
  skill is the recipe.

## Skills

- Reusable procedures live in [`.agents/skills/`](.agents/skills/), one directory per skill with a
  `SKILL.md`, following the [Agent Skills](https://agentskills.io/specification) standard. Codex,
  Gemini CLI, and GitHub Copilot discover this location natively.
- Claude Code discovers project skills only from `.claude/skills/`, so each skill is bridged there by
  a tracked relative symlink. Update the matching symlink in the same commit as any skill add,
  rename, or removal.
- `CLAUDE.md` (an `@AGENTS.md` import) and `.gemini/settings.json` (`context.fileName`) exist only to
  point those harnesses at this file. Keep shared guidance here, never in per-agent files.

## Context discipline

- Keep stable rules in files and volatile decisions in the current task or compaction summary. Read
  the precise sections the task needs; avoid whole-canon rereads and keep command output bounded.
- Use a fresh thread for a bounded implementation or review; keep the orchestration thread on
  requirements, decisions, PR state, and handoff.
- Use `orientation` for a cold task session and `resume-orchestration` for the team-lead seat after
  compaction or restart, not both. Full procedure in
  [`.agents/context-management.md`](.agents/context-management.md).
