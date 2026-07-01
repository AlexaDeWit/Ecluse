## Summary

This PR cleans up stale conversational terminology scattered across the codebase, specifically replacing the legacy terms 'Layer A', 'Layer B', and decision identifiers ('D1', 'D2', etc.) with their plain-English descriptive equivalents.

## In plain terms

**The situation / the risk.** The codebase documentation and benchmarks contained historical conversational shorthand (like "Layer A" for work-per-request micro-benches or "Layer B" for load benchmarks). These terms require tribal knowledge to understand and make the architecture less accessible to new contributors.

**What changed.** The legacy shorthand was systematically replaced with self-evident descriptive phrases (e.g., "work-per-request micro-benches" instead of "Layer A") across all GitHub workflows, documentation files, and the benchmark suites. The decision identifiers (e.g., "decision D1") were either removed if redundant or integrated as plain text rationale.

**The trade-off.** By making these replacements we slightly increase the verbosity of certain benchmark labels and descriptions, but the clarity gained for future maintainers far outweighs the cost of slightly longer lines.

## Checklist

- [x] `make check` passes locally (build, unit tests, fourmolu, hlint, Semgrep)
- [x] Docs updated in this PR (README / `docs/` / AGENTS.md) where behaviour, interfaces, or config changed
- [x] Conventional Commit subjects; commits are GPG-signed
- [x] Every commit is signed off, DCO (`git commit -s`)
- [x] Tests added or updated for the change

## Sign-off (DCO)
Signed-off-by: Alexandra DeWit <alexa.dewit@gmail.com>

## AI assistance
- [x] No AI assistance beyond editor autocomplete, **or** disclosed below and marked with an `Assisted-by:` trailer on the relevant commits.

Assisted by Antigravity (Google DeepMind). Verified through codebase searches and manual file review.
