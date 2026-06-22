---
id: S42
title: Per-file SPDX license headers (REUSE-style) + new-file lint
milestone: M8 — Release hardening
status: not-started
depends-on: []
test-tier: []
arch-refs:
  - docs/architecture/release-supply-chain.md#releases--container-image
pr: null
---

# S42 — Per-file SPDX license headers + new-file lint

> Milestone **M8** · depends on: — (independent; tree-wide mechanical change) · tier: n/a (mechanical / workflow)

**Goal.** Give every source file unambiguous, machine-readable licensing by adding
an `SPDX-License-Identifier: MIT` tag (plus a copyright line) to the top of each
`.hs` file, with a lint that keeps new files honest. The root [`LICENSE`](../../LICENSE)
answers "what is the license of this _repository_", but source files get copied,
vendored, snippeted, and extracted — and lose the root file the moment they
travel. A per-file SPDX tag attaches the license to the unit that actually moves,
makes it deterministically parseable by SBOM / license-compliance tooling, and
disambiguates any future non-MIT file. It is the source-level analogue of the
artifact-level provenance Écluse already ships (SBOM + attestation), and aligns
with the [REUSE specification](https://reuse.software) and OpenSSF Best Practices
silver (`license_per_file` / `copyright_per_file`, both _suggested_).

**Why a dedicated slice (not done inline).** It touches **every** source file, so
its conflict surface against in-flight feature slices is maximal. It must land as
**one sweep at a quiet point** (e.g. an inter-wave pass) to avoid half-completion
and merge churn — hence parked here rather than done opportunistically.

**Acceptance criteria.**
- [ ] Every `.hs` under `src/` and `app/` (and `test/` — decide at implementation)
  carries `SPDX-License-Identifier: MIT` and a copyright line, as line comments
  **above** the module Haddock header so docs and the `-Werror` build are
  undisturbed.
- [ ] A CI lint fails a PR that adds a source file without an SPDX tag (a small
  `grep`-based gate, or `reuse lint` if the REUSE layout is adopted), wired into
  the `static-checks` job, off the product path. — _AGENTS.md (CI & Security)_
- [ ] The new-file header convention is documented in [`STYLE.md`](../../STYLE.md)
  and/or [`CONTRIBUTING.md`](../../CONTRIBUTING.md).
- [ ] _(Optional)_ Full REUSE compliance — a `LICENSES/MIT.txt` copy and a green
  `reuse lint` — decided at implementation; the dev shell gains `reuse` if so.

**File scope.**
- `src/**/*.hs`, `app/**/*.hs` (and possibly `test/**/*.hs`) — the headers themselves.
- `.github/workflows/ci.yml` — the new-file SPDX lint step in `static-checks`.
- `flake.nix` — add `reuse` to the dev shell **only if** the REUSE lint is chosen.
- `STYLE.md` / `CONTRIBUTING.md` — the header convention for new files.
- `LICENSES/MIT.txt` — only if full REUSE compliance is adopted.

**Test tier.** None (mechanical) — comment-only headers are inert; correctness is
"the build still passes and the lint is green." The PR gate is otherwise
unaffected.

**Notes / risks.** Low risk (comments only), high churn (every file). It is
_suggested_, not required, for OpenSSF silver — do it for the supply-chain
provenance value and to future-proof against a non-MIT file entering the tree, not
to unblock the badge. The header must sit **above** the Haddock module block so it
does not become the module's rendered documentation. Schedule for a quiet tree;
pairs naturally with an inter-wave housekeeping pass.
