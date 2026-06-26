#!/usr/bin/env bash
#
# Regenerate the version-ordering differential-test fixtures from the canonical
# reference implementations:
#
#   * npm      -> node-semver           (semver.compare)
#   * PyPI     -> Python `packaging`    (packaging.version.Version)
#   * RubyGems -> Ruby `Gem::Version`   (built in)
#
# The output is committed and consumed by the PURE, GATING unit test
# (test/unit/Ecluse/VersionOrderingSpec.hs). This script needs the reference
# tools — the Nix dev shell provides them — and is run only when the curated
# version lists below change. The same comparisons are re-checked live (against
# whatever tools are installed) by the non-gating smoke test; see
# docs/architecture/domain-model.md -> "Internal Domain Model".
#
# Usage:  make gen-version-fixtures   (runs inside the Nix dev shell)
#
# Fixture line format (one comparison per line):  ECOSYSTEM|A|B|ORD
# where ORD is LT | EQ | GT, the ordering of A relative to B.
#
set -euo pipefail

out="${1:-test/unit/fixtures/version-ordering.txt}"
mkdir -p "$(dirname "$out")"

# Curated version lists chosen to exercise the fiddly corners of each grammar:
# numeric ordering (10 vs 9), prerelease vs release, the per-ecosystem
# prerelease tie-break rules, build/local metadata, normalisation
# equivalences, and real-world versions from popular packages. Every entry
# MUST be accepted by its reference tool (node-semver / packaging /
# Gem::Version) — invalid/rejection cases are out of scope here (they belong to
# the must-not-parse work) and would abort generation. The pairwise expansion is
# O(n^2), so each list is kept to a few dozen carefully-chosen strings.
npm_versions=(
  # Numeric ordering, incl. multi-digit (10 > 9) and patch/minor rollover.
  0.0.0 0.0.1 0.1.0 0.10.0 1.0.0 1.0.1 1.0.2 1.0.10 1.1.0 1.2.0 1.9.0 1.10.0 1.11.0
  2.0.0 2.1.0 9.9.9 10.0.0 10.0.1 100.0.0
  # Numeric prerelease ids: compared numerically, and always lower than release.
  1.0.0-0 1.0.0-1 1.0.0-2 1.0.0-10 1.0.0-1.2 1.0.0-1.10 1.0.0-1.2.3
  # The SemVer spec precedence chain (alpha < alpha.1 < alpha.beta < beta <
  # beta.2 < beta.11 < rc.1 < release) plus numeric-vs-alphanumeric id ties.
  1.0.0-alpha 1.0.0-alpha.0 1.0.0-alpha.1 1.0.0-alpha.2 1.0.0-alpha.10 1.0.0-alpha.beta
  1.0.0-beta 1.0.0-beta.2 1.0.0-beta.11 1.0.0-rc.1 1.0.0-rc.2 1.0.0-rc.11
  1.0.0-x.7.z.92 1.0.0-alpha.1.2 1.0.0-SNAPSHOT
  # Build metadata: MUST be ignored for ordering (all equal to 1.0.0 and to each
  # other); also prerelease+build combinations.
  1.0.0+build 1.0.0+build.1 1.0.0+build.2 1.0.0+20130313144700 1.0.0-beta+exp.sha.5114f85 1.0.0-alpha+001
  # Real npm versions (lodash, react, axios) and a real prerelease.
  4.17.21 16.13.1 18.2.0 0.21.1 7.0.0-rc.1 1.2.3-next.0
)
pypi_versions=(
  # Release segments, incl. trailing-zero normalisation (1.0 == 1.0.0 == 1.0.0.0)
  # and multi-digit ordering (1.10 > 1.9).
  0.1 0.9 1.0 1.0.0 1.0.0.0 1.0.1 1.0.10 1.1 1.9 1.10 1.11 2.0 10.0
  # Epochs: an epoch dominates the release (1!0.1 > 2.0; 2!1.0 > 1!2.0).
  1!1.0 1!2.0 2!1.0 1!0.1
  # Full pre/post/dev matrix and the tricky cross-orderings:
  #   1.0.dev0 < 1.0a1 < 1.0a1.post1 < 1.0rc1 < 1.0 < 1.0.post1 < 1.0.post1.dev0
  1.0.dev0 1.0.dev1 1.0a1 1.0a1.dev1 1.0a1.post1 1.0a2 1.0b1 1.0rc1
  1.0.post0 1.0.post1 1.0.post1.dev0 1.0.post2
  # Normalisation equivalences: alpha->a, leading-zero strip, implicit post,
  # and a dev-of-a-pre.
  1.0.0a1 1.0.0alpha1 1.0alpha1 01.0 1.0-1 1.0.0a1.dev2
  # Local versions (PEP 440 "+local") — ordered after the public release they
  # annotate, segment-by-segment; the current 0-coverage gap.
  1.0+ubuntu.1 1.0+ubuntu.2 1.0+local 1.0+abc 1.0+1 1.0.post1+ubuntu.1
  # Pre/post spelling aliases and case-insensitivity (c/pre/preview -> rc;
  # rev -> post; A1 -> a1) — all normalise to the same canonical key.
  1.0c1 1.0pre1 1.0preview1 1.0.rev1 1.0-rc1 1.0RC1 1.0.A1
  # Real PyPI versions (requests, numpy, tqdm, black) and a fully-loaded one.
  2.28.1 1.21.6 4.64.1 23.1.0 1!1.2.3.post4.dev5
)
rubygems_versions=(
  # Release segments, incl. zero-padding equivalence (1 == 1.0 == 1.0.0) and
  # multi-digit ordering (1.10 > 1.9).
  0.9.0 1 1.0 1.0.0 1.0.0.0 1.0.1 1.0.10 1.1.0 1.9.0 1.10.0 2.0.0 10.0.0
  # Prerelease (any letter segment): a string segment sorts BEFORE a numeric
  # one, so these are all < 1.0.0; "beta1" and "beta.1" split identically.
  1.0.0.a 1.0.0.alpha 1.0.0.beta 1.0.0.beta1 1.0.0.beta2 1.0.0.beta.1 1.0.0.beta.2
  1.0.0.pre 1.0.0.pre.1 1.0.0.pre.2 1.0.0.rc1 1.0.0.rc.1 1.2.0.rc1 1.0.0.dev
  # Hyphen is rewritten to ".pre." by Gem::Version.
  1.0.0-1 1.2.3-4
  # Real gem versions (rails, rake, a beta, a 4-segment patch).
  6.1.4 3.0.0 2.7.0 5.0.0.beta3 4.2.11.3 1.16.0 0.1.0
)

ord() { case "$1" in -1) echo LT ;; 0) echo EQ ;; *) echo GT ;; esac; }

emit_npm() {
  node - "$@" <<'JS'
const semver = require("semver");
const vs = process.argv.slice(2);
for (const a of vs) for (const b of vs) {
  console.log(`npm|${a}|${b}|${semver.compare(a, b)}`);
}
JS
}

emit_pypi() {
  python3 - "$@" <<'PY'
import sys
from packaging.version import Version
vs = sys.argv[1:]
for a in vs:
    for b in vs:
        A, B = Version(a), Version(b)
        print(f"pypi|{a}|{b}|{(A > B) - (A < B)}")
PY
}

emit_rubygems() {
  ruby - "$@" <<'RB'
vs = ARGV
vs.each do |a|
  vs.each do |b|
    puts "rubygems|#{a}|#{b}|#{Gem::Version.new(a) <=> Gem::Version.new(b)}"
  end
end
RB
}

{
  echo "# Generated by scripts/gen-version-fixtures.sh from the reference"
  echo "# implementations (node-semver / Python packaging / Ruby Gem::Version)."
  echo "# Do not edit by hand; rerun the script. Format: ECOSYSTEM|A|B|ORD"
  emit_npm "${npm_versions[@]}"       | while IFS='|' read -r e a b n; do echo "$e|$a|$b|$(ord "$n")"; done
  emit_pypi "${pypi_versions[@]}"     | while IFS='|' read -r e a b n; do echo "$e|$a|$b|$(ord "$n")"; done
  emit_rubygems "${rubygems_versions[@]}" | while IFS='|' read -r e a b n; do echo "$e|$a|$b|$(ord "$n")"; done
} > "$out"

echo "wrote $(grep -vc '^#' "$out") comparisons to $out"
