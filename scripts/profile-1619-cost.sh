#!/usr/bin/env bash
# Profiles include unchanged harness preparation. Listing profiles retain that startup cost.
set -euo pipefail

[[ ${GITHUB_ACTIONS:-} == true && ${RUNNER_ENVIRONMENT:-} == github-hosted && ${RUNNER_ARCH:-} == ARM64 ]] || {
  echo 'This temporary profile runs only on hosted arm64 Actions.' >&2
  exit 2
}

candidate=d263d5a18c9975395193fbd3ba6654aae640439b
baseline=4b0b5d12d478fc5dac4112e374f4b2d030b640cf
feature=978f32cc169e7eafc400385f1f60bb9157b6fdda
workspace="${GITHUB_WORKSPACE:?}"
script="$workspace/scripts/profile-1619-cost.sh"
results="$workspace/scratchpad/1619-cost-profile"
export LC_ALL=C.UTF-8 TZ=UTC
unset GHCRTS
export CABAL_CONFIG="${CABAL_DIR:?}/config"
mkdir -p "$results"
trap 'printf "line=%s\tcode=%s\tmode=%s\n" "$LINENO" "$?" "${1:-all}" >> "$results/failures.tsv"' ERR

# Timeouts cover Cabal as well as the harness, and retain the command's exit status.
capture() {
  local label="$1" bound="$2" code
  shift 2
  printf '%s\tstarted\t%s\n' "$label" "$(date -u +%FT%TZ)" >> "$out/harness-status.tsv"
  if timeout --kill-after=60s "$bound" "$@" 2>&1 | tee "$out/$label.output.txt"; then
    printf '%s\t0\t%s\n' "$label" "$(date -u +%FT%TZ)" >> "$out/harness-status.tsv"
  else
    code=$?
    printf '%s\t%s\t%s\n' "$label" "$code" "$(date -u +%FT%TZ)" >> "$out/harness-status.tsv"
    printf '%s\t%s\n' "$label" "$code" >> "$out/failures.tsv"
    return "$code"
  fi
}

retain_profile() {
  local destination="$1"
  mkdir -p "$destination"
  if [[ -f ecluse-bench.prof ]]; then
    mv ecluse-bench.prof "$destination/profile.prof"
  fi
  if [[ -f ecluse-bench.svg ]]; then
    mv ecluse-bench.svg "$destination/flamegraph.svg"
  fi
}

# Preserve plans and unfinished profiles when a build, assertion or renderer fails.
finish_source() {
  local code=$?
  retain_profile "$out/$phase"
  if [[ -d "$builddir/cache" ]]; then
    cp -a "$builddir/cache" "$out/build-cache"
  fi
  if [[ -d "$CABAL_DIR/store" ]]; then
    find "$CABAL_DIR/store" -name cabal-hash.txt -type f -print -exec cat {} \; > "$out/cabal-store.txt"
  fi
  git status --porcelain=v1 --untracked-files=all > "$out/source-status-after.txt"
  printf 'exit_code=%s\n' "$code" > "$out/source-exit.txt"
}

case "${1:-all}" in
  source)
    ref="${2:?}"
    out="$results/$ref"
    builddir="$workspace/scratchpad/cost-profile-builds/$ref/prof"
    mkdir -p "$out"
    phase=preparation
    trap finish_source EXIT
    [[ "$ref" == "$candidate" || "$ref" == "$baseline" ]]
    [[ "$(git rev-parse HEAD)" == "$ref" ]]
    [[ ! -e cabal.project.local ]]
    git status --porcelain=v1 --untracked-files=all > "$out/source-status-before.txt"
    [[ ! -s "$out/source-status-before.txt" ]]
    git show -s --format=raw HEAD > "$out/source-identity.txt"
    git ls-tree -r HEAD > "$out/source-tree.txt"
    git archive HEAD | tar -x -C "$out" -- \
      README.md Taskfile.yml flake.nix flake.lock cabal.project cabal.project.freeze ecluse.cabal \
      bench/Main.hs bench/Ecluse/Core/ColdReadBench.hs \
      core/src/Ecluse/Core/Package/Pep503.hs core/src/Ecluse/Core/Security/Limits.hs \
      core/src/Ecluse/Core/Registry/PyPI core/src/Ecluse/Core/Registry/Json \
      test/support/Ecluse/Test/EcosystemBench.hs test/support/Ecluse/Test/Corpus.hs
    sha256sum bench/corpus/npm/*.json bench/corpus/pypi/*.json > "$out/capture-sha256.txt"
    wc -c bench/corpus/pypi/requests.simple.json bench/corpus/pypi/numpy.simple.json > "$out/capture-bytes.txt"
    ghc --info > "$out/compiler-info.txt"
    cabal --version > "$out/cabal-version.txt"
    cabal path > "$out/cabal-paths.txt"
    nix --version > "$out/nix-version.txt"
    nix flake metadata --json --no-write-lock-file > "$out/flake-metadata.json"
    compiler="$(readlink -f "$(command -v ghc)")"
    nix path-info --json --recursive "${compiler%/bin/*}" > "$out/compiler-store.json"
    printf 'optimization=1 (Cabal default)\nprofiling=enabled\nprofiling-detail=late\nRTS=-N1 -T -p\ntime-mode=cpu\nstdev=0.0001\ntimeout=30s\noverlay=none\n' > "$out/settings.txt"
    capture update 10m task update
    cp "$CABAL_DIR/config" "$out/cabal.config"
    awk '/^[[:space:]]*optimization:/ {if ($2 != "1" && $2 != "True") exit 1}' "$out/cabal.config"

    common='/ecosystem: pypi/ && /cold production reads (per package)/ && /default cap 134217728 bytes: success/ && /full document/'
    selection="$common && (/requests (medium)/ || /numpy (large)/)"
    list_options="--list-tests --pattern=\"$selection\" +RTS -N1 -T -RTS"
    phase=listing
    task --dry bench-profile BENCH_PROFILE_DIR="$builddir" BENCH_PROFILE_OPTS="$list_options" > "$out/profile-commands.txt" 2>&1
    capture listing 75m task --silent bench-profile BENCH_PROFILE_DIR="$builddir" BENCH_PROFILE_OPTS="$list_options"
    retain_profile "$out/listing"
    prefix='All.ecluse-core (work-per-request).ecosystem: pypi.cold production reads (per package).'
    for entry in 'numpy (large)' 'requests (medium)'; do
      printf '%s%s.default cap 134217728 bytes: success.full document\n' "$prefix" "$entry"
    done > "$out/expected-tests.txt"
    awk '/^All[.]ecluse-core/ {print}' "$out/listing.output.txt" | sort > "$out/selected-tests.txt"
    cat "$out/selected-tests.txt"
    capture selection-check 1m diff -u "$out/expected-tests.txt" "$out/selected-tests.txt"
    binary="$(cabal list-bin ecluse-bench --enable-profiling --builddir="$builddir")"
    printf '%s\n' "$binary" > "$out/binary-path.txt"
    sha256sum "$binary" > "$out/binary-sha256.txt"

    for package in requests numpy; do
      phase="$package"
      mkdir -p "$out/$package"
      options="--pattern=\"$common && /$package (/\" --csv $out/$package/bench-results.csv --time-mode cpu --stdev 0.0001 --timeout 30s +RTS -N1 -T -RTS"
      task --dry bench-profile BENCH_PROFILE_DIR="$builddir" BENCH_PROFILE_OPTS="$options" >> "$out/profile-commands.txt" 2>&1
      capture "$package" 5m task --silent bench-profile BENCH_PROFILE_DIR="$builddir" BENCH_PROFILE_OPTS="$options"
      retain_profile "$out/$package"
      [[ -s "$out/$package/profile.prof" && -s "$out/$package/flamegraph.svg" ]]
      csv="$out/$package/bench-results.csv"
      capture "$package-row-check" 1m awk -F, -v expected="$(awk -v package="$package" 'index($0, "." package " (") {print}' "$out/expected-tests.txt")" \
        'NR == 1 {if ($0 != "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory") exit 1}
         NR == 2 {if (NF != 6 || $1 != expected) exit 1}
         END {if (NR != 2) exit 1}' "$csv"
      jq -Rn --rawfile csv "$csv" '{format: "tasty-bench CSV converted to JSON", rows: ($csv | split("\n") | map(select(length > 0) | split(",")))}' \
        > "$out/$package/bench-results.json"
      awk '/Ecluse[.]Core[.](ColdReadBench|Registry[.](PyPI|Json))/ {print}' "$out/$package/profile.prof" > "$out/$package/relevant-cost-centres.txt"
      for target in readManifest reduceMember finishFacts retainFileFact finishPackage sealValue sameManifest sameDocument; do
        awk -v target="$target" 'index($0, target) {print}' "$out/$package/profile.prof" > "$out/$package/$target.cost-centres.txt"
        if [[ -s "$out/$package/$target.cost-centres.txt" ]]; then
          printf '%s\tpresent\n' "$target"
        else
          printf '%s\tunresolved in late cost centres\n' "$target"
        fi
      done > "$out/$package/attribution-status.tsv"
    done
    sha256sum "$binary" > "$out/binary-sha256-after.txt"
    cmp "$out/binary-sha256.txt" "$out/binary-sha256-after.txt"
    git diff --exit-code HEAD
    ;;
  all)
    git merge-base --is-ancestor "$candidate" HEAD
    git diff --exit-code "$candidate" HEAD -- . ':!.github/workflows/bench.yml' ':!scripts/profile-1619-cost.sh'
    git show -s --format=raw HEAD > "$results/measurement-identity.txt"
    sha256sum .github/workflows/bench.yml scripts/profile-1619-cost.sh > "$results/measurement-sha256.txt"
    cp .github/workflows/bench.yml "$results/bench.yml"
    cp "$script" "$results/profile-1619-cost.sh"
    printf 'candidate=%s\nbaseline=%s\nfeature=%s\n' "$candidate" "$baseline" "$feature" > "$results/source-ids.txt"
    parents="$(git -C "$workspace/scratchpad/cost-profile-sources/$candidate" show -s --format=%P HEAD)"
    [[ "$parents" == "$baseline $feature" ]]
    for ref in "$candidate" "$baseline"; do
      source_dir="$workspace/scratchpad/cost-profile-sources/$ref"
      [[ "$(git -C "$source_dir" rev-parse HEAD)" == "$ref" ]]
      sha256sum "$source_dir/core/src/Ecluse/Core/Package/Pep503.hs" | cut -d' ' -f1 > "$results/$ref-pep503.sha256"
      (cd "$source_dir" && sha256sum bench/corpus/npm/*.json bench/corpus/pypi/*.json) > "$results/$ref-captures.sha256"
      for file in flake.nix flake.lock Taskfile.yml cabal.project cabal.project.freeze; do
        cmp "$workspace/$file" "$source_dir/$file"
      done
    done
    cmp "$results/$candidate-pep503.sha256" "$results/$baseline-pep503.sha256"
    cmp "$results/$candidate-captures.sha256" "$results/$baseline-captures.sha256"
    cat > "$results/scope.txt" <<'EOF'
Temporary cost attribution, not an unprofiled CPU or HTTP throughput result.
Both immutable sources use Cabal's -O1 default and the existing late profiling task.
Each binary lists exactly two PyPI cold production full-document default-cap successes.
Each selected capture runs once, with the original preparation, bounds and timed equality.
Profiles include preparation. Listing profiles retain startup and preflight costs separately.
Raw tasty-bench output is CSV. The JSON file converts its fields without changing values.
Cost-centre absence or inlining needs review before any proposed temporary SCC overlay.
Function extracts retain flat and inherited rows. They overlap and must not be summed.
Baseline PyPI uses tree reduction. Packed sealing in its startup profile comes from npm.
No source overlay, comparator correction, assertion removal or product change applies.
The build cache retains Cabal plans and configurations beside compiler and store identities.
EOF
    failed=0
    for ref in "$candidate" "$baseline"; do
      mkdir -p "$results/$ref"
      if (cd "$workspace/scratchpad/cost-profile-sources/$ref" && \
        timeout --kill-after=60s 90m env -u IN_NIX_SHELL nix develop .#ci --command bash "$script" source "$ref") \
        2>&1 | tee "$results/$ref/source.output.txt"; then
        printf '%s\t0\n' "$ref" >> "$results/harness-status.tsv"
      else
        code=$?
        printf '%s\t%s\n' "$ref" "$code" >> "$results/harness-status.tsv"
        printf '%s\t%s\n' "$ref" "$code" >> "$results/failures.tsv"
        failed=1
      fi
    done
    exit "$failed"
    ;;
  *) echo 'Expected all or source mode.' >&2; exit 2 ;;
esac
