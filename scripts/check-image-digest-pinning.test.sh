#!/usr/bin/env bash
#
# Deterministic unit test for check-image-digest-pinning.sh. It gates the guard's
# own precision: a compliant reference passes, a tag-pinned reference at a real
# pull construct is caught, and every innocuous colon in the harness (host:port
# endpoints, bind-mount specs, network aliases, and constructs mentioned only in
# comments) passes untouched. The guard is a supply-chain check, so a false
# negative (a tag slipping through) is the dangerous failure this locks against;
# the false-positive fixtures keep it from becoming noise that gets disabled.
# Run via `task test-scripts` (folded into `task check`).
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
guard="$here/check-image-digest-pinning.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fail=0

# Real, valid digest pins (verbatim from the harness), so the compliant fixtures
# exercise the exact reference shape the guard must accept.
verd_pin='verdaccio/verdaccio@sha256:9d622d256378c6e7ae09f384774ee2f0f8ac67a66c066db55921a0b7218abc4c'
nginx_pin='nginx@sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fefa'
mini_pin='ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594'
otel_pin='otel/opentelemetry-collector@sha256:3805724e26351df55a45032a793c9b64a2117ac9a58f13f070674a9723fab373'

# Assert the guard exits 0 (all references pinned) on the given files.
expect_pass() {
  local name="$1"
  shift
  if "$guard" "$@" >/dev/null 2>&1; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (guard flagged a compliant/innocuous fixture)\n' "$name"
    "$guard" "$@" || true
    fail=1
  fi
}

# Assert the guard exits non-zero AND names the expected offending reference.
expect_catch() {
  local name="$1" offender="$2"
  shift 2
  local out status
  out="$("$guard" "$@" 2>&1)" && status=0 || status=$?
  if [[ "$status" -ne 0 && "$out" == *"$offender"* ]]; then
    printf 'ok   - %s\n' "$name"
  else
    printf 'FAIL - %s (status=%s, expected non-zero naming %s)\n' "$name" "$status" "$offender"
    printf '       out: %s\n' "$out"
    fail=1
  fi
}

# --- Fixture: every construct pinned by digest (must pass) ------------------

cat >"$work/pass.hs" <<EOF
verdRun = (dockerRun verd net "$verd_pin")
stubRun = (dockerRun stub net "$nginx_pin")
collectorImage = "$otel_pin"
ministackDockerfile =
    "FROM $mini_pin\n\\
    \\LABEL com.ecluse.test=integration\n"
EOF

cat >"$work/pass.yml" <<EOF
      - name: Pre-pull pinned images
        run: >-
          bash scripts/docker-prepull.sh
          $mini_pin
          $otel_pin
          $verd_pin
          $nginx_pin

      - name: Next step
        run: echo done
EOF

expect_pass "compliant Haskell constructs pass" "$work/pass.hs"
expect_pass "compliant docker-prepull args pass" "$work/pass.yml"

# --- Fixture: innocuous colons the guard must NOT flag ----------------------
# host:port endpoints, publish specs, bind mounts, network aliases, and a
# Haddock/comment mention of a fake pull construct.

cat >"$work/decoys.hs" <<EOF
-- A Haddock note: an immutable @sha256@ digest cannot be re-pointed.
{- | Doc example that must be ignored: dockerRun verd net "verdaccio/verdaccio:5"
     and a FROM nginx:alpine inside prose. -}
drPorts = ["127.0.0.1:0:4566", "127.0.0.1:0:4873"]
drAliases = ["ministack", "verdaccio", "otelcol", "upstream", "mirror"]
sqsEndpoint = "http://ministack:4566"
otlpEndpoint = "http://otelcol:4318"
verdaccioLocal = "http://127.0.0.1:4873"
drMounts = [(a, "/certs:ro"), (b, "/usr/share/nginx/html:ro")]
proxRun = (dockerRun prox net image)
collRun = (dockerRun coll net collectorImage)
collectorDockerfile = "FROM " <> collectorImage <> "\n"
trailing = "value" -- example only: dockerRun a b "nginx:latest" in a comment
EOF

cat >"$work/decoy.yml" <<EOF
    env:
      SQS_ENDPOINT: ministack:4566
      OTLP_ENDPOINT: otelcol:4318
    services:
      verdaccio:
        ports:
          - "4873:4873"
EOF

expect_pass "host:port, mounts, aliases, comments not flagged (Haskell)" "$work/decoys.hs"
expect_pass "non-prepull yaml colons not flagged" "$work/decoy.yml"
expect_pass "compliant + decoy fixtures together pass" \
  "$work/pass.hs" "$work/pass.yml" "$work/decoys.hs" "$work/decoy.yml"

# --- Fixture: tag-pinned references at real pull constructs (must be caught) -

cat >"$work/fail-run.hs" <<'EOF'
verdRun = (dockerRun verd net "verdaccio/verdaccio:5")
EOF

cat >"$work/fail-image.hs" <<'EOF'
collectorImage = "otel/opentelemetry-collector:0.119.0"
EOF

cat >"$work/fail-from.hs" <<'EOF'
ministackDockerfile =
    "FROM ministackorg/ministack:1.3-full\n\
    \LABEL com.ecluse.test=integration\n"
EOF

cat >"$work/fail-prepull.yml" <<'EOF'
      - name: Pre-pull images
        run: >-
          bash scripts/docker-prepull.sh
          verdaccio/verdaccio:5
          nginx@sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fefa
EOF

expect_catch "tag-pinned dockerRun caught" 'verdaccio/verdaccio:5' "$work/fail-run.hs"
expect_catch "tag-pinned *Image binding caught" 'otel/opentelemetry-collector:0.119.0' "$work/fail-image.hs"
expect_catch "tag-pinned FROM caught" 'ministackorg/ministack:1.3-full' "$work/fail-from.hs"
expect_catch "tag-pinned docker-prepull arg caught" 'verdaccio/verdaccio:5' "$work/fail-prepull.yml"

# A short (63-hex) digest must not be accepted as a valid pin.
cat >"$work/fail-shorthash.hs" <<'EOF'
collectorImage = "nginx@sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fef"
EOF
expect_catch "63-hex digest rejected" 'sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fef' "$work/fail-shorthash.hs"

if [[ "$fail" -ne 0 ]]; then
  echo "check-image-digest-pinning: TESTS FAILED" >&2
  exit 1
fi
echo "check-image-digest-pinning: all tests passed"
