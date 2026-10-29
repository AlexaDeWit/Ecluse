#!/usr/bin/env bash
# Convert explicit HPC ticks and check the unchanged product-module denominator.
set -euo pipefail

tix="${1:?tick file}"
out="${2:?output file}"
suite="${3:-combined}"
builddir="${4:-dist-coverage}"

# Every HPC mix directory produced by the coverage build.
mix_args=()
while IFS= read -r d; do
  mix_args+=(-m "$d")
done < <(find "$builddir" -type d -path '*/hpc/vanilla/mix')

mkdir -p "$(dirname "$out")"

# Shared contracts and drivers are not product coverage.
support_exclude=()
while IFS= read -r module; do
  support_exclude+=(-x "$module")
done < <(find test/support test/contracts runtime/test-support -name '*.hs' | sed -e 's#^\(test/support\|test/contracts\|runtime/test-support\)/##' -e 's#/#.#g' -e 's#\.hs$##')

hpc-codecov "${mix_args[@]}" \
  -s . \
  -s vendor/lzma-managed \
  -x Main \
  -x Paths_ecluse \
  "${support_exclude[@]}" \
  -f codecov \
  -o "$out" \
  "$tix"

# Unit suites must link each product module in their layer. HPC silently omits unlinked code.
# Integration contributes its exercised subset to the unchanged four-report union.
case "$suite" in
  ecluse-core-unit)
    src_dir="core/src"
    src_prefix="./core/src/"
    # Each excluded module needs a reason for having no ticks in this suite.
    unscoped=(
      # Re-exports declare no expressions. HPC covers their implementation modules.
      ./core/src/Ecluse/Core/Credential/Refresh.hs
      ./core/src/Ecluse/Core/Registry/Cargo/Route.hs
      ./core/src/Ecluse/Core/Registry/Npm/Route.hs
      ./core/src/Ecluse/Core/Registry/PyPI/Route.hs
      ./core/src/Ecluse/Core/Security.hs
      ./core/src/Ecluse/Core/Server/Admission.hs
      ./core/src/Ecluse/Core/Server/Admission/Bytes.hs
      ./core/src/Ecluse/Core/Server/Pipeline.hs
      ./core/src/Ecluse/Core/Supervision.hs
      ./core/src/Ecluse/Core/Worker.hs
      # Pure handle and error types. Remove when this module gains executable logic.
      ./core/src/Ecluse/Core/Registry.hs
      # test and dev-only loopback constructor, enabled by the dev-http-egress
      # flag. The integration tests cover it, and core-unit does not link it.
      ./core/src/Ecluse/Core/Security/Egress/DevHttp.hs
      # only the integration test suite covers servePublish
      # (test/integration/Ecluse/Core/Server/Pipeline/PublishIntegrationSpec.hs).
      ./core/src/Ecluse/Core/Server/Pipeline/Publish.hs
    )
    ;;
  ecluse-unit)
    src_dir="src"
    src_prefix="./src/"
    # Intentionally unscoped in ecluse-unit: currently none. Add an entry only with
    # a stated reason, in the ./src/... form the report emits.
    unscoped=()
    ;;
  ecluse-runtime-unit)
    src_dir="runtime/src"
    src_prefix="./runtime/src/"
    # Runtime modules the runtime-unit suite intentionally does not link. Another
    # gating tier covers each one, merged into the Codecov total under its flag.
    unscoped=(
      # Re-exports declare no expressions. HPC covers their implementation modules.
      ./runtime/src/Ecluse/Runtime/Aws/Env.hs
      ./runtime/src/Ecluse/Runtime/Credential/CodeArtifact.hs
      ./runtime/src/Ecluse/Runtime/Cve/Sync.hs
      ./runtime/src/Ecluse/Runtime/Log.hs
      ./runtime/src/Ecluse/Runtime/Maintenance/CodeArtifact.hs
      ./runtime/src/Ecluse/Runtime/Maintenance/CodeArtifact/Decide.hs
      ./runtime/src/Ecluse/Runtime/Maintenance/CodeArtifact/Read.hs
      ./runtime/src/Ecluse/Runtime/Queue/Sqs.hs
      ./runtime/src/Ecluse/Runtime/Server.hs
      ./runtime/src/Ecluse/Runtime/Telemetry.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/Batched.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/Correlation.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/ExportFailure.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/Instruments.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/Resolve.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/Scrape.hs
      ./runtime/src/Ecluse/Runtime/Telemetry/Tracing.hs
      # The application unit suite covers runServer and runWorker.
      # The runtime unit suite cannot link the application library.
      ./runtime/src/Ecluse/Runtime/Server/Internal.hs
      ./runtime/src/Ecluse/Runtime/Env.hs
      # Application server specs cover middleware and health probes.
      ./runtime/src/Ecluse/Runtime/Server/Middleware.hs
      # only the integration tier exercises the S3 export adapter
      # (test/integration/Ecluse/Runtime/Pilot/ExportIntegrationSpec.hs).
      ./runtime/src/Ecluse/Runtime/Pilot/Export.hs
    )
    ;;
  *)
    echo "coverage: wrote $out"
    exit 0
    ;;
esac

expected="$(find "$src_dir" -name '*.hs' | sed 's#^#./#' | sort)"
if [ ${#unscoped[@]} -gt 0 ]; then
  expected="$(comm -23 <(echo "$expected") <(printf '%s\n' "${unscoped[@]}" | sort))"
fi
present="$(grep -oE "\"${src_prefix}[^\"]+"'\.hs"' "$out" | tr -d '"' | sort -u || true)"
missing="$(comm -23 <(echo "$expected") <(echo "$present"))"
if [ -n "$missing" ]; then
  {
    echo "coverage: library modules absent from $out (not linked by $suite):"
    printf '%s\n' "$missing" | sed 's/^/  /'
    echo "Add a test that exercises the module, or list it in scripts/coverage.sh 'unscoped'."
  } >&2
  exit 1
fi

echo "coverage: wrote $out"
