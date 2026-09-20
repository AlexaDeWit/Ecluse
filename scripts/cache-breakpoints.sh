#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

root=$(git rev-parse --show-toplevel)
cd "$root"
experiment=${GRAPH_EXPERIMENT:-$root/scratchpad/cache-breakpoints}
origin_port=${GRAPH_ORIGIN_PORT:-18101}
revision=89054e5e4857ca2e94e663d4344257b49cb68dd2
pnpm_version=11.25.0
mkdir -p "$experiment"
: > "$experiment/user.npmrc"
: > "$experiment/global.npmrc"
export NPM_CONFIG_USERCONFIG="$experiment/user.npmrc"
export NPM_CONFIG_GLOBALCONFIG="$experiment/global.npmrc"

fail() { printf '%s\n' "$*" >&2; exit 1; }
binary() { cabal list-bin bench-load; }
isolated_tool() { env -i PATH="$PATH" NPM_CONFIG_USERCONFIG="$NPM_CONFIG_USERCONFIG" NPM_CONFIG_GLOBALCONFIG="$NPM_CONFIG_GLOBALCONFIG" "$@"; }
pnpm_client() { isolated_tool "$experiment/tools/node_modules/.bin/pnpm" "$@"; }

prepare() {
    [[ ! -e $experiment/inputs ]] || fail 'inputs already exist, choose a new GRAPH_EXPERIMENT'
    mkdir -p "$experiment/inputs/saerskriven" "$experiment/inputs/next" "$experiment/tools"
    curl --fail --location "https://api.github.com/repos/AlexaDeWit/Saerskriven/tarball/$revision" -o "$experiment/source.tar.gz"
    tar -xzf "$experiment/source.tar.gz" --strip-components=1 -C "$experiment/inputs/saerskriven"
    find "$experiment/inputs/saerskriven" -type f \( -name pnpm-lock.yaml -o -name package-lock.json -o -name npm-shrinkwrap.json -o -name yarn.lock \) -delete
    printf '{"name":"next-cache-input","private":true,"version":"1.0.0","packageManager":"pnpm@%s","dependencies":{"next":"16.2.1","react":"19.2.4","react-dom":"19.2.4"}}\n' "$pnpm_version" > "$experiment/inputs/next/package.json"
    isolated_tool npm install --prefix "$experiment/tools" --registry=https://registry.npmjs.org --ignore-scripts --no-audit --no-fund "pnpm@$pnpm_version"
    {
        printf 'source_revision=%s\n' "$revision"
        printf 'source_archive_sha256='; sha256sum "$experiment/source.tar.gz"
        printf 'pnpm='; pnpm_client --version
        printf 'node='; node --version
        uname -a
        git rev-parse HEAD
        printf 'scripts=disabled\ninput_lockfiles=absent\n'
    } > "$experiment/provenance.txt"
    find "$experiment/inputs" -type f \( -name package.json -o -name pnpm-workspace.yaml -o -name .npmrc -o -name pnpmfile.cjs \) -print0 |
        sort -z | xargs -0 sha256sum > "$experiment/input-hashes.txt"
}

client() {
    local project=$1 output=$2 registry=$3
    [[ $project == saerskriven || $project == next ]] || fail "unknown project: $project"
    mkdir -p "$output/project" "$output/cache" "$output/store"
    cp -a "$experiment/inputs/$project/." "$output/project/"
    local start result install_status inventory_status=0 graph_status=0
    start=$(date +%s%N)
    result=0
    (
        cd "$output/project"
        pnpm_client install --no-frozen-lockfile --ignore-scripts --ignore-pnpmfile --registry "$registry" \
            --store-dir "$output/store" --cache-dir "$output/cache" \
            --config.pm-on-fail=error --config.minimum-release-age=0 \
            --network-concurrency "${GRAPH_CLIENT_CONCURRENCY:-16}" --reporter ndjson
    ) > "$output/install.jsonl" 2> "$output/install.stderr" || result=$?
    install_status=$result
    if [[ -f $output/project/pnpm-lock.yaml ]]; then
        cp "$output/project/pnpm-lock.yaml" "$output/resolved-lock.yaml"
        sed -E 's@http://(localhost|127\.0\.0\.1):[0-9]+(/npm)?/@https://registry.npmjs.org/@g' "$output/resolved-lock.yaml" > "$output/canonical-lock.yaml"
        (cd "$output/project" && pnpm_client list --recursive --depth Infinity --json) > "$output/installed.json" 2> "$output/inventory.stderr" || inventory_status=$?
        jq -e 'type == "array" and length > 0' "$output/installed.json" > /dev/null || inventory_status=1
        if [[ $output != "$experiment/capture/"* ]]; then
            if [[ -f $experiment/capture/$project/canonical-lock.yaml ]]; then
                diff -u "$experiment/capture/$project/canonical-lock.yaml" "$output/canonical-lock.yaml" > "$output/graph.diff" || graph_status=1
            else
                graph_status=1
            fi
        fi
    else
        inventory_status=1
        graph_status=1
    fi
    if [[ $inventory_status != 0 || $graph_status != 0 ]]; then result=1; fi
    jq -n --arg project "$project" --argjson start "$start" --argjson end "$(date +%s%N)" \
        --argjson status "$result" --argjson install "$install_status" --argjson inventory "$inventory_status" --argjson graph "$graph_status" --arg registry "$registry" \
        '{project:$project,startNs:$start,endNs:$end,status:$status,installStatus:$install,inventoryStatus:$inventory,graphStatus:$graph,registry:$registry,scripts:false,pnpmfile:false,minimumReleaseAgeOverride:0,initialLockfile:false,initialClientCache:"empty"}' > "$output/outcome.json"
    return "$result"
}

origin_pid=
proxy_pid=
cleanup() {
    if [[ -n $proxy_pid ]]; then kill "$proxy_pid" 2>/dev/null || true; wait "$proxy_pid" 2>/dev/null || true; fi
    if [[ -n $origin_pid ]]; then kill "$origin_pid" 2>/dev/null || true; wait "$origin_pid" 2>/dev/null || true; fi
}
trap cleanup EXIT

start_origin() {
    local mode=$1
    "$(binary)" graph origin "$mode" "$experiment/corpus" "$origin_port" "${GRAPH_UPSTREAM_DELAY_US:-5000}" +RTS -N2 -RTS > "$experiment/origin-$mode.log" 2>&1 &
    origin_pid=$!
    for _ in {1..100}; do
        if curl --silent --output /dev/null "http://127.0.0.1:$origin_port/private-miss/ready"; then return; fi
        kill -0 "$origin_pid" || fail 'origin exited before readiness'
        sleep 0.1
    done
    fail 'origin readiness timed out'
}

capture() {
    [[ -f $experiment/provenance.txt ]] || fail 'run prepare first'
    [[ ! -e $experiment/capture ]] || fail 'capture already exists, choose a new GRAPH_EXPERIMENT'
    start_origin capture
    local failures=0
    for project in saerskriven next; do
        client "$project" "$experiment/capture/$project" "http://127.0.0.1:$origin_port/" || failures=$((failures + 1))
    done
    [[ $failures == 0 ]] || fail "$failures capture installers failed. Outcomes are preserved."
    local latest
    latest=$(jq -rs 'map(.capDate) | max' "$experiment/corpus"/*.json)
    date -u -d "$latest +2 days" +%Y-%m-%dT%H:%M:%SZ > "$experiment/policy-clock"
    jq -s '[.[] | select(.capKey | contains("/-/") | not)] | {count:length,totalBytes:(map(.capBytes)|add),largestBytes:(map(.capBytes)|max),distribution:(map(.capBytes)|sort)}' "$experiment/corpus"/*.json > "$experiment/metadata-sizes.json"
}

cell() {
    local name=${1:?cell name} bytes=${2:?full byte budget} projects=${3:-saerskriven}
    [[ $name =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] || fail 'invalid cell name'
    local output=$experiment/runs/$name
    [[ ! -e $output ]] || fail "cell already exists: $name"
    mkdir -p "$output"
    env | sort | sed -n '/^GRAPH_/p' > "$output/experiment-env.txt"
    start_origin frozen
    local clock
    clock=$(cat "$experiment/policy-clock")
    "$(binary)" graph proxy "$experiment/corpus" "$output" "$origin_port" "$bytes" \
        "${GRAPH_FULL_ENTRIES:-100000}" "${GRAPH_TTL_SECONDS:-3600}" "$clock" "${GRAPH_BODY_LIMIT:-12582912}" \
        +RTS -N2 -RTS > "$output/proxy.log" 2>&1 &
    proxy_pid=$!
    for _ in {1..300}; do
        [[ ! -f $output/ready ]] || break
        kill -0 "$proxy_pid" || fail 'proxy exited before readiness'
        sleep 0.1
    done
    [[ -f $output/ready ]] || fail 'proxy readiness timed out'
    local registry index=0 failures=0
    registry=$(cat "$output/ready")
    local -a pids=()
    IFS=, read -ra selected <<< "$projects"
    for project in "${selected[@]}"; do
        client "$project" "$output/client-$index" "$registry" &
        pids+=("$!")
        index=$((index + 1))
        sleep "${GRAPH_CLIENT_STAGGER_SECONDS:-0}"
    done
    for pid in "${pids[@]}"; do wait "$pid" || failures=$((failures + 1)); done
    touch "$output/stop"
    wait "$proxy_pid"
    proxy_pid=
    [[ $failures == 0 ]] || fail "$failures cell installers failed. Outcomes are preserved. No capacity model was emitted."
    bash scripts/cache-breakpoint-trace.sh "$output/http.jsonl" "$output"
    "$(binary)" graph model "$output/model-input.json" "$output/model.json" "$(( ${GRAPH_TTL_SECONDS:-3600} * 1000000 ))"
}

case ${1:-} in
    prepare) prepare ;;
    capture) capture ;;
    cell) shift; cell "$@" ;;
    *) fail 'usage: task cache-breakpoints -- prepare|capture|cell NAME FULL_BYTES [PROJECTS]' ;;
esac
