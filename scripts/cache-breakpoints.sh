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
touch "$experiment/user.npmrc" "$experiment/global.npmrc"
export NPM_CONFIG_USERCONFIG="$experiment/user.npmrc"
export NPM_CONFIG_GLOBALCONFIG="$experiment/global.npmrc"

fail() { printf '%s\n' "$*" >&2; exit 1; }
binary() { cabal list-bin bench-load; }
pnpm_client() { "$experiment/tools/node_modules/.bin/pnpm" "$@"; }

prepare() {
    [[ ! -e $experiment/inputs ]] || fail 'inputs already exist, choose a new GRAPH_EXPERIMENT'
    mkdir -p "$experiment/inputs/saerskriven" "$experiment/inputs/next" "$experiment/tools"
    curl --fail --location "https://api.github.com/repos/AlexaDeWit/Saerskriven/tarball/$revision" -o "$experiment/source.tar.gz"
    tar -xzf "$experiment/source.tar.gz" --strip-components=1 -C "$experiment/inputs/saerskriven"
    find "$experiment/inputs/saerskriven" -type f \( -name pnpm-lock.yaml -o -name package-lock.json -o -name npm-shrinkwrap.json -o -name yarn.lock \) -delete
    printf '{"name":"next-cache-input","private":true,"version":"1.0.0","packageManager":"pnpm@%s","dependencies":{"next":"16.2.1","react":"19.2.4","react-dom":"19.2.4"}}\n' "$pnpm_version" > "$experiment/inputs/next/package.json"
    npm install --prefix "$experiment/tools" --ignore-scripts --no-audit --no-fund "pnpm@$pnpm_version"
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
    date -u -d '+2 days' +%Y-%m-%dT%H:%M:%SZ > "$experiment/policy-clock"
}

client() {
    local project=$1 output=$2 registry=$3
    mkdir -p "$output/project" "$output/cache" "$output/store"
    cp -a "$experiment/inputs/$project/." "$output/project/"
    local start result
    start=$(date +%s%N)
    result=0
    (
        cd "$output/project"
        pnpm_client install --no-frozen-lockfile --ignore-scripts --ignore-pnpmfile --registry "$registry" \
            --store-dir "$output/store" --cache-dir "$output/cache" \
            --config.pm-on-fail=error \
            --network-concurrency "${GRAPH_CLIENT_CONCURRENCY:-16}" --reporter ndjson
    ) > "$output/install.jsonl" 2> "$output/install.stderr" || result=$?
    jq -n --arg project "$project" --argjson start "$start" --argjson end "$(date +%s%N)" \
        --argjson status "$result" --arg registry "$registry" \
        '{project:$project,startNs:$start,endNs:$end,status:$status,registry:$registry,scripts:false,initialLockfile:false,initialClientCache:"empty"}' > "$output/outcome.json"
    if [[ -f $output/project/pnpm-lock.yaml ]]; then
        cp "$output/project/pnpm-lock.yaml" "$output/resolved-lock.yaml"
        sed -E 's@http://(localhost|127\.0\.0\.1):[0-9]+(/npm)?/@https://registry.npmjs.org/@g' "$output/resolved-lock.yaml" > "$output/canonical-lock.yaml"
        (cd "$output/project" && pnpm_client list --recursive --depth Infinity --json) > "$output/installed.json" 2> "$output/inventory.stderr" || return 1
        if [[ $output != "$experiment/capture/"* ]] && [[ -f $experiment/capture/$project/canonical-lock.yaml ]]; then
            diff -u "$experiment/capture/$project/canonical-lock.yaml" "$output/canonical-lock.yaml" > "$output/graph.diff" || result=1
        fi
    fi
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
    [[ -f $experiment/policy-clock ]] || fail 'run prepare first'
    [[ ! -e $experiment/capture ]] || fail 'capture already exists, choose a new GRAPH_EXPERIMENT'
    start_origin capture
    local failures=0
    for project in saerskriven next; do
        client "$project" "$experiment/capture/$project" "http://127.0.0.1:$origin_port/" || failures=$((failures + 1))
    done
    [[ $failures == 0 ]] || fail "$failures capture installers failed. Outcomes are preserved."
}

cell() {
    local name=${1:?cell name} bytes=${2:?full byte budget} projects=${3:-saerskriven}
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
    jq -s '[.[] | select(.accountedBytes != null and .accountedBytes > 0 and (.key | startswith("private-miss/") | not)) | {trKey:.package,trStart:.startMicros,trEnd:.endMicros,trWeight:.accountedBytes,trAccess:(if (.key | contains("/-/")) then "Artifact" else "Listing" end),trSuccess:(.status != null and .status >= 200 and .status < 300)}]' "$output/http.jsonl" > "$output/model-input.json"
    "$(binary)" graph model "$output/model-input.json" "$output/model.json" "$(( ${GRAPH_TTL_SECONDS:-3600} * 1000000 ))"
    [[ $failures == 0 ]] || fail "$failures cell installers failed. Outcomes are preserved."
}

case ${1:-} in
    prepare) prepare ;;
    capture) capture ;;
    cell) shift; cell "$@" ;;
    *) fail 'usage: task cache-breakpoints -- prepare|capture|cell NAME FULL_BYTES [PROJECTS]' ;;
esac
