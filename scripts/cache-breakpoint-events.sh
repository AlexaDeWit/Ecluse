#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail

input=${1:?cache event trace}
output=${2:?report destination}
jq -s '
  sort_by(if .kind == "insert" then .insertNs else .observedNs end) |
  reduce .[] as $event ({admittedBytes:0,counts:{},generations:{},reuse:[],expiryRemovals:0,bytePressureRemovals:0,countPressureRemovals:0};
    .counts[$event.kind] = ((.counts[$event.kind] // 0) + 1) |
    ([$event.key, $event.expiryNs] | tojson) as $generation |
    if $event.kind == "insert" then
      .admittedBytes += $event.bytes |
      .generations[$generation] = {insertNs:$event.insertNs,admitted:.admittedBytes,bytes:$event.bytes}
    elif $event.kind == "listing-hit" or $event.kind == "artifact-full-hit" then
      .generations[$generation] as $insert |
      if $insert == null then error("reuse lacks an insertion generation") else
        .reuse += [{key:$event.key,kind:$event.kind,bytes:$event.bytes,delayNs:($event.observedNs-$insert.insertNs),interveningAdmittedBytes:(.admittedBytes-$insert.admitted)}]
      end
    elif $event.kind == "remove" then
      .expiryRemovals += (if $event.cause == "expiry" then 1 else 0 end) |
      .bytePressureRemovals += (if $event.bytePressure then 1 else 0 end) |
      .countPressureRemovals += (if $event.countPressure then 1 else 0 end)
    else . end
  ) | del(.generations)
' "$input" > "$output"
