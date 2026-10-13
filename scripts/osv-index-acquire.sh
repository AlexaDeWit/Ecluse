#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
out="${1:?fresh input directory}"
mkdir "$out"
fetch() {
  local name="$1" url="$2" bytes="$3" etag="$4" identity_header="$5" identity="$6"
  curl --fail --location --retry 2 --dump-header "$out/$name.headers" --output "$out/$name" "$url"
  test "$(wc -c < "$out/$name")" -eq "$bytes"
  tr -d '\r' < "$out/$name.headers" | awk -v want="$bytes" 'tolower($1)=="content-length:" {value=$2} END {exit value!=want}'
  tr -d '\r' < "$out/$name.headers" | awk -v want="$etag" 'tolower($1)=="etag:" {gsub(/"/, "", $2); value=$2} END {exit value!=want}'
  tr -d '\r' < "$out/$name.headers" | awk -v key="$identity_header:" -v want="$identity" 'tolower($1)==key {value=$2} END {exit value!=want}'
}
fetch npm-full.zip 'https://osv-vulnerabilities.storage.googleapis.com/npm/all.zip?generation=1791860590767687' 218363673 e3f9f7acf00a6f84a63a27f5ce8a6cec x-goog-generation 1791860590767687
fetch pypi-full.zip 'https://osv-vulnerabilities.storage.googleapis.com/PyPI/all.zip?generation=1791838994093506' 35977300 7f1ee8b695321095b5637f66effabd73 x-goog-generation 1791838994093506
fetch epss.csv.gz 'https://epss.empiricalsecurity.com/epss_scores-2026-09-27.csv.gz?versionId=GL3FB0.f4yDl8jzyyptuxspqz5LOCE6T' 2671545 0313af55cdf6ee83ff8d5f7d16aa5bd0 x-amz-version-id GL3FB0.f4yDl8jzyyptuxspqz5LOCE6T
