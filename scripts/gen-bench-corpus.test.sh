#!/usr/bin/env bash
# Exercise byte preservation and provenance without contacting a registry.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p "$here/../scratchpad"
temporary="$(mktemp -d "$here/../scratchpad/gen-bench-corpus.XXXXXX")"
trap 'rm -rf "$temporary"' EXIT
export CAPTURE_TEST_ROOT="$temporary"
cat > "$temporary/pins.json" <<'JSON'
{"pins":{"example":"1.0.0"},"pypiPins":{"example":"1.0.0"},"rubygemsPins":{"example":"1.0.0"},"cargoPins":{"example":"1.0.0"}}
JSON
cat > "$temporary/npm.json" <<'JSON'
{ "name": "example", "versions": {"1.0.0": {}, "9.0.0-rc.1": {"contributors": ["A"]}},
  "unrecognised": 1e2, "dist-tags": {"latest": "9.0.0-rc.1"} }
JSON
cat > "$temporary/pypi.json" <<'JSON'
{ "name": "example", "files": [{"filename": "example-9.0.0rc1.tar.gz", "upload-time": "2099-01-01T00:00:00Z"}],
  "versions": ["9.0.0rc1"], "meta": {"_last-serial": 123}, "_last-serial": 123 }
JSON
cat > "$temporary/rubygems.json" <<'INFO'
---
1.0.0 dep:>= 0|checksum:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,created_at:2020-01-01T00:00:00Z
2.0.0.pre-x86_64-linux |checksum:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,created_at:2021-01-01T00:00:00Z
INFO
cat > "$temporary/cargo.json" <<'INDEX'
{"name":"example","vers":"1.0.0","deps":[],"cksum":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc","features":{},"yanked":false}
{"name":"example","vers":"2.0.0-rc.1","deps":[],"cksum":"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd","features":{},"yanked":true,"v":2}
INDEX
cat > "$temporary/https.cjs" <<'JS'
const fs = require("fs");
const { EventEmitter } = require("events");
const jsonFailures = { name: '{"name":"other","versions":{"1":{}}}', empty: '{"name":"example","versions":{}}', malformed: "{" };
const compactIndexFailures = {
  empty: "---\n", malformed: "---\n1.0.0 dep:>= 0\n", unterminated: "---\n1.0.0 |checksum:aa", utf8: "---\n\xff\n"
};
const sparseIndexFailures = {
  name: '{"name":"other","vers":"1.0.0","cksum":"' + "c".repeat(64) + '"}\n', empty: "", malformed: "{\n",
  unterminated: '{"name":"example"}', utf8: "\xff\n", version: '{"name":"example","cksum":"' + "c".repeat(64) + '"}\n',
  checksum: '{"name":"example","vers":"1.0.0","cksum":"ABC"}\n'
};
require("https").get = (source, options, callback) => {
  if (options.headers["accept-encoding"] !== "identity") throw new Error("missing identity request");
  const npm = source === "https://registry.npmjs.org/example";
  const ruby = source === "https://rubygems.org/info/example";
  const cargo = source === "https://index.crates.io/ex/am/example";
  if (!npm && !ruby && !cargo && source !== "https://pypi.org/simple/example/") throw new Error("unexpected request: " + source);
  const request = new EventEmitter();
  request.setTimeout = () => {};
  request.destroy = error => request.emit("error", error);
  process.nextTick(() => {
    const response = new EventEmitter();
    response.statusCode = process.env.CAPTURE_TEST_CASE === "http" ? 503 : 200;
    response.headers = {
      "content-type": process.env.CAPTURE_TEST_CASE === "media" ? "text/html"
        : npm ? "application/json; charset=utf-8" : ruby ? "text/plain; charset=utf-8"
        : cargo ? "text/plain" : "application/vnd.pypi.simple.v1+json"
    };
    if (process.env.CAPTURE_TEST_CASE === "encoded") response.headers["content-encoding"] = "gzip";
    response.resume = () => {};
    callback(response);
    if (process.env.CAPTURE_TEST_CASE === "aborted") {
      response.emit("aborted");
      return;
    }
    const replacement = (ruby ? compactIndexFailures : cargo ? sparseIndexFailures : jsonFailures)[process.env.CAPTURE_TEST_CASE];
    const body = replacement === undefined
      ? fs.readFileSync(process.env.CAPTURE_TEST_ROOT + (npm ? "/npm.json" : ruby ? "/rubygems.json" : cargo ? "/cargo.json" : "/pypi.json"))
      : Buffer.from(replacement, "latin1");
    response.emit("data", body.subarray(0, 13));
    response.emit("data", body.subarray(13));
    response.emit("end");
  });
  return request;
};
JS
export NODE_OPTIONS="--require=$temporary/https.cjs"
bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm"
cmp "$temporary/npm.json" "$temporary/npm/example.full.json"
cmp "$temporary/pypi.json" "$temporary/pypi/example.simple.json"
cmp "$temporary/rubygems.json" "$temporary/rubygems/example.info"
cmp "$temporary/cargo.json" "$temporary/cargo/example.index.ndjson"
BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm"
node - "$temporary/pins.json" <<'JS'
const assert = require("assert/strict");
const fs = require("fs");
const crypto = require("crypto");
const catalogue = JSON.parse(fs.readFileSync(process.argv[2]));
for (const ecosystem of ["npm", "pypi", "rubygems", "cargo"]) {
  const capture = catalogue.captures[ecosystem].example;
  const raw = fs.readFileSync(process.env.CAPTURE_TEST_ROOT + "/" + ecosystem + ".json");
  assert.equal(capture.bytes, raw.length);
  assert.equal(capture.sha256, crypto.createHash("sha256").update(raw).digest("hex"));
  assert.equal(capture.contentEncoding, "identity");
  assert(Number.isFinite(Date.parse(capture.capturedAt)));
}
assert.equal(catalogue.captures.npm.example.mediaType, "application/json; charset=utf-8");
JS
cp "$temporary/pins.json" "$temporary/before.json"
for failure in http media encoded aborted name empty malformed; do
  if CAPTURE_TEST_CASE="$failure" bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm" > "$temporary/error" 2>&1; then
    echo "capture accepted $failure failure" >&2
    exit 1
  fi
  cmp "$temporary/before.json" "$temporary/pins.json"
  cmp "$temporary/npm.json" "$temporary/npm/example.full.json"
done
# Each case must fail with its own message and leave the catalogue unchanged.
expect_refusal() {
  if BENCH_CORPUS_ECOSYSTEM="$1" CAPTURE_TEST_CASE="$2" bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm" < /dev/null > "$temporary/error" 2>&1; then
    echo "$1 capture accepted $2 failure" >&2
    exit 1
  fi
  if ! grep -qF "$3" "$temporary/error"; then
    echo "$1 $2 failure did not report: $3" >&2
    cat "$temporary/error" >&2
    exit 1
  fi
  cmp "$temporary/before.json" "$temporary/pins.json"
}
while IFS='|' read -r failure message; do
  expect_refusal rubygems "$failure" "$message"
  cmp "$temporary/rubygems.json" "$temporary/rubygems/example.info"
done <<'CASES'
http|registry returned HTTP 503
media|unexpected media type text/html
encoded|expected identity content encoding
aborted|response aborted
empty|malformed or empty Compact Index records
malformed|malformed or empty Compact Index records
unterminated|incomplete Compact Index body
utf8|not valid for encoding utf-8
CASES
while IFS='|' read -r failure message; do
  expect_refusal cargo "$failure" "$message"
  cmp "$temporary/cargo.json" "$temporary/cargo/example.index.ndjson"
done <<'CASES'
http|registry returned HTTP 503
media|unexpected media type text/html
encoded|expected identity content encoding
aborted|response aborted
name|sparse index record names other
empty|empty sparse index
malformed|undecodable sparse index record
unterminated|incomplete sparse index body
utf8|not valid for encoding utf-8
version|sparse index record has no version
checksum|sparse index record has no SHA-256 checksum
CASES
printf '\n' >> "$temporary/npm/example.full.json"
if BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm" > "$temporary/error" 2>&1; then
  echo "verification accepted changed bytes" >&2
  exit 1
fi
grep -q 'capture bytes mismatch' "$temporary/error"
node - <<'JS'
const fs = require("fs");
const root = process.env.CAPTURE_TEST_ROOT;
const body = fs.readFileSync(root + "/npm.json");
body[body.length - 1] = 32;
fs.writeFileSync(root + "/npm/example.full.json", body);
JS
if BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm" > "$temporary/error" 2>&1; then
  echo "verification accepted changed digest" >&2
  exit 1
fi
grep -q 'capture sha256 mismatch' "$temporary/error"
BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$here/../bench/corpus/pins.json" "$here/../bench/corpus/npm"
echo 'gen-bench-corpus tests passed'
