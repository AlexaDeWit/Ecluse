#!/usr/bin/env bash
# Exercise byte preservation and provenance without contacting a registry.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p "$here/../scratchpad"
temporary="$(mktemp -d "$here/../scratchpad/gen-bench-corpus.XXXXXX")"
trap 'rm -rf "$temporary"' EXIT
export CAPTURE_TEST_ROOT="$temporary"
cat > "$temporary/pins.json" <<'JSON'
{"pins":{"example":"1.0.0"},"pypiPins":{"example":"1.0.0"}}
JSON
cat > "$temporary/npm.json" <<'JSON'
{ "name": "example", "versions": {"1.0.0": {}, "9.0.0-rc.1": {"contributors": ["A"]}},
  "unrecognised": 1e2, "dist-tags": {"latest": "9.0.0-rc.1"} }
JSON
cat > "$temporary/pypi.json" <<'JSON'
{ "name": "example", "files": [{"filename": "example-9.0.0rc1.tar.gz", "upload-time": "2099-01-01T00:00:00Z"}],
  "versions": ["9.0.0rc1"], "meta": {"_last-serial": 123}, "_last-serial": 123 }
JSON
cat > "$temporary/https.cjs" <<'JS'
const fs = require("fs");
const { EventEmitter } = require("events");
require("https").get = (source, options, callback) => {
  if (options.headers["accept-encoding"] !== "identity") throw new Error("missing identity request");
  const npm = source === "https://registry.npmjs.org/example";
  if (!npm && source !== "https://pypi.org/simple/example/") throw new Error("unexpected request: " + source);
  const request = new EventEmitter();
  request.setTimeout = () => {};
  request.destroy = error => request.emit("error", error);
  process.nextTick(() => {
    const response = new EventEmitter();
    response.statusCode = process.env.CAPTURE_TEST_CASE === "http" ? 503 : 200;
    response.headers = {
      "content-type": process.env.CAPTURE_TEST_CASE === "media" ? "text/html"
        : npm ? "application/json; charset=utf-8" : "application/vnd.pypi.simple.v1+json"
    };
    if (process.env.CAPTURE_TEST_CASE === "encoded") response.headers["content-encoding"] = "gzip";
    response.resume = () => {};
    callback(response);
    if (process.env.CAPTURE_TEST_CASE === "aborted") {
      response.emit("aborted");
      return;
    }
    let body = fs.readFileSync(process.env.CAPTURE_TEST_ROOT + (npm ? "/npm.json" : "/pypi.json"));
    if (process.env.CAPTURE_TEST_CASE === "name") body = Buffer.from('{"name":"other","versions":{"1":{}}}');
    if (process.env.CAPTURE_TEST_CASE === "empty") body = Buffer.from('{"name":"example","versions":{}}');
    if (process.env.CAPTURE_TEST_CASE === "malformed") body = Buffer.from("{");
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
BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm"
node - "$temporary/pins.json" <<'JS'
const assert = require("assert/strict");
const fs = require("fs");
const crypto = require("crypto");
const catalogue = JSON.parse(fs.readFileSync(process.argv[2]));
for (const ecosystem of ["npm", "pypi"]) {
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
printf '\n' >> "$temporary/npm/example.full.json"
if BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$temporary/pins.json" "$temporary/npm" > "$temporary/error" 2>&1; then
  echo "verification accepted changed bytes" >&2
  exit 1
fi
rg -q 'capture bytes mismatch' "$temporary/error"
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
rg -q 'capture sha256 mismatch' "$temporary/error"
BENCH_CORPUS_VERIFY=1 bash "$here/gen-bench-corpus.sh" "$here/../bench/corpus/pins.json" "$here/../bench/corpus/npm"
echo 'gen-bench-corpus tests passed'
