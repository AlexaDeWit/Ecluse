#!/usr/bin/env bash
# Capture complete registry bodies. Set BENCH_CORPUS_VERIFY=1 for offline verification.
# Set BENCH_CORPUS_ECOSYSTEM=npm or pypi to select one catalogue section.
set -euo pipefail

pins_file="${1:-bench/corpus/pins.json}"
outdir="${2:-bench/corpus/npm}"

node - "$pins_file" "$outdir" "${BENCH_CORPUS_ECOSYSTEM:-all}" <<'JS'
const fs = require("fs");
const https = require("https");
const path = require("path");
const crypto = require("crypto");

const [pinsPath, outDir, ecosystem] = process.argv.slice(2);
const catalogue = JSON.parse(fs.readFileSync(pinsPath, "utf8"));
const verify = process.env.BENCH_CORPUS_VERIFY === "1";
if (!["all", "npm", "pypi"].includes(ecosystem)) throw new Error("unknown corpus ecosystem: " + ecosystem);
catalogue.captures ||= {};

function stem(name) {
  return name.replace(/^@/, "").replace(/\//, "-");
}

function fetchDocument(source, accept) {
  return new Promise((resolve, reject) => {
    const req = https.get(source, {
      headers: { accept, "accept-encoding": "identity", "user-agent": "ecluse-registry-capture" }
    }, (res) => {
      if (res.statusCode !== 200) {
        res.resume();
        reject(new Error(`${source}: registry returned HTTP ${res.statusCode}`));
        return;
      }
      if (res.headers["content-encoding"] && res.headers["content-encoding"] !== "identity") {
        res.resume();
        reject(new Error(`${source}: expected identity content encoding`));
        return;
      }
      const chunks = [];
      let bytes = 0;
      res.on("error", reject);
      res.on("aborted", () => reject(new Error(`${source}: response aborted`)));
      res.on("data", chunk => {
        bytes += chunk.length;
        if (bytes > 128 * 1024 * 1024) {
          req.destroy(new Error(`${source}: capture exceeds 128 MiB`));
          return;
        }
        chunks.push(chunk);
      });
      res.on("end", () => resolve({
        body: Buffer.concat(chunks),
        mediaType: res.headers["content-type"],
        capturedAt: new Date().toISOString()
      }));
    });
    req.on("error", reject);
    req.setTimeout(60000, () => req.destroy(new Error(`${source}: registry fetch timed out`)));
  });
}

function validate(body, mediaType, expectedType, name, eco) {
  if (typeof mediaType !== "string" || mediaType.split(";")[0].trim() !== expectedType) {
    throw new Error(`${name}: unexpected media type ${mediaType}`);
  }
  const document = JSON.parse(body.toString("utf8"));
  if (document.name !== name) throw new Error(`${name}: response names ${document.name}`);
  const entries = eco === "npm" ? document.versions : document.files;
  if (!entries || typeof entries !== "object" || Object.keys(entries).length === 0
      || (Array.isArray(entries) !== (eco === "pypi"))) {
    throw new Error(`${name}: response has no ${eco === "npm" ? "versions" : "files"}`);
  }
  return Object.keys(entries).length;
}

async function capture(eco, name) {
  const npm = eco === "npm";
  const source = npm ? "https://registry.npmjs.org/" + name.replace("/", "%2f")
    : "https://pypi.org/simple/" + encodeURIComponent(name) + "/";
  const expectedType = npm ? "application/json" : "application/vnd.pypi.simple.v1+json";
  const file = npm ? path.join(outDir, stem(name) + ".full.json")
    : path.join(path.dirname(outDir), "pypi", name + ".simple.json");
  const existing = catalogue.captures[eco]?.[name];
  if (verify && !existing) throw new Error(`${name}: missing capture record`);
  const response = verify ? { ...existing, body: fs.readFileSync(file) }
    : await fetchDocument(source, expectedType);
  const count = validate(response.body, response.mediaType, expectedType, name, eco);
  const sha256 = crypto.createHash("sha256").update(response.body).digest("hex");
  const record = {
    path: path.relative(path.dirname(pinsPath), file), source,
    mediaType: response.mediaType, contentEncoding: "identity",
    bytes: response.body.length, sha256, capturedAt: response.capturedAt
  };
  if (verify) {
    for (const key of Object.keys(record)) {
      if (existing[key] !== record[key]) throw new Error(`${name}: capture ${key} mismatch`);
    }
    if (!Number.isFinite(Date.parse(record.capturedAt))) throw new Error(`${name}: invalid capture time`);
  } else {
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, response.body);
    catalogue.captures[eco] ||= {};
    catalogue.captures[eco][name] = record;
    fs.writeFileSync(pinsPath, JSON.stringify(catalogue, null, 2) + "\n");
  }
  console.log(`${eco}/${name}: ${record.bytes} bytes, ${count} ${npm ? "versions" : "files"}, sha256 ${sha256}`);
}

(async () => {
  for (const [eco, pins] of [["npm", catalogue.pins], ["pypi", catalogue.pypiPins]]) {
    if (ecosystem === "all" || ecosystem === eco) {
      for (const name of Object.keys(pins || {})) await capture(eco, name);
    }
  }
})().catch(error => {
  console.error("gen-bench-corpus failed: " + error.message);
  process.exit(1);
});
JS
