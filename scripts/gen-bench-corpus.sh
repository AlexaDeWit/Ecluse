#!/usr/bin/env bash
# Capture frozen registry metadata using bench/corpus/pins.json.
# npm uses node-semver to retain stable releases through the pin.
# PyPI retains PEP 691 files uploaded by the pinned release's last upload.
# Registry edits and deletions can change a deliberate recapture. Review its diff.
# Set BENCH_CORPUS_ECOSYSTEM=npm or pypi to capture one ecosystem.
set -euo pipefail

pins_file="${1:-bench/corpus/pins.json}"
outdir="${2:-bench/corpus/npm}"
mkdir -p "$outdir"

node - "$pins_file" "$outdir" "${BENCH_CORPUS_ECOSYSTEM:-all}" <<'JS'
const fs = require("fs");
const https = require("https");
const path = require("path");
const semver = require("semver");

const [pinsPath, outDir, ecosystem] = process.argv.slice(2);
const catalogue = JSON.parse(fs.readFileSync(pinsPath, "utf8"));
const pins = catalogue.pins || {};
if (!["all", "npm", "pypi"].includes(ecosystem)) throw new Error("unknown corpus ecosystem: " + ecosystem);

// The package name as a filesystem-safe fixture stem: drop the leading scope '@'
// and turn the scope separator '/' into '-' (so '@types/node' -> 'types-node').
function stem(name) {
  return name.replace(/^@/, "").replace(/\//, "-");
}

// The registry path for a (possibly scoped) name: the scope separator is %2f
// encoded, the leading '@' kept literal: the form the registry serves.
function registryPath(name) {
  return "/" + name.replace("/", "%2f");
}

function fetchDocument(host, requestPath, accept) {
  const name = host + requestPath;
  return new Promise((resolve, reject) => {
    const req = https.get(
      { host, path: requestPath, headers: { accept } },
      (res) => {
        if (res.statusCode !== 200) {
          res.resume();
          reject(new Error(`${name}: registry returned HTTP ${res.statusCode}`));
          return;
        }
        const chunks = [];
        res.on("data", (c) => chunks.push(c));
        res.on("end", () => {
          try {
            resolve(JSON.parse(Buffer.concat(chunks).toString("utf8")));
          } catch (e) {
            reject(new Error(`${name}: response did not parse: ${e.message}`));
          }
        });
      }
    );
    req.on("error", reject);
    req.setTimeout(60000, () => req.destroy(new Error(`${name}: registry fetch timed out`)));
  });
}

function trimVersion(v) {
  for (const k of [
    "readme", "gitHead", "_npmUser", "_npmOperationalInternal", "_hasShrinkwrap",
    "_nodeVersion", "_npmVersion", "_engineSupported", "_defaultsLoaded", "contributors",
  ]) {
    delete v[k];
  }
  return v;
}

async function capture(name, pin) {
  if (!semver.valid(pin)) throw new Error(`${name}: pin "${pin}" is not a valid semver version`);
  const pkmt = await fetchDocument("registry.npmjs.org", registryPath(name), "application/json");
  const versions = pkmt.versions || {};

  const kept = {};
  for (const [v, manifestEntry] of Object.entries(versions)) {
    if (semver.valid(v) && semver.prerelease(v) === null && semver.lte(v, pin)) {
      kept[v] = trimVersion(manifestEntry);
    }
  }
  if (Object.keys(kept).length === 0) throw new Error(`${name}: no stable versions <= ${pin}`);

  const time = {};
  const srcTime = pkmt.time || {};
  if (srcTime.created !== undefined) time.created = srcTime.created;
  if (srcTime.modified !== undefined) time.modified = srcTime.modified;
  for (const v of Object.keys(kept)) if (srcTime[v] !== undefined) time[v] = srcTime[v];

  const distTags = { latest: pin };
  for (const [tag, v] of Object.entries(pkmt["dist-tags"] || {})) {
    if (tag !== "latest" && kept[v] !== undefined) distTags[tag] = v;
  }

  const out = {};
  out.name = pkmt.name;
  out["dist-tags"] = distTags;
  out.versions = kept;
  out.time = time;
  for (const k of ["maintainers", "description", "homepage", "repository", "bugs", "license", "keywords"]) {
    if (pkmt[k] !== undefined) out[k] = pkmt[k];
  }

  const file = path.join(outDir, stem(name) + ".full.json");
  fs.writeFileSync(file, JSON.stringify(out));
  const kb = Math.round(fs.statSync(file).size / 1024);
  console.log(`${name.padEnd(22)} @ ${pin.padEnd(10)} -> ${path.basename(file).padEnd(28)} ${String(Object.keys(kept).length).padStart(5)} versions  ${String(kb).padStart(6)} KiB`);
}

async function capturePyPI(name, pin) {
  const pinPath = "/pypi/" + encodeURIComponent(name) + "/" + encodeURIComponent(pin) + "/json";
  const release = await fetchDocument("pypi.org", pinPath, "application/json");
  if (!Array.isArray(release.urls) || release.urls.length === 0) throw new Error(name + ": pin has no files");
  const timestamp = (value, label) => {
    const time = typeof value === "string" ? Date.parse(value) : NaN;
    if (!Number.isFinite(time)) throw new Error(name + ": missing or invalid upload time for " + label);
    return time;
  };
  const cutoff = Math.max(...release.urls.map(file => timestamp(file.upload_time_iso_8601, file.filename)));
  const indexPath = "/simple/" + encodeURIComponent(name) + "/";
  const index = await fetchDocument("pypi.org", indexPath, "application/vnd.pypi.simple.v1+json");
  if (!Array.isArray(index.files)) throw new Error(name + ": Simple index has no files");
  const files = index.files.filter(file => timestamp(file["upload-time"], file.filename) <= cutoff);
  if (files.length === 0) throw new Error(name + ": capture has no files");
  const versions = new Set(files.map(file => {
    const filename = file.filename;
    if (filename.endsWith(".whl")) return filename.split("-")[1];
    const stem = filename.replace(/\.(tar\.gz|tar\.bz2|tar\.xz|zip)$/, "");
    return stem.slice(name.length + 1);
  }));
  const out = { ...index, files };
  if (Array.isArray(index.versions)) out.versions = index.versions.filter(version => versions.has(version));
  delete out["_last-serial"];
  if (out.meta) delete out.meta["_last-serial"];
  const destination = path.join(path.dirname(outDir), "pypi");
  fs.mkdirSync(destination, { recursive: true });
  fs.writeFileSync(path.join(destination, name + ".simple.json"), JSON.stringify(out));
  fs.writeFileSync(path.join(destination, name + ".capture.json"), JSON.stringify({
    package: name, version: pin,
    source: "https://pypi.org" + indexPath,
    mediaType: "application/vnd.pypi.simple.v1+json",
    pinSource: "https://pypi.org" + pinPath,
    uploadCutoff: new Date(cutoff).toISOString(),
    files: files.length
  }));
  console.log(name + " @ " + pin + ": " + files.length + " Simple files through " + new Date(cutoff).toISOString());
}

(async () => {
  if (ecosystem !== "pypi") {
    for (const [name, pin] of Object.entries(pins)) await capture(name, pin);
  }
  if (ecosystem !== "npm") {
    for (const [name, pin] of Object.entries(catalogue.pypiPins || {})) await capturePyPI(name, pin);
  }
})().catch((e) => {
  console.error("gen-bench-corpus failed: " + e.message);
  process.exit(1);
});
JS

echo "captured ${BENCH_CORPUS_ECOSYSTEM:-all} corpus"
