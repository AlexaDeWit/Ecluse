#!/usr/bin/env bash
#
# Capture the real-world packument corpus that drives both benchmark layers
# (docs/architecture/performance.md) from pinned npm registry packuments.
#
# The corpus packages and their capture pins live in bench/corpus/package.json,
# kept fresh by Renovate's npm manager exactly as test/oracles is. This script is
# the analogue of scripts/gen-version-fixtures.sh: it is the regeneration tool, run
# only when the pins change (a Renovate bump) or a capture-policy change. Its output
# (bench/corpus/npm/*.full.json) is committed and read at run time by the Layer A
# micro-benches (Ecluse.Bench.Corpus) and the Layer B load harness
# (Ecluse.BenchLoad.Npm).
#
# Usage:  make gen-bench-corpus   (runs inside the Nix dev shell, which carries
#                                  node + node-semver on NODE_PATH)
#
# Determinism w.r.t. the pin: for each package@version pin, the live full packument
# is fetched and reduced to the versions at or below the pinned version, so a re-run
# without a pin bump reproduces the same fixture (versions published after the pin
# are excluded) — the dataset stays committed and deterministic and only moves when
# Renovate moves a pin.
#
# Capture policy (preserve real shape; trim only noise):
#   * KEEP every stable release (no prerelease tag) at or below the pin, with its
#     full per-version manifest — the heterogeneous dependency / peerDependencies /
#     engines / deprecated / scripts / dist shape the hot paths read and re-serialise.
#   * DROP the degenerate nightly/canary/dev/insiders PRERELEASE versions: they are
#     near-identical day-to-day builds (the synthetic generator's degeneracy), and
#     for typescript/react they are the bulk of the size while adding no real shape.
#   * DROP pure-noise fields no hot path reads: top-level readme/users/_attachments,
#     and per-version readme / npm operational internals.
# express.full.json is deliberately NOT regenerated here: it is the pre-existing
# untrimmed anchor under core/test/unit/fixtures/npm/, reused in place by the bench
# and shared with the unit suite.
set -euo pipefail

manifest="${1:-bench/corpus/package.json}"
outdir="${2:-bench/corpus/npm}"
mkdir -p "$outdir"

node - "$manifest" "$outdir" <<'JS'
const fs = require("fs");
const https = require("https");
const path = require("path");
const semver = require("semver");

const [manifestPath, outDir] = process.argv.slice(2);
const manifest = JSON.parse(fs.readFileSync(manifestPath, "utf8"));
const pins = manifest.dependencies || {};

// The package name as a filesystem-safe fixture stem: drop the leading scope '@'
// and turn the scope separator '/' into '-' (so '@types/node' -> 'types-node').
function stem(name) {
  return name.replace(/^@/, "").replace(/\//, "-");
}

// The registry path for a (possibly scoped) name: the scope separator is %2f
// encoded, the leading '@' kept literal — the form the registry serves.
function registryPath(name) {
  return "/" + name.replace("/", "%2f");
}

function fetchPackument(name) {
  return new Promise((resolve, reject) => {
    const req = https.get(
      { host: "registry.npmjs.org", path: registryPath(name), headers: { accept: "application/json" } },
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

// Strip the per-version noise fields no hot path reads, preserving the manifest's
// real heterogeneous shape (dependencies, peerDependencies, engines, deprecated,
// scripts, dist, ...).
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
  const pkmt = await fetchPackument(name);
  const versions = pkmt.versions || {};

  // Keep every stable release at or below the pin; drop prereleases and anything
  // published past the pin (so the capture is deterministic w.r.t. the pin).
  const kept = {};
  for (const [v, manifestEntry] of Object.entries(versions)) {
    if (semver.valid(v) && semver.prerelease(v) === null && semver.lte(v, pin)) {
      kept[v] = trimVersion(manifestEntry);
    }
  }
  if (Object.keys(kept).length === 0) throw new Error(`${name}: no stable versions <= ${pin}`);

  // Restrict the time map to the kept versions, keeping the created/modified
  // bookkeeping keys the projection ignores but a real document carries.
  const time = {};
  const srcTime = pkmt.time || {};
  if (srcTime.created !== undefined) time.created = srcTime.created;
  if (srcTime.modified !== undefined) time.modified = srcTime.modified;
  for (const v of Object.keys(kept)) if (srcTime[v] !== undefined) time[v] = srcTime[v];

  // Rebuild dist-tags so latest is the pin (a kept version) and every other tag that
  // survives points at a kept version — no dangling tag onto a dropped prerelease.
  const distTags = { latest: pin };
  for (const [tag, v] of Object.entries(pkmt["dist-tags"] || {})) {
    if (tag !== "latest" && kept[v] !== undefined) distTags[tag] = v;
  }

  // Reassemble a faithful packument: the real top-level fields the wire decode reads,
  // minus the pure-noise blobs (readme/users/_attachments and the _id/_rev couch keys).
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

(async () => {
  for (const [name, pin] of Object.entries(pins)) {
    await capture(name, pin);
  }
})().catch((e) => {
  console.error("gen-bench-corpus failed: " + e.message);
  process.exit(1);
});
JS

echo "captured $(ls -1 "$outdir"/*.full.json | wc -l) packument(s) into $outdir"
