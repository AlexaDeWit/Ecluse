+++
title = "Request handling and metadata"
description = "How Écluse decides what a build may install: the quarantine on new public versions, the four registry roles, and the deny-by-default policy."
weight = 1
+++

Use this reference to follow metadata discovery, downloads, mirroring, and publication.
For the operating purpose and deployment choices, start with
[Understand Écluse](@/docs/understand.md) or [Set up Écluse](@/docs/setup.md).
npm, PyPI, Cargo, and RubyGems support reads, mirroring, and first-party publication.
[Complete PyPI setup](@/docs/pypi-setup.md) connects those roles and their required stores.
[Protocol support](@/docs/protocol-support.md) describes each protocol and its configuration requirements.

A new public version waits in a quarantine, seven days by default, before a build can install it.
The delay reduces exposure to versions removed during that window. It does not establish
that an older version is harmless. An advisory source enables a fast track for exact security fixes
and a deny for known malicious versions. A partial fix passes only when the remaining advisory
scores meet the configured ceilings and gaps. Missing or old advisory evidence can refuse
admission. Higher-ranked denies still apply. The
[rule policy](@/docs/configuration.md#rule-policy) defines the evidence, precedence, and override conditions.

If you run a private registry, Écluse treats its packages as trusted, subject to integrity and
managed-mirror completion checks. Any https registry that speaks the ecosystem's protocol serves
in that role. Écluse can also mirror admitted public versions into a registry you nominate.
A complete mirrored version survives a public outage or yank.

You declare each registry under a tag that names the store behind it. A mirror target under the
`codeArtifact` tag mints its own short-lived write token, and the other tags take a static token
you supply. Package archives remain in your registries.

Écluse ships as one container image with four roles:

- `ecluse proxy` serves clients and runs the mirror worker.
- `ecluse mirror` runs the mirror worker alone, when you want to scale it apart from the proxy.
- `ecluse pilot` builds the advisory database from the OSV exports and the EPSS feed. The fast
  lane, the advisory denies, and Dredger all read it.
- `ecluse dredger` deletes versions your rules deny at revocation from each mount's mirror target
  and private cache. It is the only role that deletes, and [Running the Dredger](@/docs/dredger.md) covers it.

[Deploying Écluse](@/docs/deployment.md) covers all four.

## How it works

### The registry roles

Écluse sees registries by role, and each role is a URL on a mount. Each ecosystem gets its own
path prefix: `/npm/`, `/pypi/`, `/cargo/`, or `/rubygems/`.

| Role | What it does | Trust | Required? |
|------|--------------|-------|-----------|
| Public upstream | The registry Écluse gates | Subject to admission | Defaults to `registry.npmjs.org` for npm, `pypi.org` for PyPI, `index.crates.io` for Cargo, and `rubygems.org` for RubyGems |
| Private upstream | Supplies trusted package versions | Trusted | No. Without it, a pure public gate |
| Mirror target | Receives admitted package files and supplies completion proof | Operator-controlled | No. Declaring it enables mirroring for the mount |
| Publication target | Receives your `publish` requests | Trusted | Opt-in |

These are roles, not necessarily separate servers. The
[recommended topology](@/docs/deployment.md#the-recommended-topology) gives each one its own store
and explains what you lose when two share one.

### A request, step by step

For npm, a client requests a version listing and then a tarball. PyPI clients request project indexes
and distribution files. Cargo clients read sparse discovery, native index records, and crate archives.
RubyGems clients read a Compact Index directory, per-gem metadata, and gem archives.
Cargo's `/cargo/` registry replaces crates.io and merges private and public crates like the other ecosystems.
The [Cargo protocol contract](@/docs/protocol-support.md#cargo) defines its discovery and source rules.

{{ diagram(name="metadata-admission", alt="The proxy combines caller-authorised private metadata with admitted anonymous public metadata. First-party names stay private, and response reuse follows fresh private authorisation.", caption="Version discovery and admission. Advisory polling runs independently of each request.") }}

1. **The listing.** For npm, PyPI, Cargo, and RubyGems non-first-party names, Écluse fetches private and public metadata in parallel.
   If the private read fails, the listing refuses with `503` rather than serving public versions alone.
   Private versions must meet the trusted integrity floor, and managed mirror versions must also prove completion.
   The policy filters public versions before the listing is merged. For npm, `dist-tags.latest` follows an admitted
   public tag, otherwise the highest admitted version, with stable versions before prereleases.
   It never exposes a private or mirror store's `latest` tag.
2. **The artifact.** Private files use the trusted path. A managed mirror hit also requires current
   authorisation, matching target identities, and a complete expected version. A private `404` uses current
   public metadata and policy, and a failed private read refuses the download.
   An admitted public request delivers the file and queues a whole-version job
   when mirroring is configured. Clients verify streamed archives against checksums from admitted metadata.
   For PyPI, that job copies every supported wheel and sdist in the admitted version.
   For RubyGems, it copies every platform archive in the admitted version.
3. **A publish.** Publishing stays off until you configure a publication target. With one, Écluse
   refuses any name outside the mount's first-party namespaces before it writes upstream.

{{ diagram(name="artifact-downloads", alt="An artifact request prefers the private source. An admitted public fallback delivers the file and queues a version trigger when mirroring is configured.", caption="Downloads and the trigger for background mirroring.") }}

Private versions do not re-enter public admission. Private metadata passes the trusted integrity
floor, but a conventional private npm artifact hit bypasses that metadata path.
Those reads rely on the client's integrity checks.
A public/private digest conflict logs a WARNING with both digests and increments the divergence
counter. Comparison uses complete digest sets for each shared filename and algorithm.
Hexadecimal case does not affect agreement. Diagnostics keep the original digest spelling.
An unmanaged private copy stays listed and readable under the existing trusted rules.
The alarm remains when a public rule denies the conflicting copy. The denied public version stays
excluded. Only metadata that passes identity, artifact-location, and integrity checks supplies conflict evidence.
A disagreement proves neither copy correct. An unmanaged private copy can still be served because the
private repository is trusted to hold previously admitted or first-party content. Managed mirror
reads must still pass their completeness checks.
Divergence alone never authorises Dredger to delete a version. Removing an allow does not revoke a mirrored
version unless a named deny that applies at revocation becomes decisive. See
[Revoking a mirrored version](@/docs/operations.md#revoking-a-mirrored-version-internal-yank).

npm and PyPI listings require a usable filename in each artifact URL.
They omit `.` and `..` names and filenames with a backslash or control character.
The check also applies after one percent decode and refuses encoded separators or invalid UTF-8.
A release disappears when no file survives.

PyPI mirroring retains the supported file inventory and consumer metadata. Its worker extracts native
upload fields from verified archives. [PyPI uploads and mirror state](@/docs/configuration.md#pypi-uploads-and-mirror-state)
explains completion and recovery through partial copies.

### npm mirror metadata

The npm mirror write republishes supported metadata from the public version object. It retains
dependencies, executable mappings, engines, scripts, and deprecation notices. The worker reads
that object at admission time, never from the queued job.

- Écluse keeps the [supported npm fields](@/docs/protocol-support.md#npm-metadata-fields). Fields outside that contract do not survive extraction.
- Écluse rewrites only what it verified itself: the name, the version, the attachment, and the
  `dist` location and digests. A digest the public registry declared but the worker did not verify
  is dropped rather than republished.
- Écluse strips `dist.signatures`, `dist.attestations`, and underscore-prefixed bookkeeping fields.
  It retains `_hasShrinkwrap` because this marker affects installation.
- Missing or inconsistent mirror upload data stops mirroring before the artifact download. The worker
  checks the admitted package, version, and complete npm tarball inventory against the publication
  input. It retires a refused job without publishing and logs the reason apart from a policy denial.

Écluse never edits a version the mirror already holds, so a copy that an earlier release mirrored
with the shorter manifest keeps it. To repair one, delete that version from the mirror target with
the store's own tooling (`DeletePackageVersions` on CodeArtifact, an unpublish on Verdaccio), then
request the version through the proxy again. The next tarball request enqueues a fresh mirror job,
which republishes the version with its full metadata. A separate private cache that retained the
old copy keeps serving it until you remove that copy too.

Each mirror write sets `dist-tags.latest` on the mirror target. Écluse reads the public
registry's own `latest` and the versions the mirror holds after the write, then names the public
target when the mirror holds it. Otherwise it names the highest stable version present, or the
highest prerelease when no stable version is present. Mirroring one version never makes that
version `latest` by itself, so back-filling an old version does not change what an unqualified
install resolves. Écluse owns `latest` on every public package it mirrors and overwrites a value
it did not set. That stored tag governs a client pointed straight at the mirror, never the listing
Écluse serves. First-party names never reach the mirror worker, and their release tags are
untouched.

A mirror job whose target metadata cannot be read is not published, because the tag cannot be
chosen without the current inventory. It follows the worker's usual fault handling instead, which
retries a transport failure and retires a fault no redelivery can clear. Two workers mirroring the
same package at the same instant still race: an npm publish carries no compare-and-set, so the last
write wins. The next job that publishes a new version of that package corrects the tag.

Before publication, the mirror worker verifies fetched bytes against the current admitted
metadata. For npm `dist.integrity`, any matching SRI alternative at the strongest algorithm
permits publication, regardless of token order. A matching weaker digest cannot rescue a
stronger-algorithm mismatch. This alternative rule applies only to SRI components.

### The policy

The policy is deny by default: a public version reaches a build only when a rule admits it. Rules
run in precedence order and the first decisive one wins. The revoke, the install-time deny and the
external-dependency deny sit above every allow by default.

| Rule | Band | On by default | What it decides |
|------|------|----------------|-----------------|
| `min-age` | Allow | Yes | Admits a public version older than seven days: the quarantine |
| `remediation-fast-track` | Allow | With an advisory source | Admits a version a synced advisory names as the exact fix for a vulnerability. A fix that another advisory still affects passes only when that advisory's stored scores sit inside the limits. Refuses while advisory evidence is unavailable |
| `AllowScope` | Allow | No | Allow-lists every package under a scope you name |
| `AllowByIdentity` | Allow | No | Pins a package or a `package@version` by exact name |
| `DenyByIdentity` | Deny | No | Revokes a package or version |
| `DenyInstallTimeExecution` | Deny | npm only | `deny-scripts` denies npm install scripts at admission, leaving stored copies in place |
| `DenyExternalDependencies` | Deny | npm only | `deny-external-dependencies` denies, at admission, a public npm version whose installable dependency names a git source or a remote tarball URL, which a client would fetch past Écluse. Stored copies stay |
| `DenyIfCve` | Deny | No | Denies versions with a known vulnerability above a severity you choose |
| `DenyIfEpss` | Deny | No | Denies versions with a known vulnerability whose exploitation probability (EPSS) is at or above a threshold you choose |

An advisory source (`advisories.url`) activates `remediation-fast-track`, together with the
`deny-known-malicious` MAL deny. Without a source, only the quarantine supplies the shared allow.
With one, the fast lane refuses a version it reaches while the advisory database is missing,
unreadable, or too old, so an outage also holds back old versions until the database recovers. The
npm mount also denies, by default, install scripts and dependencies fetched from outside the registry.
Disable `deny-scripts` or `deny-external-dependencies` under `mounts.npm.rules`, or add an
`AllowByIdentity` pin with precedence above 300.
The off rules opt in by name, and
[Rule policy](@/docs/configuration.md#rule-policy) has their knobs.

Independent of the rules, Écluse serves a public **file** only if it carries a digest that meets
the public integrity floor, `sha256` by default. The check runs per file, so a release that ships
several, as a Python release ships an sdist beside its wheels, keeps the files that clear the
floor and loses the ones that do not. A release disappears only when no file of it survives. On a
custom or off-spec public upstream this has a gotcha: files without such a digest silently
disappear, and a public download is refused with `403`. Lowering the trusted floor can permit
weaker supported digests, but not hashless metadata entries. The private role also bypasses
public admission, so use it only for a source you already trust. Conventional private npm
artifact reads have a separate client-integrity contract. The mechanics are in
[Integrity floors](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#integrity-floors).
