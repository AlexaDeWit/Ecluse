# Configuration and authentication

> Part of the [Écluse architecture overview](../architecture.md).

Why Écluse's configuration has the shape it has: two layers, a derived mount, safety floors that
fail closed, and client authentication at the edge.

## Configuration

> **Operators:** [`config/default.yaml`](../../config/default.yaml) documents every key and its
> default, and [the operator manual](https://ecluse-proxy.com/docs/) covers the environment-variable mapping,
> client setup, and the network-egress checklist. This document holds the design rationale.

Configuration has two layers. Environment variables carry process-level and secret values. A
structured YAML document carries the two things too expressive for flat env vars: the **rule
policy** and the **mount map**. The rule policy earns the document its keep: per-rule
precedence and value overrides, layered over a built-in default (see [Rule policy](#rule-policy)).

A mount's shape is **derived, not declared**. Any operator-supplied key under `mounts.<ecosystem>`
activates it, and a declared `mirrorTarget`, not a mode flag, makes it mirrored. A mirrored mount
then requires a `privateUpstream` so the mirror reads back.

Each endpoint a mount names is one **tagged target**: an object with exactly one key, the tag, and
under it the keys that tag admits there. The tag names the store backend and the load checks the
URL against it, so a `codeArtifact:` endpoint whose host is not a CodeArtifact endpoint refuses at
load naming the key. There is no bare-URL shorthand, because one fact with two spellings drifts.
The operator manual carries the admission matrix. Two consequences fall out of the layering rule.
Layers union objects, so an environment variable under a tag the document did not use leaves two
tags on the endpoint and refuses as such: a layer fills keys under a tag, it never switches one.
And two endpoints that name one registry under different tags refuse for every role, because one
store has one backend.

Secrets never live in the structured config. A token is always an environment variable. A mirror
target under the minting tag holds none: the worker mints a short-lived token from ambient cloud
credentials instead (see [Outbound registry credentials](#outbound-registry-credentials)).

### Registry endpoints must be https

Every registry endpoint must be an `https://` URL: the private and public upstreams, the mirror
target, and the publication target. A plain-HTTP endpoint fails closed at boot with an error naming
the URL.

Certificate validation is the endpoint-authentication boundary. The proxy pre-bakes no custom CA
trust. It upgrades a plaintext `dist.tarball` to https when the legacy upstream advertises it on
its own host. It drops a plaintext tarball on any other host and skips that version.

### The TLS trust store

The CA bundle decides which servers Écluse believes, and Écluse has no second check behind it: it
verifies neither Certificate Transparency nor revocation. Its TLS stack (`crypton-x509-store`) also
takes every certificate in the `SSL_CERT_FILE` bundle as a trust anchor for servers and ignores
OpenSSL trust rules. A `TRUSTED CERTIFICATE` block that rejects servers still anchors a server
chain. So the distroless image ships one bundle of plain certificates: the roots NSS trusts for TLS
servers, minus the roots that eMudhra's compromised intermediate CAs chain to and the other emSign
roots Mozilla removed with them. The release build refuses an image whose bundle holds a trust-rule
block or one of those roots.
[Trusted certificate authorities](https://ecluse-proxy.com/docs/deployment/#trusted-certificate-authorities)
lists the roots and covers a bundle an operator brings.

### Upstream composition (optional)

`mounts.npm.privateUpstream` may point at a single registry, or at one that aggregates
others: a CodeArtifact repository with upstream relationships to the mirror-target and first-party
repos. One fetch then returns the whole trusted set. This is an optimisation, never a precondition,
because Écluse [merges packuments across upstreams](registry-model.md#packument-merge-across-upstreams)
itself. One rule keeps it safe: the aggregator must not add a direct connection to the public
registry, which would route unvetted packages around the gate. The proxy always fetches and gates
the public upstream itself.

### Outbound registry credentials

A **mirrored** mount holds a credential to write its mirror target. The async worker uses
Écluse's own identity. Proxy private reads forward the caller's credential and never use that
write credential. Dredger preview holds a separate target-bound credential for private-cache
observations, because no caller supplies one to an autonomous maintenance process.

A credential is always its own key, never part of an endpoint. Écluse refuses a registry URL
carrying userinfo, a query string, or a fragment at load, and the error names the key. That refusal
lets the boot-time configuration echo, the endpoint-collision warnings, and the mount posture lines
print a configured endpoint as written.

The tag is the declaration: it names the store behind an endpoint, and the load checks the URL
against it, so Écluse can never pair a credential with an endpoint it was not scoped for. The
`codeArtifact` tag requires a host of the shape
`{domain}-{owner}.d.codeartifact.{region}.amazonaws.com`, which encodes the whole mint identity, so
the worker and Dredger mint short-lived tokens scoped to each target's domain. The tag
admits no static token for either mirror writes or private observations. The two non-minting
tags each require a `token` under a mirror target.

A tag never moves the proxy's credential posture. Private reads stay per-caller passthrough,
public reads stay anonymous. A `publicationTarget` token replaces the authenticated edge token on
every publish, so Écluse's own credential becomes its authority at the publication target and the
client's token only grants access to Écluse. The boot refuses that token without an edge token,
because an open edge would let any caller publish under it. Dredger uses its private credential for
maintenance only, including cache cleanup.

The CodeArtifact mint is per domain. Mirror and private maintenance consumers whose resolved
mint identities coincide share one
[`CredentialProvider`](cloud-backends.md#the-credential-mint): one mint, one refresh, one breaker.

### Outbound egress safety

Écluse constrains its own outbound fetches. It applies an https-only host allowlist, a literal
internal-range block on the `dist.tarball` host, and certificate validation that authenticates the
dialled host. Network egress is still a shared responsibility: the deployment must also fence
egress at the platform layer, with security groups, `NetworkPolicy`, or Istio egress policy. See
[Securing network egress](https://ecluse-proxy.com/docs/deployment/#network-egress).

One application-level knob adjusts threat tolerance, and it only tightens: it widens the fixed
internal-range set with operator-supplied CIDRs. The tarball-host gate derives from the mount's own
endpoints and takes no operator setting. See the
[configuration reference](https://ecluse-proxy.com/docs/configuration/#the-configuration-reference)
for the names and values.

### Runtime sizing: cores and heap ceiling

The resolved posture seeds a second derivation, the **memory plan**. It partitions the effective
heap ceiling between named tenants. Their sum is an accounting plan, not a measurement of all
live allocations:

- a runtime reserve
- the metadata cache
- the publish-body aggregate
- the in-memory queue tenant, when selected
- the enqueue buffer
- the mirror-artifact envelope, when any mount mirrors

The mirror-artifact tenant covers the worker's transient buffers. For npm, the worker buffers
a tarball and base64-encodes it into a publish document. The archive bytes, base64 text, and
serialised document coexist before collection. PyPI uses native distribution uploads and
transient archive metadata instead of an npm publish envelope. The derivation sets the worker fetch cap
(`maxArtifactBytes`) so that this envelope is what the tenant charges. An explicit config value
wins its own bound, and otherwise the shipped fallbacks apply.

A pod below the automatic plan's floors sheds optional tenants in a documented order. The
mirror-artifact cap goes first, to zero, so the background back-fill leg gives way before the serve
hot path. The cache goes next, also to zero, and each step logs a loud warning. The boot and
`check-config` alike refuse only an explicit override that breaks the plan.

Metadata ingest, CPU concurrency, memory admission and cache retention have separate controls. A
larger source body does not automatically enlarge a cache or reduce CPU concurrency. The fixed
ingest ceiling bounds source bytes. The default byte and count ceilings allow growth beyond the
captured large-package corpus. They express bounded policy headroom, not measured limits on heap use.

The boot sizes the allocation area and the heap ceiling together from the cgroup memory limit, and
keeps every core the ladder resolved. A smaller nursery costs some collector time, where shedding a
core would cost that core's throughput. A configured heap ceiling tighter than the memory limit, or
set with no memory limit, takes the limit's place, so the nursery still fits. Since GHC 9.6 the
nursery counts inside `-M`, so the ceiling reserves only what the heap does not cover: native and
kernel memory, and one allocation area of growth between collections. The collector keeps its
default compaction threshold, 30% of `-M`, which switches to compaction before copying would
overflow and above the live budget.

Memory admission exists to keep the pod clear of an OOM kill and of collector thrash, and to admit
as much work as that allows. A busy copying collector keeps about four times its live data plus
the nursery. So the live data it holds without strain is a quarter of the heap the nursery leaves,
and it overflows the heap at half. That quarter, less the cache, the queue tenants and the idle
process, is the budget metadata requests pay into. A static estimate per request cannot hold that
line, because request cost spans about sixty times across real packages while the cost per byte
stays in a narrow band. So a request pays per byte as it reads, and pays for its response before it
builds it. A started read pauses rather than failing, which avoids wasting its upstream transfer and
inviting a retry storm. Only one request at a time may run past the budget, until it ends, and the
shared work it waits on runs with it, so the overshoot stays within about one request and a pause
never deadlocks.

A full read's charge per source byte is the larger of two figures. Both come from the read peaks
per source byte that the residency tier measures for one ecosystem, among listings with at least
one 1 MiB meter step of sources. A listing's read peak is the most live data it holds while its
reads parse and project, which is more than it keeps afterwards. The margin figure is 1.25 times
the highest peak among the realistic listings, rounded up to a tenth. Those listings are a single
document and the identical, overlapping, disjoint and publish-order merges, and the margin allows
for packages that differ from the corpus. The all-shape figure is the smallest tenth at or above
the highest peak among all the listings. Those add the heavy bases, merges whose private document
holds far more text than its version count suggests.

Canonical sidecars add retained read state. The [sidecar measurements](../testing.md#listing-peaks) give
PyPI a full-read charge of 3.5 and a read limit of 3.25 under the formulas above and below.
The per-source-byte charge rises 9.375% from 3.2, so the same admission budget admits less read work before pausing.
npm retains its charge of 1.1 and read limit of 1.0.

Earlier qualification of packed source `62b458ab54dbdff7a296458ed094be74621ec20b` against actual main
`efece1f1fca410dc1d214dcc76a99f106e2a0ff7` gave PyPI a full-read charge of 3.2 and read limit of 2.75.
The [qualified calibration](../testing.md#qualified-packed-integration) records that arithmetic and source context.
A charge revision requires fresh review, CI, native measurements and default npm and PyPI loads.
The following historical snapshots retain their original measurements and charge conclusions.

#### Packed-file qualification before parser integration

The table uses the qualified native arm64 Build job of
[CI run 38103656822](https://github.com/AlexaDeWit/Ecluse/actions/runs/38103656822/job/114364621043).
Its synthetic checkout `245113a792af38b0206d8bee11581b527ec447f0` has literal parents
`7f214208ea93a54584f0687bef6870d68f7ac8a8` (actual `main`) and
`bb3a29d7b47bf5af0cef82a47007c66244f19f86` (PR #1619's runtime source).
It does not measure a merged PR on `main`. The matching literal-main baseline is
[CI run 38102664684](https://github.com/AlexaDeWit/Ecluse/actions/runs/38102664684/job/114362617303).
The maxima include listings of at least one step. They display six decimal places, but the rules
use measured byte counts before rounding. The last row is the limit for a single document's read.

| Figure | npm | PyPI |
|---|---|---|
| Highest peak among the realistic listings | 0.953751 (express, publish order) | 2.628749 (boto3, single document) |
| Margin figure | 1.2 | 3.3 |
| Highest peak among all the listings | 1.071052 (express, oldest heavy base) | 2.628749 (boto3, single document) |
| All-shape figure | 1.1 | 2.7 |
| Full-read charge | 1.2 | 3.3 |
| Highest peak among single documents | 0.862843 (react) | 2.628749 (boto3) |
| Read limit | 1.0 | 3.0 |

A capture under one step can peak above the per-byte charge, up to 1.67 for npm (lodash) and 3.91
for PyPI (requests). What the meter holds for one such read, whole steps and at least the 1 MiB
entry step, covers every capture the tier measures. A request for a name that is not first-party
reads its private and public documents at once on one ticket, so two such reads can exceed what
the meter holds by a fraction of a step. The sampler's measurement of live data outside the
charges absorbs that excess.

The charges come from the committed corpus and its merges, so a document shaped or sized unlike
them can hold more than it pays for. Écluse treats such documents as rare. Two things absorb the
excess. The sampler measures it as live data outside the charges after the next major collection,
and the ceiling the budget may reach, set out below, falls as that remainder grows.

#### Native-parser qualification before packed-file integration

This snapshot belongs to the merged parser source identities it names.

The table gives heap bytes per source byte from the native arm64 Build job of
[CI run 38107103485](https://github.com/AlexaDeWit/Ecluse/actions/runs/38107103485/job/114374944192).
Its PR merge `3f63e1e7912aa0b118ea39044e32c368380f9a58` has the same tree as
`689b7a19f4c3c1840a115cc185582075293c050e`, including `main` at `7f214208`.
The maxima include listings of at least one step. They display six decimal places, but the rules
use measured byte counts before rounding. The last row is the limit for a single document's read.

| Figure | npm | PyPI |
|---|---|---|
| Highest peak among the realistic listings | 0.837052 (react, single document) | 3.112123 (boto3, single document) |
| Margin figure | 1.1 | 3.9 |
| Highest peak among all the listings | 0.958492 (express, oldest heavy base) | 3.112123 (boto3, single document) |
| All-shape figure | 1.0 | 3.2 |
| Full-read charge | 1.1 | 3.9 |
| Highest peak among single documents | 0.837052 (react) | 3.112123 (boto3) |
| Read limit | 1.0 | 3.5 |

Listings under one step peak at up to 1.05 bytes per source byte for npm (lodash, oldest heavy base)
and 3.55 for PyPI (requests, publish order). Whole meter steps cover every measured read, including
the initial 1 MiB entry step. A request for a name that is not first-party
reads its private and public documents at once on one ticket, so two such reads can exceed what
the meter holds by a fraction of a step. The sampler's measurement of live data outside the
charges absorbs that excess.

The charges come from the committed corpus and its merges, so a document shaped or sized unlike
them can hold more than it pays for. Écluse treats such documents as rare. Two things absorb the
excess. The sampler measures it as live data outside the charges after the next major collection,
and the ceiling the budget may reach, set out below, falls as that remainder grows.

One such shape is a document of many tiny entries, whose map nodes, keys and typed values outweigh
the source bytes they come from. An npm or PyPI full read therefore pays at least 256 bytes for each
entry it retains, through its workspace, after each piece it parses:

| Full read | Entries the floor charges | Of those, entries no count bounds |
|---|---|---|
| npm | Versions, timestamps and dist-tags | Timestamps and dist-tags |
| PyPI | Files, dropped version entries, and `alternate-locations` and `meta.tracks` items | Dropped version entries, and `alternate-locations` and `meta.tracks` items |

A PyPI read counts `alternate-locations` and `meta.tracks` items when their array closes, so the
floor reaches them from the next piece. Realistic listings carry enough bytes per entry that the
floor adds nothing. The floor is the condition for later dropping the version count from listing
reads, where it still caps the entries a dense document retains.

A listing's response pays per byte of its output basis. The basis is the larger of two estimates,
one anchored on the largest document the listing merges and one on the base document, whose
top-level fields the response keeps. Each adds the other document's bytes in proportion to the
versions the anchor does not hold. That share assumes the other document's versions are of like
size, and the base anchor covers a base that renders more than its version count suggests.
Documents that overlap, such as a package mirrored into a private upstream, render about one
document and pay for about one. The output working set is the larger of the listing's peak above
the documents it holds and twice the served body, for the encoding with its strict copy. The
response charge is 1.25 times the highest working set per basis byte that the residency tier
measures for one ecosystem, from one step of basis up, rounded up to a tenth. It takes the
realistic shapes: a single document, and merges whose documents are identical, overlap, share no
version, or put the newest quarter of the versions in the private copy, as a registry that holds
the versions a deployment consumed does. The measurements give npm's charge as 2.0 (@aws-sdk/client-s3
at 1.523900). PyPI's charge is 1.9 (NumPy at 1.476585 after retaining canonical sidecars). The tier also renders heavy bases that render
far more than their version count suggests, and from one step of basis up holds each of them
within the output charge. Below one step, the whole meter steps a response pays for hold every
listing's output working set, heavy bases included, and the tier checks that too.

A completed read keeps less than its peak, and nothing reaches its document once the body exists.
So when a full read completes, its charge steps down to the kept charge: 1.25 times the largest
shared cache entry per source byte that the residency tier measures, rounded up to a tenth. That is
0.9 for npm (react at 0.699300) and 3.3 for PyPI (requests at 2.601638). Once the body exists, the
reads' charges go back, and the response holds only its own charge while it is sent. The residency
tier checks each phase against what the meter holds then. Its heap census of the live listing path
also covers the policy and merge plan between the reads and the render.

The residency tier in [`docs/testing.md`](../testing.md#listing-peaks) fails when a listing's reads
or render outgrow what the meter holds for them, so a representation change cannot silently
outgrow a charge. From one step up, it fails once a single document's read peak or a realistic
listing's output working set passes its capture's regression limit. A read peak's limit is its
measured figure plus 4%, and an output working set's is its figure plus 3%, each rounded up to a
permille. Every limit sits below its charge, so the tier fails on growth well before a charge stops
covering it.

A charge above what a request holds costs throughput. After each major collection the sampler
measures the live data outside the charges, so the budget may grow until charges and that remainder
reach a third of the heap, less room for the largest recent request under the overflow point. An
excess charge takes budget that live data never fills, and the sampler cannot give it back. It
halves the budget, at most once a second, while the collector takes more than half the CPU. The
charges act the moment work starts and the measurement arrives later, so neither alone holds the
line.

Écluse keeps a structural count only where no byte, memory or deadline bound already covers the
work. Both count keys stay pinned policy: they bound document shape, not bytes, and do not scale
with RAM.

| Bound | Work it covers | Why no other bound covers it |
|---|---|---|
| `maxNestingDepth` | Recursion while decoding, and tar path depth and link chains | Source bytes do not bound depth or link chains |
| `maxVersionCount` | The versions the Dredger gathers for one package across store listing pages | That work has no deadline, and no byte bound across pages |
| `maxVersionCount` | The versions one mirror-state record holds | A pre-scan counts them before the record decodes |
| `maxVersionCount` | Source entry positions on every listing read, which refuses at the first entry past it | Per-byte charges are calibrated on realistic listings, so a dense npm or PyPI listing can retain more than its bytes pay for |
| Fixed ceilings in code | Tar and ZIP entries, gzip and bzip2 members, Marshal and YAML nodes, multipart parts, mirror-state files, and RubyGems directory gem names | Bytes and per-entry charges already bound this work, so it has no key |

Resolution remains role-agnostic across proxy, Pilot, and Dredger. The body bounds name their
operations separately:

| Bound | Consumers | Source |
|---|---|---|
| Metadata and control response | Serve origins, worker packuments, mirror presence probes, publication replies, Dredger reads and delete replies | Resolved metadata response cap |
| Client publish request | First-party publish body and its reservation | Resolved publish request cap |
| Buffered mirror artifact | Worker artifact download before verification | Mirror-artifact tenant cap |

These controls impose no metadata-serving minimum pod size on roles that never serve packuments. Measured body sizes count decompressed bytes before projection.
The Operator Manual carries the [per-pod arithmetic](https://ecluse-proxy.com/docs/operations/#appendix-runtime-sizing-arithmetic).

### Rule policy

The rule policy is a named map of rules layered over a built-in default that ships with the binary.
An entry whose name the default already defines is a **patch**: it overrides precedence, values, or
both. An entry with a new name must carry a full `type`, and it **adds** a rule. Any entry may set
`"enabled": false` to **suppress** a default rule. With no rule config, the default policy applies
unchanged.

This top-level policy applies to every mount. Each active mount resolves that shared policy,
then its shipped `mounts.<ecosystem>.rules`, then the operator's mount rules.
The mount decoder accepts the rules object, and resolution parses each layer separately.
This keeps suppression and type checks available for shipped mount rules.
The document and environment still merge into one operator layer, strongest-last.

```yaml
rules:
  min-age:
    ageSeconds: 1209600
mounts:
  npm:
    rules:
      deny-scripts:
        appliesTo: [admission, revocation]
```

Here `min-age` patches a shared default. The npm patch widens the shipped mount deny to revocation.
Each rule may set an integer `precedence`, where higher wins.
Omit it for the type's default.

Each rule may also set `appliesTo`, the list of phases it applies at. It merges as `precedence`
does: a new rule without it applies at admission and at revocation, a patch without it keeps the
phases of the rule it patches, and a stated list replaces them whole. A layer can therefore narrow
a deny that a lower layer defines, and a higher layer can widen it again for one mount.

The load refuses the setting at one of two points:

| Refused form | Where | What the load reports |
|---|---|---|
| A key written with no value, or a value that is not a list of strings, an empty string in its place included | When the document is decoded | That one fault alone, because the decode stops there |
| An empty list, a list without `admission`, an unknown word (an empty string inside the list is one), an allow limited to admission | When the rules resolve | Every such refusal in the same `rules` object together |

[Rules engine → Admission and revocation](rules-engine.md#admission-and-revocation) holds the reason
for each refusal.

[Rules engine → Evaluation model](rules-engine.md#evaluation-model) is the canonical home for the
precedence values, the single total order the rules resolve into, and the evaluation model. This
document owns only the document-merge schema above.

#### The default policy

The shared shipped default enables one rule, and two more when `advisories.url` is set. `min-age`
(`AllowIfOlderThan`, 7 days) admits a public version after the waiting period. This reduces
exposure to versions removed before they become eligible. Age is not a malware verdict.
With a source, `remediation-fast-track` (`AllowIfRemediatesCve`) ranks above the age rule. It can
admit an exact security fix early when the accepted advisory evidence supports that exception. It admits a partial fix
only when the advisories still affecting it store scores inside its limits, and it refuses while
advisory evidence is unavailable (see
[Rules engine](rules-engine.md#allowifremediatescve-remediation-fast-track)).
`deny-known-malicious` (`DenyIfKnownMalicious`) ranks at 300, above every default allow (see
[Rules engine](rules-engine.md#denyifknownmalicious-primary-namespace-evidence)). Without a source,
neither shared advisory rule is active. The age rule supplies the shared allow, and npm's
install-script and external-dependency denies and Cargo's external-dependency deny still apply.

The npm mount adds `deny-scripts` (`DenyInstallTimeExecution`) and `deny-external-dependencies`
(`DenyExternalDependencies`) at admission only. They do not activate a mount. The operator can
disable either under `mounts.npm.rules`, widen its phases, or add an `AllowByIdentity` pin with
explicit precedence above 300. The shared layer cannot disable either, because it defines neither.
The Cargo mount adds `deny-external-dependencies` the same way, under `mounts.cargo.rules`. PyPI
and RubyGems have no shipped mount rules.
Every remaining built-in rule is off and opts in by name. The scored advisory denies (`DenyIfCve`
and `DenyIfEpss`) in particular can deny historical versions an existing build depends on, if an
operator enables them before the mirror is warm. Read their
[onboarding steps](https://ecluse-proxy.com/docs/configuration/#onboarding-the-advisory-denies) first.

### Advisory database sync

The remediation fast lane and the advisory denies read a synced local advisory database, not an
API per request. The compilation, ETag polling, and atomic shadow-swap are under [Rules engine → CVE
subsystem](rules-engine.md#cve-subsystem). The operator knobs (the store URL, the poll interval, the
OSV export and EPSS feed sources, and the download size cap) are in the
[configuration reference](https://ecluse-proxy.com/docs/configuration/#the-configuration-reference).
With no store configured, the default advisory rules remain inactive. The age rule and any
ecosystem defaults still apply. With a store, a mount whose rules require advisory evidence stays
not-ready until its first artifact syncs.

### Validation: fail fast, reject the unknown

Écluse validates the whole config at startup and refuses to start on any problem, never running in
a degraded state. It aggregates the errors, so one run reports every issue. An unknown name is an
error, not a silent skip:

- Écluse rejects an unknown rule `type`, and an unknown field or key. An operator authors the
  config alongside the binary, and deny-by-default protects you only if the policy you wrote is the
  policy that loaded. A typo must fail the load rather than silently stop blocking. The same
  reasoning reaches inside a rule: a rule reads only its own type's parameters, so a threshold or
  an `onUnavailable` written under a type that does not read it is refused rather than ignored.
  Ignoring it would leave an operator believing they set a gate's failure direction.
- Malformed values (bad URL, non-integer precedence, unparseable JSON) fail the same way.
- Merge references must resolve. Écluse rejects a `rules` entry that neither names a known default
  nor supplies a complete new rule.
- Écluse rejects a mount incoherent with its derived mode: a mirrored mount with no
  `privateUpstream`. The error names the offending key, and one report covers every incomplete
  mount.
- An endpoint must carry exactly one tag and only the keys that tag admits there. The mirror-write
  credential falls out of that: the minting tag admits no static token and the two non-minting tags
  require one. Écluse still rejects a CodeArtifact identity that cannot mint an initial token, on
  the roles that hold one.
- A `codeArtifact` endpoint's URL must match its tag. Écluse rejects a host that is not a
  CodeArtifact endpoint on any endpoint, and on a mirror target or a private upstream it also
  rejects a path that is not the repository endpoint for the mount's own ecosystem
  (`/npm/{repository}/` for an npm mount) and an ecosystem CodeArtifact carries no package format
  for. Each refusal names the key path it was written at.
- On `mirrorTarget` and `publicationTarget`, `url` is where Écluse reads and `uploadUrl` is where
  it writes. Where the protocol derives its write address from `url` (npm, RubyGems), the load
  refuses `uploadUrl`, because the key could only send uploads to another address. Where the
  protocol uploads to a separate endpoint (PyPI, Cargo), the load requires `uploadUrl` on both
  targets in every role, because the write address cannot be derived from `url`.
- A static publish credential requires a verifiable edge. A `token` under a `publicationTarget`
  tag without `ECLUSE_SERVER__AUTH_TOKEN` is refused as
  `PublishStaticCredentialNeedsEdge`. That pairing would let any unauthenticated client publish
  under Écluse's own identity.
- Endpoints that hold different registry roles must not name one registry. Every role refuses a
  `publicationTarget` on any mount's `publicUpstream` host, a `publicationTarget` equal to another
  mount's `privateUpstream`, `mirrorTarget`, or `publicationTarget`, and a `mirrorTarget` on any
  mount's `publicUpstream` host. Each role also refuses a private upstream equal to its own mount's
  public repository: the private leg forwards caller credentials and bypasses the public rules.
  Distinct repositories on one host remain valid for this private/public comparison.
  `ecluse dredger` deletes from each mount's separately authorised mirror and private cache, so it
  also refuses a `mirrorTarget` equal to any mount's `privateUpstream` or to its own mount's
  `publicationTarget`. `ecluse proxy` and `ecluse mirror` boot on those mirror collisions and warn once per
  collapsed pair, and the operator prunes that mirror by hand. Dredger, including preview, also
  refuses a private upstream equal to its own publication target: its private read cache must
  remain separate from user publications. Proxy and mirror retain their existing behaviour for
  that pair and issue no advisory. One combinator turns each detected
  collision into the outcome the booting role earns, so a refusal on one path and a warning on
  another always come from the same rule. The comparison is by full registry URL, not by host,
  because repositories of one CodeArtifact domain differ only in path, and a repository's per-format
  endpoints are separate stores. It folds the authority to lower case and applies the default port,
  so neither a capital letter nor an explicit `:443` defeats a refusal. Applying the default port
  keeps the port in the key rather than dropping it, so `:8443` stays a separate store. The path is
  compared exactly, which is what keeps those per-format endpoints apart.
- Every mount's `mirrorTarget` and private cache need store maintenance backends. `ecluse dredger` reads the tag,
  so it refuses a target whose tag names a store this build carries no control plane for. The
  refusal names the mount key and the reason. Only the Dredger deletes, so only the Dredger refuses:
  the other roles boot on such a target and log nothing, and `ecluse check-config` names the
  Dredger's refusal.
- `dredger.chunkPause` carries a hardcoded floor of two seconds. The pause between chunks is what
  leaves an operator time to stop a mistaken sweep, and deletion is permanent, so the value may be
  raised and never lowered. `ecluse dredger` refuses a value beneath the floor and names the key,
  the configured value, and the floor. Only the Dredger reads the group, so the same severity split
  applies: the other roles boot on it, and the checker names the Dredger's refusal.

Most of those refusals are decided as the configuration loads. The mount-adapter rule, the
publish-policy pairing, the endpoint-disjointness rules, and the store maintenance backend are
decided after it, by one pure pass over the loaded configuration and the environment snapshot that
load read. The pass takes the booting role and accumulates, so one run reports every refusal and
every advisory that role earns, and the advisories reach the log even when a refusal stops the
boot. One decision sits outside it: a memory-plan override is judged against the resolved mirror
runtime, so a refused queue URL reports without it. The refusals `ecluse check-config` does not
reach are the ones a live environment settles. These are the steps that raise them:

- minting a CodeArtifact identity's first token
- building the mirror-queue backend
- preparing each mount ecosystem's advisory sync
- resolving a mount's mirror-write provider
- building the clients the Dredger sweeps each mirror target and its private cache with

The same validation runs without a boot. `ecluse check-config` runs the full resolution chain:
config load, runtime plan, sizing and memory-budget resolvers, mirror-queue selection, and the
ambient `AWS_ENDPOINT_URL` override. It prints every decision, one provenance line per resolved
key, secrets redacted, precedence environment > document > default. It exits `0` on a valid
configuration, and `2` with the same aggregated report a boot would log. Both entry points call
the one pure pass, so a role's verdict on one set of inputs is the same on either side of it. The
inputs are not the same value. A boot passes the runtime posture it measured after applying it,
and the checker passes the posture `appliedRuntimePlan` predicts an application would reach. Where
an application falls short of that prediction, the boot sizes the memory plan against the smaller
measured posture, so it can refuse an explicit override the checker cleared.

The checker picks no subcommand, so it runs the pass once per role. It runs no mirror pipeline and
prunes no store, so its own pass vets under the writing roles' severities, and that pass decides
the exit status. A refusal only some roles earn prints as a warning naming the command that earns
it: `ecluse proxy --no-worker` and `ecluse mirror` over the bounded in-memory queue, `ecluse mirror`
where no mount declares a `mirrorTarget`, and `ecluse dredger` on a collapsed endpoint pair, on a
`mirrorTarget` this build has no maintenance backend for, or on a chunk pause beneath its floor. A
configuration one role refuses and another boots is a normal deployment, which is why those do not
fail the check.

What the checker does not reach is the environment-dependent tier those refusals sit in. A boot
builds it and the checker makes no cloud call, which is also why allocating each mount's rule state
waits for a boot. Two types keep that boundary visible. The pure pass yields the boot plan, which is
the artefact the checker prints and the last one it can reach. A boot then runs an effectful
planning phase over that plan, which spends every remaining refusal and yields an executable plan.

Every role runs that phase, and each has its own arm in it. The three mirror-pipeline halves
settle the mount wiring, the advisory sync, and the queue backend there, and one run reports every
refusal all of that earns. `ecluse dredger` settles the credential its stores answer to, the
advisory sync its rules read, and one maintenance handle per cleared mirror store there, so a store
whose client cannot be built reports at the gate rather than on the first call against it. `ecluse
pilot` settles its export loop there, one compile cycle per mounted ecosystem, and refuses where an
advisory store is configured and no mount is. So an executable plan carries the role's own wiring,
and a boot spends its last refusal in one place whichever role it started.

Nothing downstream of an executable plan refuses to boot. Holding one means the assembly below it
only builds and allocates, so a role's runtime cannot reject a configuration the boot already
cleared. A listener that fails to bind and an upstream that stops answering are still possible, and
those are runtime faults for supervision rather than refusals.

## Client authentication

Inbound auth (client to proxy) is the edge half of the credential model. Écluse authenticates to
the upstreams per [Credential flow and authority](registry-model.md#credential-flow-and-authority),
and the client's credential never reaches the public upstream.

Cloud IAM cannot be the edge: registry clients speak bearer tokens and HTTP Basic, not SigV4,
mTLS, or OIDC.

Two edge modes ship. The **open** mode leaves `ECLUSE_SERVER__AUTH_TOKEN` unset and delegates
access to the network layer. The **static token** mode sets `ECLUSE_SERVER__AUTH_TOKEN`, and the
client presents it in whichever form its own ecosystem speaks: `Bearer <token>` or an `.npmrc`
`_authToken` from npm tooling, an HTTP Basic password under any username from Python tooling.
Écluse compares the secret half, so one token serves every mount.
