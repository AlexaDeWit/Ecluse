+++
title = "Deployment reference"
description = "Which roles of the one container image to run, which stores to put behind them, and how to fence the edge and the network so builds cannot step around the gate."
weight = 3
+++

Use this reference when configuring service roles, repository relationships, credentials, or
network access. For an ordered deployment path, start with [Set up Écluse](@/docs/setup.md).

## The image and its roles

Écluse ships as one reproducible container image. The image holds a multicall executable, so the
container command selects the role. Every role reads the same configuration, so you write it once
and run the roles your deployment needs.

The image is built for `linux/amd64` and `linux/arm64`.

| Command | What it does | How to run it |
|---|---|---|
| `ecluse proxy` (default) | Serves clients on `ECLUSE_SERVER__PORT` (default `8080`) and runs the mirror worker | Scale horizontally behind a load balancer |
| `ecluse proxy --no-worker` | Serves clients and enqueues mirror jobs, but does not drain the queue | Scale on request rate. Needs a durable queue |
| `ecluse mirror` | Runs the mirror worker alone, and serves only its health probes | Scale on queue depth. Needs a durable queue |
| `ecluse pilot` | Builds each ecosystem's advisory database from the OSV exports and the EPSS feed | One instance, because parallel instances race and duplicate API calls |
| `ecluse dredger` | Deletes versions your current rules deny at revocation from each mount's mirror target and private cache. No other role deletes | One per store, because it takes no lease |

The fast lane, the advisory denies, and Dredger read the advisory database that Pilot publishes.
[Splitting the proxy from the mirror worker](#splitting-the-proxy-from-the-mirror-worker) and
[Running Pilot](#running-pilot) cover those roles, and [Running the Dredger](@/docs/dredger.md)
covers Dredger.

Dredger reads and cleans both the mirror target and the configured private cache with independent
maintenance authority. `privateUpstream` supplies cache reads, `mirrorTarget` receives mirrors, and
`publicationTarget` receives user publications. Preview constructs observations only and requires no
deletion consent. Each deleting target needs its own backend-specific consent and credential.

Dredger refuses some endpoint pairs that the proxy and the mirror worker accept, because its
deletions could reach first-party packages that exist only in the publication target. It refuses a
mount whose `mirrorTarget` equals any mount's `privateUpstream` or its own `publicationTarget`, and a
mount whose `privateUpstream` equals its own `publicationTarget`. The proxy and the mirror worker
start on those pairs, and warn on the two mirror-target pairs. Dredger also refuses a cleanup target
it cannot sweep, on either side. [Running the Dredger](@/docs/dredger.md) lists those cases. The
[endpoint collision table](@/docs/configuration.md#endpoint-collisions) lists every pair and its
outcome per role.

### The advisory data volume

Once `advisories.url` is set, a role that syncs advisories stores each database under
`advisories.dataDir` (default `/var/lib/ecluse/advisories`), and Pilot compiles there. The image
runs as uid `65532` and sets no working directory. So mount a writable volume at that path on every
role that reads or writes advisories, and let uid `65532` write it.

In Kubernetes an `emptyDir` is enough. The artifact re-syncs after a restart, and Écluse sweeps the
partial downloads an interrupted run left behind. A missing volume fails at a different point per
role:

| Role | Without the volume |
|---|---|
| `ecluse proxy`, `ecluse proxy --no-worker`, `ecluse mirror` | The boot refuses, naming `ECLUSE_ADVISORIES__DATA_DIR` and the error it hit |
| `ecluse pilot` | Pilot creates the directory when it first compiles, so the fault appears at runtime |

### Validating before a rollout

`ecluse check-config` validates the shared configuration and prints the resolved posture without
starting anything. It exits `0` when the configuration is valid and `2` when it is refused. It
checks every role, so a refusal that only one command earns prints as a warning that names the
command. Examples are `ecluse mirror` without a durable queue or without any `mirrorTarget`, and
`ecluse dredger` on a collapsed endpoint pair. Run it in CI or before a rollout.

### Running without Pilot

Only the rules that read advisories need Pilot. `advisories.url` ships unset, so the advisory stack
is off until you point it at a bucket of your own. Without an advisory store:

- The shipped `deny-known-malicious` and `remediation-fast-track` rules stay inactive, so every
  public version waits out the quarantine.
- A mount whose rules refuse without advisory evidence refuses the boot. Those rules are
  `DenyIfKnownMalicious`, `DenyIfCve`, and `DenyIfEpss` under either `onUnavailable`, and
  `AllowIfRemediatesCve` under `onUnavailable: deny`. They cannot decide without a database, so
  every version they evaluate would refuse. `ecluse check-config` reports the same refusal.
- An `AllowIfRemediatesCve` rule under `onUnavailable: skip` boots with a warning. It admits no fix,
  and every admitted artifact logs its skipped check.
- Dredger's default sweep considers only the names a `DenyByIdentity` rule pins. A full walk still
  covers every name.
- Every other rule works unchanged, and readiness does not wait for advisories.

The shipped policy activates its advisory rules only with a source. So it runs without Pilot, on
the quarantine alone.

With a store configured, run Pilot before the roles that need a database. Readiness follows each
mount's own rules. A mount whose rules refuse without advisory evidence reports not ready until an
artifact syncs, and answers `/readyz` with `503` naming the missing database and Pilot as its
producer. The shipped policy is such a policy once a source activates it. A mount with no such rule
is ready before any artifact exists. The role keeps polling and
never exits. Once the boot retry budget is spent it logs an `ERROR` naming Pilot and the store, and
repeats that line every 15 minutes until an artifact loads.

If Pilot stops later, the last artifact keeps serving until it passes the maximum advisory age. The
advisory denies then refuse, whatever `onUnavailable` says, and the shipped fast lane refuses under
its `deny` ([Advisory push age](@/docs/operations.md#advisory-push-age)). An outage therefore keeps
an old public version from passing the quarantine too, unless a higher-ranked allow admits it.

The shipped fast lane compares EPSS scores, so Pilot needs the EPSS feed for every ecosystem that
runs it. A failed feed publishes no artifact for such an ecosystem, and its consumers keep the last
one they accepted. If the feed stays down past the maximum advisory age, the advisory rules refuse.

## The recommended topology

{{ diagram(name="registry-topology", alt="The proxy reads trusted endpoint C, which unions first-party A and mirror B. Public packages pass through the proxy admission gate, never directly into C.", caption="The recommended registry roles and their trust boundaries. A and B hold packages. C provides the private read view.") }}

The [threat model](@/docs/threat-model.md) follows each operation in its own view, including
[mirror writes](@/docs/threat-model.md#mirror-writes-and-completion),
[advisory distribution](@/docs/threat-model.md#advisory-distribution), and
[revocation](@/docs/threat-model.md#revocation-and-cleanup).

This is the posture the [threat model](@/docs/threat-model.md) treats as canonical. Aim for it
unless you have a specific reason to diverge.

1. **Run three registries, not one.** Give the three store roles distinct backends. The publication
   target is a first-party store, the mirror target is a public-derived store, and
   `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL` is a pull-through read endpoint that
   unions both. Separate provenance keeps the mirror auditable. One rule is hard: the aggregating
   endpoint unions **trusted** stores only, never a direct public upstream. Otherwise raw ungated
   packages reach clients as trusted and bypass the gate. See
   [registry-level composition](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity.** By default Écluse forwards each caller's credential to the
   private upstream and the publication target, with nothing to set. Access then matches your
   registry IAM exactly, and the proxy holds no read credential of its own. See
   [Credential flow and authority](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#credential-flow-and-authority).
3. **Give each role its own identity.** Declare the mirror target under the `codeArtifact` tag
   (`ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL`), and the worker mints a short-lived
   token from its role identity instead of carrying a static secret. Keep
   `ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__TOKEN_DURATION` short, because the mirror
   worker's identity is the only one that writes the trusted store. Scope the mirror queue the same
   way, because anyone who can write the queue can request a mirror write, subject to worker
   admission. [Role identities and least privilege](#role-identities-and-least-privilege) lists
   each role's grants.
4. **Let the edge own access, and leave `ECLUSE_SERVER__AUTH_TOKEN` off.** Écluse is not your
   access boundary. Front it with a gateway, mesh, or IAP. Restrict reachability **both**
   north-south and east-west (pod-to-pod), because an ingress-only allow-list that leaves the pod
   reachable inside the cluster is a common vulnerability. See
   [Edge authentication](#edge-authentication-and-client-credentials).
5. **Fence egress, keep metadata reachable.** Deny outbound traffic by default, then allow only
   your upstream metadata and artifact hosts, the mirror target, the queue, identity endpoints,
   and the advisory store when `ECLUSE_ADVISORIES__URL` is set. Require IMDSv2 with hop limit 1,
   and do not block the metadata endpoint, because Écluse needs it to mint credentials. See
   [Network egress](#network-egress).
6. **Make the proxy unbypassable.** Deny CI runners (and, where practical, workstations) outbound
   access to the public registries. See [Locking down CI egress](#locking-down-ci-egress).
7. **Verify what you run.** Pin the image by digest and verify its provenance and SBOM attestations
   before you run it
   ([Verifying the image](https://github.com/AlexaDeWit/Ecluse/blob/main/README.md#verifying-the-image)).

The reasoning behind each choice, and the residual risks it accepts, is in the
[threat model](@/docs/threat-model.md) and
[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#trust-assumptions--credential-posture).

## Role identities and least privilege

Écluse handles three kinds of credential, and each has a different lifetime:

- **Caller credential:** the token a client presents. Écluse forwards it to the private upstream
  and the publication target, and keeps it only for that request.
- **Role identity:** the workload identity a role runs under, such as an AWS IAM role through EKS
  Pod Identity, IRSA, or an ECS task role. The platform's credential chain issues short-lived tokens
  and renews them, so Écluse stores no secret. A CodeArtifact token that a role mints derives from
  its role identity.
- **Static secret:** a token you configure, which lasts until you replace it. Only a `registry` or
  `verdaccio` store and the optional static publication token need one.

Give every role its own identity, and grant each identity only its role's work. The Needs column
holds on any backend, and the AWS column shows one way to grant it:

| Role | Needs | AWS example |
|---|---|---|
| `ecluse proxy --no-worker` | Send mirror jobs, read advisory artifacts, read and list configured mirror state, inspect the private upstream's repository chain | `sqs:SendMessage`, `sqs:GetQueueAttributes`, `s3:GetObject`, `codeartifact:DescribeRepository` on the private upstream repository and every repository in its upstream chain |
| `ecluse mirror` | Consume mirror jobs, publish to the mirror store, read advisory artifacts, read/list/conditionally write configured mirror state | SQS receive, delete, change visibility, and get attributes. CodeArtifact token mint, reads, and publish on the mirror repository. `s3:GetObject` |
| `ecluse pilot` | Read the published artifacts' metadata, and publish advisory artifacts | `s3:GetObject` and `s3:PutObject` on the advisory prefix, and `s3:ListBucket` limited to that prefix. With SSE-KMS, `kms:GenerateDataKey` on the bucket's key |
| `ecluse dredger` | Read and delete in the mirror target and in the private cache, read advisory artifacts, inspect the private upstream's repository chain | CodeArtifact token mint, repository reads, and `codeartifact:DeletePackageVersions` on package ARNs in both repositories ([full list](@/docs/dredger.md#permissions)). `codeartifact:DescribeRepository` on both repositories and on every repository in the private upstream's chain. `s3:GetObject` |

`ecluse proxy` without `--no-worker` runs the mirror worker in the same process, so its identity
needs the proxy row and the mirror row together. Split the proxy from the worker when the serving
fleet must hold no write access to the mirror store. The mirror worker publishes public-derived copies. The proxy relays first-party publication
under the configured publisher credential. Within Écluse, Dredger holds deletion authority
for the maintained mirror and private-cache repositories.

Scope configured mirror state access separately from advisory storage. The proxy needs
`s3:GetObject` and prefix-scoped `s3:ListBucket`. The worker also needs `s3:PutObject` for
conditional state writes. Both use the same `mirrorState.url` bucket and prefix.

The mirror worker renews each message's visibility while it holds the message, so
`sqs:ChangeMessageVisibility` is required, not optional
([Mirror receipts and their visibility](@/docs/operations.md#mirror-receipts-and-their-visibility)).

No role identity reads a package from the private upstream or writes the publication target for a
client, because those requests carry the caller credential. A static publication token is the
exception, described under [Edge authentication](#edge-authentication-and-client-credentials).

Both proxy roles do read the private upstream's own configuration once at boot, under their role
identity, and so does `ecluse dredger`, in both its modes, through the store handle it already
holds. Écluse asks the backend whether that repository, or one in its upstream chain, connects to a
public registry, and refuses the role when it does: such a connection would let raw public packages
reach clients as trusted private content. An identity that cannot ask cannot clear the store, so a
refused `codeartifact:DescribeRepository`, and an environment that resolves no AWS identity at all,
refuse the role too. A serve-only proxy in front of a CodeArtifact private upstream therefore needs
that identity and that grant, even though its package reads carry the caller's own credential. The
mirror worker and Pilot read no private upstream and never ask. A `registry` or `verdaccio` private
upstream reports no such configuration, so the boot warns once and that topology stays yours to
verify. `ecluse check-config` makes no cloud call, so it prints that the check runs at boot rather
than running it.

## Splitting the proxy from the mirror worker

By default one `ecluse proxy` process serves clients and drains the mirror queue. The two loads are
unrelated. Request rate follows your builds, while queue depth follows how many novel versions those
builds pull. So a burst of new packages can make a proxy fleet sized for traffic look busy for the
wrong reason.

Split them when you want to size each fleet on its own signal:

1. Point `ECLUSE_QUEUE__URL` at a durable queue. The in-memory queue holds its jobs inside one
   process, so a split deployment would strand every one of them. Écluse refuses both split roles
   at boot without it and names the key to set.
2. Run the proxy fleet as `ecluse proxy --no-worker`. It still admits versions and enqueues a
   mirror job for each one, but it does not drain the queue.
3. Run a second fleet as `ecluse mirror` with the same intended configuration. The worker
   checks current metadata and advisory evidence before new writes, so its verdict can differ
   from the earlier request. It serves health probes on `ECLUSE_SERVER__PORT`, not registry paths.
4. Scale the worker fleet on queue depth (KEDA's SQS scaler, or an Auto Scaling policy on
   `ApproximateNumberOfMessagesVisible`) and the proxy fleet on request rate.

Both fleets read the advisory store, because both evaluate the rules. The split changes which
process does the mirror work, not what gets mirrored. Each fleet's grants are in
[Role identities and least privilege](#role-identities-and-least-privilege).

Health-check a worker pod on `GET /livez`. It reports the consume loop's last successful poll
beside the verdict, so you can alert on staleness as well as on the `503`
([Health probes](@/docs/operations.md#health-probes)).

## Running Pilot

`ecluse pilot` runs as a long-lived loop. A Pilot pod does not need to idle between runs, though:
`ecluse pilot compile --out DIR` runs one OSV compilation and exits. It fetches one ecosystem's
advisory export and the EPSS feed, and writes `<ecosystem>-osv-schema5.db` into `DIR`.

| Flag | Effect |
|---|---|
| `--out DIR` | Required. The directory the artifact is written into |
| `--ecosystem ECOSYSTEM` | The export to compile. Default `npm` |
| `--upload` | Also publishes the artifact to the advisory store under the [publication guard](@/docs/operations.md#advisory-publication-guard). Aborts before compiling when no store is configured |
| `--rebaseline-etag ETAG` with `--rebaseline-max-shrink-percent PERCENT` | Only beside `--upload`. Replaces only the stored object with that ETag, holds the shrink to that percentage, and restarts the size baseline ([Rebaselining](@/docs/operations.md#rebaselining)) |

Both feeds come from the configuration keys the scheduled loop reads, `advisories.osvExportBaseUrl`
and `advisories.epssFeedUrl`. To point one run at a mirror or a moved feed, set
`ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL` or `ECLUSE_ADVISORIES__EPSS_FEED_URL` in its environment.
Neither URL may carry a credential ([Secrets](@/docs/configuration.md#secrets)).

Pilot uses `HTTP_PROXY` and `HTTPS_PROXY` for OSV and EPSS downloads.
A `socks5://` or `socks5h://` URL in `HTTPS_PROXY` or `https_proxy` makes HTTPS feed downloads connect directly.

The loaded configuration decides what a failed EPSS feed means, as it does for the scheduled loop
([When the EPSS feed fails](@/docs/configuration.md#when-the-epss-feed-fails)). An ecosystem the
configuration does not mount counts as one with an EPSS-reading rule, because a compile run ahead of
a new mount should hold the full dataset. Pilot logs a warning naming that ecosystem. A compile with
no `advisories.url` counts the same way, because no source activates the shipped remediation rule
there, so the compile cannot tell whether its artifact's consumer reads EPSS.

| Outcome | Exit status |
|---|---|
| The artifact was written, and uploaded if asked | `0` |
| The EPSS feed failed where no rule of the ecosystem reads EPSS scores, and the artifact was written with `epss_status=unavailable` | `0`, with a `warn` line |
| The EPSS feed failed where a rule of the ecosystem reads EPSS scores, the ecosystem is not mounted, or no `advisories.url` is set | `1`. The previous artifact stays in place |
| The OSV export failed or was refused | `1`. The previous artifact stays in place |
| The publication guard refused the upload, or the store could not serve it | `1`. The previous artifact stays in place, in the store and in `DIR` |

Run the one-shot as a Kubernetes `CronJob` with `concurrencyPolicy: Forbid`, which keeps it a
single instance, and schedule it less often than the proxy polls. Give the pod its role identity
through IRSA or workload identity rather than mounted keys.

**Artifact keys.** Pilot names each artifact `<ecosystem>-osv-schema5.db`, where `5` is the schema
epoch, and uploads it under whatever prefix `advisories.url` carries. The key is stable per
ecosystem, so bucket policies and the consumers' ETag polling can target it. An IAM policy that
names exact object keys must name these keys. The artifact stores canonical package names (PEP 503
names for PyPI) and canonical version bounds and points. A consumer rejects an artifact from any other epoch and waits for its first
compatible sync. Nothing rewrites an old artifact.

**Epoch 5 rollout.** Run the new Pilot to rebuild and publish `npm-osv-schema5.db` and each other
enabled ecosystem's artifact before rolling out consumers. Epoch 4 files are incompatible even
when their tables have the same columns. New consumers select only epoch 5 keys and never migrate
or alias an old artifact. Old consumers continue selecting epoch 4 during a mixed rollout. If
an epoch 5 artifact is absent, the existing first-sync, readiness and alarm behaviour applies.

An enabled `pypi` mount reads `pypi-osv-schema5.db` under the `advisories.url` prefix.
The npm artifact cannot supply PyPI advisory decisions. Publish the PyPI artifact with
`ecluse pilot compile --ecosystem pypi --out DIR --upload` before you enable PyPI advisory denies.
See [per-mount readiness](@/docs/operations.md#health-probes) for routing and availability signals.

**Failed and empty compilations.** A corrupt or truncated export aborts the compile without
publishing, so a running consumer keeps its last good database. Pilot also refuses a compilation in
two cases:

- It dropped at least 16 advisories as oversized or malformed, and those make up at least 10% of
  the advisories it read.
- It produced zero relevant advisory rows for the requested ecosystem.

A refusal logs `ERROR`, publishes nothing, and keeps any previous artifact and its timestamps, so a
first refused compilation publishes nothing at all. Neither guard detects a source that shrank but
stayed well formed, and a genuinely empty feed needs deliberate operator handling.

**Withdrawn advisories.** Pilot excludes withdrawn OSV records from the artifacts it builds. An
artifact keeps the records it was built with until a successful compile and sync replace it. When
no relevant active rows remain, the zero-output refusal keeps the prior artifact. A withdrawal does
not cancel other active advisories or an operator's identity denies.

**Source provenance.** Pilot fetches each complete source URL and records provenance in the
artifact. [Logs](@/docs/operations.md#logs) names the source fields and the sync log line that
reads them.

## What a deviation costs

Some deviations warn, and others refuse startup. The
[endpoint collision table](@/docs/configuration.md#endpoint-collisions) gives each outcome. The
private-upstream check depends on the backend. Proxy boot and both Dredger modes reject detected
public aggregation. CodeArtifact checks its upstream chain. An undecidable result warns once.
Mirror-only and Pilot roles do not run this private-read check.

| Deviation | What you lose | Warning |
|---|---|---|
| One store for two roles | Provenance separation and per-store governance can be lost | Startup warns or refuses, depending on the pair |
| A private upstream that draws directly from public | Public rules, the quarantine, and the public-admission integrity floor | Boot refuses detected public aggregation. Undecidable evidence warns once |
| A closed edge: `ECLUSE_SERVER__AUTH_TOKEN` set | Per-caller passthrough, and per-caller publication with a static publication token | None. The posture is your own explicit setting |
| A static publication token without an edge token | Nothing at runtime, because it never boots | The boot fails closed |
| A static mirror-write secret | The short-lived token minted from the role identity | None. The secret is visible in the configuration you wrote |

With a private upstream that draws from public, the trusted listing floor still applies, but
conventional private npm artifact hits bypass metadata admission. You must aggregate
**trusted stores only** into the private upstream, including when the backend cannot prove its wiring.
With a closed edge, every caller
presents the one edge token and reaches the private upstream as one identity. The
[threat model](@/docs/threat-model.md) records both store-level deviations.

## Edge authentication and client credentials

Edge authentication to the proxy has two modes:

1. **Open**: `ECLUSE_SERVER__AUTH_TOKEN` unset. The network layer (VPC, service mesh) owns access
   control, so this mode is appropriate only on a closed network.
2. **Static token**: `ECLUSE_SERVER__AUTH_TOKEN` set. Every caller presents the same edge token,
   so private reads lose per-caller passthrough. No warning fires for this deliberate setting.
   Clients send the edge token in the form their own ecosystem speaks, and Écluse compares the
   secret half. An npm-protocol client sends `Authorization: Bearer <token>`, which is the
   `_authToken` line keyed by the mount's host and path:

   ```ini
   # .npmrc
   registry=https://ecluse.example.internal/npm/
   //ecluse.example.internal/npm/:_authToken=${NPM_EDGE_TOKEN}
   ```

   A Python client sends the same token as an HTTP Basic **password**, under any username it
   likes, which is how `pip`, `uv`, and `twine` present a credential:

   For pip, URL-encode the token and supply that value as `PYPI_EDGE_TOKEN_URLENCODED`.
   Set the index in the process environment:

   ```bash
   export PIP_INDEX_URL="https://__token__:${PYPI_EDGE_TOKEN_URLENCODED}@ecluse.example.internal/pypi/simple/"
   ```

   Do not put a literal environment placeholder in `pip.conf`. Pip does not expand it.
   Other clients use their own supported credential configuration.

The proxy holds no read credential of its own. Reads run **passthrough**: Écluse forwards the
caller's own credential to the private upstream, which stays the authority on what that caller may
see. With `ECLUSE_SERVER__AUTH_TOKEN` set, the value a client presents both satisfies the edge gate
and travels upstream. So a deployment cannot combine the static-token recipe above with the
passthrough recipes below. Écluse strips the credential before the anonymous public fetch, so a
client token never leaves for a public registry. It never caches the private origin across callers,
so one caller's read never answers another's.

A `publish` forwards the publisher's own token the same way. You can instead set a static
publication token under the publication target's tag
(`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__CODE_ARTIFACT__TOKEN`). Écluse then authenticates the
edge token and replaces it with the static publication token on every publish, so the publication
target never receives the edge token. Every holder of the edge token can then send any upload the
relay forwards, within the configured first-party scopes and what the static token permits, so
this is not the preferred deployment. The relay forwards an npm document with exactly one
version filed under its own `version` field, a non-empty `_attachments` object, every tag on that
version, no repeated key, and no installable dependency on a git source or a remote tarball URL. It forwards a PyPI upload with one `:action=file_upload` part and a
`content` file name that names the declared project and version. The
boot refuses a static publication token without `ECLUSE_SERVER__AUTH_TOKEN`
(`PublishStaticCredentialNeedsEdge`), because an open edge would let any client publish under it.
Without a static publication token, a closed-edge publish forwards the edge token unchanged.

`ECLUSE_MOUNTS__NPM__FIRST_PARTY` names the scopes you own, and only a name under one of them may be
published ([First-party namespaces](@/docs/configuration.md#first-party-namespaces)). The reasoning
is in
[security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#a-static-publish-credential-is-fail-closed)
and
[Publishing first-party packages](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#publishing-first-party-packages-the-publication-target).

### What a client configures

Écluse serves one endpoint per mount, and installs and publishes both go to it. With the recommended
open edge, the client supplies a credential the private registry accepts, keyed to the proxy's URL
in whatever per-registry auth configuration that client keeps. Écluse forwards it to the private
upstream, which authorises the caller. So what someone reaches through the proxy is what your
registry already grants them. Public installs without credentials still work unless a private
upstream explicitly refuses access. The private set is what the credential unlocks.

Public success does not prove the private credential worked. For npm, PyPI and RubyGems, an explicit
private `401` or `403` stops metadata and artifact reads with a local `403`, including HEAD and
conditional requests. Cargo keeps its native refusal statuses. In every ecosystem, public content
cannot replace the private copy after that refusal. A genuine private `404` still permits public
fallback for a name outside your first-party namespaces, and first-party names stay private-only. A backend that masks a refusal as `404` does not prove authentication
succeeded. The [private access policy](@/docs/configuration.md#first-party-namespaces) gives each
ecosystem's statuses.

Two rules hold whatever ecosystem the client speaks:

- **Key the credential to the proxy's URL.** A client resolves a credential from the URL it is
  about to call, so a URL-keyed entry stays with the proxy. An unkeyed global credential travels
  to whichever host the client reaches next.
- **Bind no name to a registry outside Écluse.** A client pointed at Écluse needs no per-scope or
  per-package override that points elsewhere, because its Écluse endpoints already cover every
  name. An override takes its names around the gate, and in some clients it decides where a publish
  goes as well ([Keeping publishes on the proxy](#keeping-publishes-on-the-proxy)).

For an npm-protocol client those two rules are a default registry line and a URL-keyed token line.
The recipes here assume the open edge that the
[recommended topology](#the-recommended-topology) sets:

```ini
# .npmrc
registry=https://ecluse.example.internal/npm/
//ecluse.example.internal/npm/:_authToken=${NPM_TOKEN}
```

### Where the client credential comes from

The credential belongs to the private registry, so issuing it is that registry's business rather
than Écluse's. A long-lived credential goes into the client's auth configuration once. Prefer a
short-lived one minted from an identity the caller already holds: a developer mints against their
own cloud identity, and a CI job mints against a role it assumes through its platform's OIDC
federation. Then no static registry secret sits in the job's settings.

Both write the same URL-keyed line, and only the minting command differs by registry. AWS
CodeArtifact is one example of the pattern:

```bash
export NPM_TOKEN="$(aws codeartifact get-authorization-token \
  --domain acme --domain-owner 123456789012 --region us-east-1 \
  --query authorizationToken --output text)"
```

Google Artifact Registry and other backends fit the same shape with their own command. Whatever
issues it, a minted credential expires, so put the refresh in a shell hook or a job step rather than
in a developer's memory.

**Mint the credential and write the auth line yourself.** A registry vendor's login helper rewrites
the client's default registry to point at that vendor, and some also write per-name bindings.
Either one routes traffic around the proxy, which is what the two rules above exist to prevent.

### Keeping publishes on the proxy

Écluse accepts a publish on the same mount endpoint it serves installs from. With the recommended open edge, it relays the publish to the publication target
under the publisher's own credential. Two failures can take a publish off that path, and they are
not alike.

A publish that reaches Écluse when the mount declares no publication target gets
`405 Method Not Allowed`. The failure is loud and immediate. Écluse relays a publish only to a
destination you declared, so a misconfigured client cannot mis-publish through the proxy.

A publish that never reaches Écluse is the quiet one. A client that resolves its publish
destination from some other part of its configuration sends it straight to that registry, and the
proxy sees nothing to refuse. Name-to-registry bindings are the usual cause, because in some
clients such a binding outranks the per-package publish setting as well as the install route. Keep
bindings to registries outside Écluse off a client pointed at Écluse, and let its Écluse endpoints
carry both directions. Cargo's own hint after a refused publish, `--registry crates-io`, is one such
route. [Connect Cargo](@/docs/connect.md#cargo) closes it with `publish = ["ecluse"]`.

## Metadata compression

Écluse compresses the metadata it serves itself, so your clients get compressed listings whatever
sits in front of the proxy. You need no front proxy feature for it: an AWS ALB does not compress
responses, and Istio compresses only through a resource your cluster may not allow.

| Response | Compressed by Écluse |
|---|---|
| An admitted npm packument, PyPI Simple index, Cargo sparse index, or RubyGems Compact Index `/versions` or `/info` reply or compact specification, when `Accept-Encoding` prefers gzip | Yes, gzip at level 1 |
| Artifacts, Core Metadata sidecars, probes, `/-/ping`, refusals, publish replies | No |

With compression on, a listing and its `304` carry `Vary: Accept-Encoding`. A gzip listing carries
`Content-Encoding: gzip`, its exact `Content-Length`, and a weak `ETag` distinct from the identity
one. A request that refuses both gzip and identity coding gets `406`.

Each gzip response costs CPU, on a cache hit as on a miss, because the metadata cache holds
identity bytes. A `HEAD` compresses too, to state the length, and a `304` compresses nothing. The
response also pays the memory budget for the compression, as
[Memory admission](@/docs/operations.md#memory-admission) lists.

Set `ECLUSE_SERVER__METADATA_GZIP=false` to turn it off, for example when a front proxy already
compresses or you would rather spend the CPU elsewhere. Écluse then serves every listing in
identity coding, ignores `Accept-Encoding`, and sends no `Vary`. The setting governs Écluse alone:
a front proxy may still compress what Écluse sends.

Compression makes a listing's length depend on its content, and a listing can merge your private
metadata with public metadata anyone can publish. The
[threat model](@/docs/threat-model.md) states what an attacker would need to learn from those
lengths. Turning compression off removes that surface from Écluse's own responses.

## Network egress

Écluse fetches from the registries you point it at, and some URLs it follows (a version's
`dist.tarball`) come from upstream responses. So egress control runs in two layers: Écluse provides
the first in the application, with an origin-aware trust model, and your platform provides the
second.

**Untrusted origins** are the public upstream and every `dist.tarball`. Three application controls
gate them:

- A host+port **allowlist**. An upstream URL with no explicit port authorises port 443 alone, so
  write a nonstandard port out (`https://repo.internal:8443`) to authorise exactly that
  `host:port`. A non-HTTPS upstream, or a port outside `1..65535`, fails closed at boot.
- **HTTPS-only fetching with TLS certificate validation.** Certificate validation is the guarantor
  against the resolve-to-internal and DNS-rebinding SSRF class, because no address a name steers to
  can present a CA-trusted certificate for the host.
- **Response-size limits** on the metadata bodies Écluse decodes and on the artifact bytes the
  mirror worker ingests before it publishes them. The client-facing tarball relay streams rather
  than buffers, so no byte ceiling applies to it.

A **literal internal-range block** adds defence in depth: loopback, link-local including the
`169.254.169.254` metadata endpoint, RFC1918, CGNAT, and IPv6 ULA. Écluse refuses a `dist.tarball`
whose host is an internal-address literal, and `ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES` extends
the block. The trusted private origin (`mounts.npm.privateUpstream`) is **not** subject to it,
because a private registry legitimately lives on your internal network.

**The artifact host gate.** Upstream chooses where an artifact lives. So Écluse fetches one only
from the same allowlisted host that served the listing, comparing host **and port** as a pair, or
from a host the ecosystem serves artifact bytes from by design. It upgrades a plaintext artifact URL
to https on its own host. A file the gate refuses, for its scheme or for its host, is **dropped from
the listing** rather than listed and refused at download, so the listing and the download gate agree
file by file. A release disappears when no file of it survives. No configuration widens that.

**Écluse identifies itself on every registry and mirror-target request and Pilot feed download.**
The `User-Agent` is `ecluse/<version> (+https://ecluse-proxy.com)`, naming the running build and
linking to the project. An upstream, a WAF, or a forward proxy that filters on the agent has to
allow it.

Set `egress.operatorContact` to a monitored email address or contact URL so an upstream operator
can reach your team before blocking your deployment. Écluse appends `; contact: <value>` inside
the comment and sends it to public and private upstreams, mirror and publication targets,
maintenance registry destinations, and Pilot's OSV and EPSS sources. The contact is unset by
default. It accepts 1-256 printable ASCII characters, excluding parentheses and backslash.
Invalid configured values, including an empty value, refuse startup and name the key.
Caller-supplied identities and AWS SDK identities stay intact.

Provide the second layer at the platform. Deny egress by default and allow only the role-specific
destinations below. Permit DNS through your resolver, and telemetry to the configured collector when
enabled:

| Platform | Egress control |
|---|---|
| AWS | Security-group egress rules or network ACLs to the upstream and mirror CIDRs. Reach CodeArtifact and S3 over VPC endpoints |
| GCP | VPC firewall egress rules and, where applicable, VPC Service Controls |
| Kubernetes | A default-deny `NetworkPolicy` with an explicit egress allowlist that includes your private upstream's internal range |
| Service mesh (Istio, Linkerd) | Sidecar outbound policy `REGISTRY_ONLY`, a `ServiceEntry` per upstream, a `Sidecar` egress listener, and an egress `AuthorizationPolicy` |

Only the proxy serves package-client traffic. Other roles expose health probes, and Prometheus adds
a separate metrics listener only when that exporter is selected.

| Role | Registry or feed egress | Configured queue and advisory egress |
|---|---|---|
| `ecluse proxy` with embedded worker | Public/private discovery, metadata and artifact hosts, mirror repository, optional publication target | Queue send/receive/ack/visibility and redrive probe, advisory S3 read, PyPI/Cargo/RubyGems state read/list/conditional write when configured |
| `ecluse proxy --no-worker` | Public/private discovery, metadata and artifact hosts, optional publication target | Queue send and redrive probe, advisory S3 read, PyPI/Cargo/RubyGems state read/list when configured |
| `ecluse mirror` | Public discovery, metadata/artifact hosts and mirror repository | Queue receive/ack/visibility and redrive probe, advisory S3 read, PyPI/Cargo/RubyGems state read/list/conditional write when configured |
| `ecluse pilot` | OSV export host and EPSS feed host | Advisory S3 upload, no mirror queue |
| `ecluse dredger` | Mirror target and private cache metadata, and each one's maintenance API | Advisory S3 read, no mirror queue |

Each role's permissions are in [Role identities and least privilege](#role-identities-and-least-privilege).
Omit destinations for features a role does not have configured. An in-memory queue needs no queue
network access.

The default public endpoints are `registry.npmjs.org` for npm metadata and artifacts, `pypi.org` for
PyPI metadata, `files.pythonhosted.org` for PyPI distributions, `index.crates.io` for Cargo discovery and indexes,
`static.crates.io` for Cargo archives, and `rubygems.org` for RubyGems metadata and artifacts. Private artifact hosts depend on
the selected backend. Pilot uses `osv-vulnerabilities.storage.googleapis.com` and
`epss.empiricalsecurity.com` by default. It attempts the EPSS feed on every compile, even when no
EPSS rule is enabled, so allow that egress whatever your rules. `ecluse check-config` prints the
feed's `host:port` on a line that starts `pilot: every compile attempts the EPSS feed at`. Pilot
retries with capped, jittered backoff on a `5xx`, `408`, or `429` from either host, and on a download
the host resets or cuts short partway through. A transient outage then does not get your NAT address
rate-limited.

Allow CodeArtifact API access for token minting on every role that mints: `ecluse proxy`,
`ecluse mirror`, and Dredger. `ecluse proxy --no-worker` mints no mirror-write token. Dredger also
uses the CodeArtifact maintenance API.

**Permit the identity endpoints your deployment uses.** The AWS credential chain can need IMDS,
an ECS credential endpoint, or STS for an assumed role. On EC2, require IMDSv2 with hop limit 1
(`httpPutResponseHopLimit: 1`). Allow configured private registry destinations without opening
every internal range. The trust assumptions behind the credential split are in
[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#trust-assumptions--credential-posture).

### Trusted certificate authorities

Écluse validates each upstream's certificate against the CA bundle that `SSL_CERT_FILE` names. The
image sets it to a bundle of the roots Mozilla trusts for TLS servers, also linked at
`/etc/ssl/certs/ca-bundle.crt`. The bundle leaves out four roots, because the private keys of eight
eMudhra intermediate CAs are compromised:

| Root | Why the image leaves it out |
|---|---|
| AAA Certificate Services (NSS name: Comodo AAA Services root) | It issued cross-signed copies of the compromised intermediate CAs |
| emSign ECC Root CA - G3 | It cross-signs emSign Root TLS CA - G3, which issued the compromised TLS intermediate CAs |
| emSign Root CA - C1 | Mozilla removed it from NSS 3.131.1 together with emSign ECC Root CA - G3 |
| emSign ECC Root CA - C3 | Mozilla removed it from NSS 3.131.1 together with emSign ECC Root CA - G3 |

A private upstream, such as an Artifactory or Nexus host, fails with an unknown CA error when its
certificate chains to a root that Mozilla does not trust for TLS servers. Give such a host a bundle
of your own through `SSL_CERT_FILE`, as for an internal CA.

To reach a private registry on an internal CA, mount a bundle that holds your CA beside the public
roots, and point `SSL_CERT_FILE` at it. Écluse then trusts every certificate in that file, and
nothing else, so build it on these terms:

- Start from the image's own bundle, or from another bundle that holds only roots trusted for TLS
  servers.
- Leave out the four roots above. Écluse trusts a certificate from a compromised CA for as long as
  your bundle holds a root it chains to, as [its limits](@/docs/security.md#limits) state.
- Use plain `-----BEGIN CERTIFICATE-----` blocks. Écluse ignores the trust rules in a
  `-----BEGIN TRUSTED CERTIFICATE-----` block, so it trusts that root for servers even where the
  rules reject it.

## Locking down CI egress

The controls above secure Écluse's own egress. This one secures your consumers'. If you control CI,
**deny runners outbound access to the public registries** (`registry.npmjs.org` and the equivalents
for other ecosystems), and let them reach only Écluse and your internal services. A misconfigured
job then fails instead of pulling an unvetted package. A stray `--registry` flag, a committed
`.npmrc`, or a tool that ignores your settings cannot route around a network that only reaches
Écluse. That makes the policy _unbypassable_ rather than merely _default_
([MOTIVATION, The bar](https://github.com/AlexaDeWit/Ecluse/blob/main/MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around)).
The same idea extends to developer workstations, as a softer control than CI.
