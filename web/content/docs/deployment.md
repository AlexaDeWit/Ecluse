+++
title = "Deploying Écluse"
description = "Which roles of the one container image to run, which stores to put behind them, and how to fence the edge and the network so builds cannot step around the gate."
weight = 3
+++

You reach this page when the quick start has proven the gate and you want a deployment your builds
can depend on. Most of the work is not in the container: it is in the stores behind it, the edge in
front of it, and the network around it.

## The image and its roles

Écluse ships as one reproducible container image, a multicall executable, so the container command
selects the role:

- **`ecluse proxy`** (default): the HTTP proxy on `ECLUSE_SERVER__PORT` (default `8080`) plus the
  mirror worker. It scales horizontally behind a load balancer.
- **`ecluse mirror`**: the mirror worker on its own, for a worker fleet you scale separately. See
  [Splitting the proxy from the mirror worker](#splitting-the-proxy-from-the-mirror-worker).
- **`ecluse pilot`**: the OSV advisory ingestion pipeline.
- **`ecluse dredger`**: the registry cleanup worker. Not yet implemented. The role starts and
  answers its health probes, and it prunes nothing.
- **`ecluse check-config`**: validates the shared configuration exactly as a boot would and prints
  the resolved posture without starting anything (exit `0` valid, `2` refused). Run it in CI or
  before a rollout.

All roles share one configuration. The proxy and the mirror worker scale. Run Pilot as a singleton,
because multiple instances race and duplicate API calls.

**Give the advisory stack a writable volume.** Once `advisories.url` is set, the proxy lands each
synced database under `advisories.dataDir` (default `/var/lib/ecluse/advisories`) and Pilot
compiles there. The image runs as uid `65532` and sets no working directory, so mount a volume at
that path on every role that reads or writes advisories, and let uid `65532` write it. In
Kubernetes an `emptyDir` is enough: the artifact re-syncs after a restart, and Écluse sweeps the
partial downloads an interrupted run left behind. Without the mount the first sync fails on
permissions and the pod never reports ready.

### Splitting the proxy from the mirror worker

By default one `ecluse proxy` process does both jobs: it serves clients and drains the mirror
queue. The two loads are unrelated. Request rate follows your builds, while queue depth follows how
many novel versions those builds pull, so a burst of new packages can make a proxy fleet sized for
traffic look busy for the wrong reason.

Split them when you want to size each on its own signal:

1. Point `ECLUSE_QUEUE__URL` at a durable queue. This is required, not advisory: the in-memory
   queue holds its jobs inside one process, so a split deployment would strand every one of them.
   Écluse refuses both split roles at boot without it and tells you which key to set.
2. Run the proxy fleet as `ecluse proxy --no-worker`. It still admits versions and still enqueues a
   mirror job for each one. It just does not drain the queue.
3. Run a second fleet as `ecluse mirror`. It boots the same configuration and the same rules, so a
   worker's re-evaluation of a job reaches the same verdict the proxy did. It serves no registry
   paths, only its health probes on `ECLUSE_SERVER__PORT`.
4. Scale the worker fleet on queue depth (KEDA's SQS scaler, or an Auto Scaling policy on
   `ApproximateNumberOfMessagesVisible`) and the proxy fleet on request rate.

Both fleets need the mirror-write credential and the advisory store, because the worker
re-evaluates policy before it publishes. Neither `ecluse mirror` nor `ecluse proxy --no-worker`
changes what gets mirrored, only which process does the work.

Health-check a worker pod on `GET /livez`. It reports the consume loop's last successful poll
beside the verdict, so you can alert on staleness as well as on the `503`.

A Pilot pod does not need to idle between syncs. `ecluse pilot compile --out DIR` runs one OSV
compilation and exits: it fetches an ecosystem's advisory export, writes
`<ecosystem>-osv-schema<N>.db` (e.g. `npm-osv-schema3.db`) into `DIR`, and exits non-zero on
failure. `--out DIR` is required. The rest are optional:

- `--ecosystem` selects the export (default `npm`).
- `--source URL` overrides the configured `advisories.osvExportBaseUrl`.
- `--epss-source URL` overrides the configured `advisories.epssFeedUrl`.
- `--upload` also publishes the artifact to the advisory store, a full sync cycle in one
  invocation. Without a configured store it aborts at once.

A corrupt or truncated export aborts the compile without publishing, so a running proxy keeps its
last-good database. Run the one-shot as a Kubernetes `CronJob` with `concurrencyPolicy: Forbid`,
which keeps it a singleton, and schedule it less often than the proxy polls. Give the pod
`s3:PutObject` through IRSA or workload identity rather than mounted keys.

Pin the image by digest and verify its provenance and SBOM attestations before you run it. The
recipe is in [Verifying the image](https://github.com/AlexaDeWit/Ecluse/blob/main/README.md#verifying-the-image).

## The recommended topology

{{ diagram(name="topology", alt="Clients and CI call the Écluse proxy, which reads the private upstream union of the publication and mirror stores, fetches gated content from the public registry, and queues admitted versions for the mirror worker to write to the mirror target.", caption="Only gated public content enters the union: the mirror write is the single path public packages take into the trusted stores, and no edge runs from the public registry into them.") }}

This is the posture the [threat model](@/docs/threat-model.md) treats as canonical. Aim for it
unless you have a specific reason to diverge.

1. **Run three registries, not one.** Give the three roles distinct backends: the publication
   target is a first-party store, the mirror target is a public-derived store, and
   `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` is a pull-through read endpoint that unions both.
   Separating provenance keeps the mirror auditable. One rule is hard: the aggregating endpoint
   unions **trusted** stores only, never a direct public upstream, because raw ungated packages
   would otherwise reach clients as trusted and bypass the gate. See
   [registry-level composition](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity.** The default forwards each caller's credential to the
   private upstream and publication target, with nothing to set: access then matches your registry
   IAM exactly, and Écluse holds no standing read credential. See
   [Credential flow and authority](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#credential-flow-and-authority).
3. **Mint the mirror-write token from the container role.** Point
   `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` at a CodeArtifact endpoint, and the worker mints a
   short-lived token under the task or instance role instead of carrying a static secret. Scope
   that role **write-only** to the mirror store, and keep
   `ECLUSE_MOUNTS__NPM__MIRROR_TOKEN_DURATION` short, because this is Écluse's only
   standing credential and it writes the trusted store. Scope the mirror queue the same way.
   Anyone who can write the queue can force a write to the trusted store, so grant only the serve
   role `SendMessage`, and only the worker
   `ReceiveMessage`/`DeleteMessage`/`ChangeMessageVisibility`. `ChangeMessageVisibility` is
   load-bearing, not optional: the worker uses it to hold a long publish and to back a
   **dead-lettered** poison message off so the message rides your redrive policy to the DLQ.
   Without the grant an over-cap artifact silently churns on the ordinary visibility cadence
   instead.
4. **Let the edge own access, and leave `ECLUSE_SERVER__AUTH_TOKEN` off.** Écluse is not your
   access boundary. Front it with a gateway, mesh, or IAP, and restrict reachability **both**
   north-south and east-west (pod-to-pod), because an ingress-only allow-list that leaves the pod
   reachable inside the cluster is a common vulnerability. See
   [Edge authentication](@/docs/deployment.md#edge-authentication-and-client-credentials).
5. **Fence egress, keep metadata reachable.** Default-deny outbound, then allow only your
   upstreams, the mirror target, the metadata endpoint, and the advisory store when
   `ECLUSE_ADVISORIES__URL` is set (the proxy needs `s3:GetObject` to sync it). Require IMDSv2
   with hop limit 1, and do not block the metadata endpoint, because Écluse needs it to mint
   credentials. See [Network egress](@/docs/deployment.md#network-egress).
6. **Make the proxy unbypassable.** Deny CI runners (and, where practical, workstations) outbound
   access to the public registries. See
   [Locking down CI egress](@/docs/deployment.md#locking-down-ci-egress).
7. **Verify what you run.** Pin the image by digest and verify its attestations
   ([Verifying the image](https://github.com/AlexaDeWit/Ecluse/blob/main/README.md#verifying-the-image)).

The reasoning behind each choice, and the residual risks it accepts, is in the
[threat model](@/docs/threat-model.md) and
[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#trust-assumptions--credential-posture).

## What a deviation costs

Écluse still runs if you diverge, but every deviation trades away a protection, and one of them is
**silent**: Écluse cannot detect it, so nothing warns you.

| Deviation | What you lose | Does anything warn you? |
|---|---|---|
| One store for two roles: `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` equal to the private upstream, or `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` onto either | Provenance separation and clean post-incident scoping. The perimeter holds, but first-party and public-derived packages share one store | Yes, for four pairs. The proxy logs a boot warning when the mirror target resolves to the same registry as the private upstream, the public upstream, or the publication target, and when the private upstream resolves to the same registry as the public upstream. A publication target equal to the private upstream is the documented publish arrangement, so it raises nothing |
| A private upstream that itself draws from public, say a CodeArtifact repo with the stock `npm-store` upstream to npmjs | The rules, integrity floor, and freshness quarantine, all nullified. Raw ungated packages reach clients through the trusted read path, behind the gate instead of through it | **No. Écluse cannot detect this one.** |
| An open edge: `ECLUSE_SERVER__AUTH_TOKEN` unset | Écluse's own authentication layer. Access control leans entirely on your network boundary | Nothing fires, but the posture is your own explicit setting |
| A static publish credential without an edge token | Nothing at runtime, because it never boots | Yes. The boot fails closed |
| A static mirror-write secret | The short-lived token minted from the container role | Nothing fires. The secret is visible in the configuration you wrote |

The silent row has one remedy: aggregate **trusted stores only** into the private upstream. The
[threat model](@/docs/threat-model.md) records both store-level deviations.

## Edge authentication and client credentials

Edge authentication to the proxy ships in two modes:

1. **Open**: `ECLUSE_SERVER__AUTH_TOKEN` unset. The network layer (VPC, service mesh) owns access
   control, so this is appropriate only on a closed network.
2. **Static token**: `ECLUSE_SERVER__AUTH_TOKEN` set. Clients send it as
   `Authorization: Bearer <token>`. For an npm-protocol client that is the `_authToken` line, keyed
   by the mount's host and path:

   ```ini
   # .npmrc
   registry=https://ecluse.example.internal/npm/
   //ecluse.example.internal/npm/:_authToken=${NPM_EDGE_TOKEN}
   ```

The edge token never becomes the upstream one. Reads run **passthrough**: Écluse forwards the
caller's own credential to the private upstream, which stays the authority on what that caller may
see. Before the anonymous public fetch it strips that credential, so a client token never leaves
for a public registry, and it never caches the private origin across callers, so one caller's read
can never answer another's.

A `publish` forwards the publisher's own token the same way. Opt into a static
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` and Écluse publishes as itself instead. That opt-in
needs `ECLUSE_SERVER__AUTH_TOKEN` in place, or the boot refuses
(`PublishStaticCredentialNeedsEdge`), because the pairing would let any unauthenticated client
publish under it. `ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW` limits which package names a client may
publish. It authorises _names_, not _callers_, so it is not authentication. The reasoning is in
[security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#a-static-publish-credential-is-fail-closed) and
[Publishing first-party packages](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#publishing-first-party-packages-the-publication-target).

### What a client configures

Your developers and your CI each need two lines in `.npmrc`, and the recipes below assume the open
edge that the [recommended topology](@/docs/deployment.md#the-recommended-topology) sets. One line
names the registry, and the other carries the caller's own private-registry credential, keyed to
the Écluse URL:

```ini
# .npmrc
registry=https://ecluse.example.internal/npm/
//ecluse.example.internal/npm/:_authToken=${NPM_TOKEN}
```

Installs travel that path under that token, and so does a publish once the mount has a publication
target. What someone reaches through Écluse is therefore what your registry IAM already grants
them. A caller holding no credential still installs public packages through the gate, and the
private set is what the token unlocks.

Key every token line to a URL. npm resolves a credential from the final target URL, and that keying
is what stops a token reaching a host it was not minted for, so never write a bare global `_auth`
or `_authToken`.

Write no `@scope:registry` lines either. A client that installs and publishes through Écluse needs
none, because the `registry=` line already covers every name. What a stray one costs is under
[Scope bindings defeat publish routing](@/docs/deployment.md#scope-bindings-defeat-publish-routing).

### Minting the client token

The token is whatever your private upstream issues, and with AWS CodeArtifact behind Écluse the
same command mints it on a laptop and in a job.

**On a laptop**, the developer mints against their own AWS identity and exports it for the
`.npmrc` line above:

```bash
export NPM_TOKEN="$(aws codeartifact get-authorization-token \
  --domain acme --domain-owner 123456789012 --region us-east-1 \
  --query authorizationToken --output text)"
```

A CodeArtifact token lasts 12 hours at the most, and a request may ask for anything from 900
seconds up to that ceiling, so the refresh is a daily event. Put the command in a shell function or
a login hook rather than asking each developer to remember it.

**Do not run `aws codeartifact login --tool npm` on a client that installs through Écluse.** It
rewrites the client's default `registry=` to the CodeArtifact endpoint, so every install then goes
straight to CodeArtifact and never reaches the gate. Its `--namespace` variant is the same problem
in narrower form: it writes an `@scope:registry` binding, which takes that one scope around Écluse
and leaves the rest coming through.

**In CI**, no static npm secret exists in the job's settings. The job assumes a cloud role through
your CI platform's OIDC federation, mints the token with the same call, and holds it only for the
run:

```bash
# after the role assumption your CI platform's OIDC integration performs
export NPM_TOKEN="$(aws codeartifact get-authorization-token \
  --domain acme --domain-owner 123456789012 --region us-east-1 \
  --query authorizationToken --output text)"
```

npm expands `${NPM_TOKEN}` in `.npmrc` from the environment, so one committed `.npmrc` serves the
laptop and the runner without a second file.

### Scope bindings defeat publish routing

An `@scope:registry` line does more than route that scope's installs. On `npm publish` it also
outranks `publishConfig.registry` in `package.json` **and** a `--registry` flag on the command,
because any scope-keyed source beats any plain-registry source. That holds on npm 10.9.8 and
11.17.0. So an inherited binding sends your publishes to the bound registry, and no publish-time
flag takes them back.

Own no bindings and the problem never arises. When you inherit one you cannot delete yet,
`npm publish --@yourscope:registry=https://ecluse.example.internal/npm/` overrides it for that one
invocation. Treat that as a repair rather than a design: it works in npm alone, and it rides a
surface npm does not document.

### Publishing without a publication target

A mount with no publication target answers `npm publish` with `405 Method Not Allowed`, so a team
that publishes has to reach its publication registry directly while its installs still run through
Écluse. Four rules keep that arrangement portable:

1. No `@scope:registry` bindings, per the section above.
2. `registry=` points at the Écluse mount, so every install goes through the gate.
3. Each publishable package carries `publishConfig.registry` naming where its publish goes.
4. The publish step passes no bare `--registry` flag. A bare flag outranks
   `publishConfig.registry` and redirects the publish. The `npm_config_registry` environment
   variable does not, so a job may set its default registry that way and `publishConfig.registry`
   still wins.

Two clients diverge from that:

- **bun** (1.3.13) ignores `publishConfig.registry`, and it offers no other way to aim a publish
  at a second registry. Publish those packages with npm.
- **Yarn Berry** ignores `.npmrc` altogether. Set `npmRegistryServer` and `npmPublishRegistry` in
  `.yarnrc.yml`, and key each token by its registry URL under `npmRegistries`.

## Network egress

Écluse fetches from the registries you point it at, and some URLs it follows (a version's
`dist.tarball`) come from upstream responses. Egress control therefore runs in two layers: Écluse
provides the first in the application, with an origin-aware trust model, and your platform provides
the second.

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
the block. The trusted private origin (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`) is deliberately
**not** subject to it, because a private registry legitimately lives on your internal network.

**The `dist.tarball` host gate.** Upstream chooses `dist.tarball`, so Écluse fetches a tarball only
from the same allowlisted host that served the listing, comparing host **and port** as a pair. It
upgrades a plaintext `dist.tarball` to https on its own host. A plaintext URL on any other host
drops the version from the listing. An https URL on a host the gate does not permit stays listed
and is refused with a `403` when a client asks for the artifact. No configuration widens that.

Provide the second layer at the platform, default-denying egress and allowing only your registries,
mirror target, and the metadata endpoint:

- **AWS**: security-group egress rules or network ACLs to the upstream and mirror CIDRs. Reach
  CodeArtifact and S3 over VPC endpoints. **Require IMDSv2 with hop limit 1**
  (`httpPutResponseHopLimit: 1`).
- **GCP**: VPC firewall egress rules and, where applicable, VPC Service Controls.
- **Kubernetes**: a default-deny `NetworkPolicy` with an explicit egress allowlist. Allow your
  private upstream's internal range.
- **Service mesh (Istio/Linkerd)**: sidecar outbound policy `REGISTRY_ONLY`, each upstream a
  `ServiceEntry`, constrained by a `Sidecar` egress listener and an egress `AuthorizationPolicy`.

Each role needs a different slice of that allowance, and only the proxy needs ingress at all:

| Role | Ingress | Egress allowlist | Credentials held |
|---|---|---|---|
| `ecluse proxy` | Client traffic, behind the edge you front it with | The upstreams, the mirror target, the metadata endpoint, and the advisory store when `ECLUSE_ADVISORIES__URL` is set | The mirror-write credential, plus the advisory-store read (`s3:GetObject`) when that store is set. Nothing more |
| `ecluse mirror` | None public (health probes only, for the orchestrator) | The public upstream, the mirror target, the mirror queue, the metadata endpoint, and the advisory store when `ECLUSE_ADVISORIES__URL` is set | The same as the proxy: the mirror-write credential and the advisory-store read |
| `ecluse pilot` | None public | The OSV export host in `ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL` (default `osv-vulnerabilities.storage.googleapis.com`), the EPSS feed host in `ECLUSE_ADVISORIES__EPSS_FEED_URL` (default `epss.empiricalsecurity.com`), the metadata endpoint, and your object store | `s3:PutObject` to upload the advisory database |
| `ecluse dredger` | None public (health probes only, for the orchestrator) | Not yet implemented. The role answers its probes and calls nothing | None |

**Do not block the metadata endpoint or internal ranges for the proxy itself.** Écluse reaches
metadata through the AWS SDK to mint its instance-role credentials, so denying it breaks those
credentials. IMDSv2 with hop limit 1 keeps the minting working while stopping a neighbour or
forwarded request from reaching metadata through extra hops. The trust assumptions behind the
credential split are in
[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#trust-assumptions--credential-posture).

Two Pilot details matter to the platform. It names the uploaded object
`<ecosystem>-osv-schema<N>.db` under whatever prefix `advisories.url` carries, a key stable per
ecosystem, so bucket policies and the proxy's ETag polling can target it. On an export-host
`5xx`/`408`/`429` it retries with capped, jittered backoff, so a transient outage cannot get your
NAT address rate-limited.

## Locking down CI egress

The controls above secure Écluse's own egress. This one secures your consumers'. If you control CI,
**deny runners outbound access to the public registries** (`registry.npmjs.org` and the equivalents
for other ecosystems), and let them reach only Écluse and your internal services. A misconfigured
job then fails instead of pulling an unvetted package, because a stray `--registry` flag, a
committed `.npmrc`, or a tool that ignores your settings cannot route around a network that only
reaches Écluse. That makes the policy _unbypassable_ rather than merely _default_
([MOTIVATION, The bar](https://github.com/AlexaDeWit/Ecluse/blob/main/MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around)). The same idea
extends to developer workstations, a softer control than CI.
