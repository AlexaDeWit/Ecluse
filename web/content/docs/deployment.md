+++
title = "Deploying Écluse"
description = "The container image and its roles, the registry topology to aim for, edge authentication, and network egress."
weight = 3
+++

This page covers the container image, the deployment topology to aim for, and the network
controls around it.

## The image and its roles

Écluse ships as one reproducible container image, a multicall executable selected by the container
command:

- **`ecluse proxy`** (default): the HTTP proxy on `ECLUSE_SERVER__PORT` (default `8080`) plus the
  mirror worker. It scales horizontally behind a load balancer.
- **`ecluse pilot`**: the OSV advisory ingestion pipeline. Run one instance.
- **`ecluse dredger`**: the registry cleanup worker. Run one instance.
- **`ecluse check-config`**: validates the shared configuration exactly as a boot would and prints
  the resolved posture without starting anything (exit `0` valid, `2` refused). Run it in CI or
  before a rollout.

All roles share one configuration. Multiple Pilot or Dredger instances race, duplicate API calls,
and overlap registry deletions.

`ecluse pilot compile --out DIR` runs one OSV compilation and exits. It fetches an ecosystem's
advisory export (`--ecosystem`, default `npm`, with `--source URL` overriding the configured
`advisories.osvExportBaseUrl`). It writes `<ecosystem>-osv-schema<N>.db` (e.g.
`npm-osv-schema3.db`) into `DIR` and exits non-zero on failure. `--upload` also publishes the
artifact to the advisory bucket, a full sync cycle in one invocation, and aborts at once without a
configured bucket. A corrupt or truncated export aborts the compile without publishing, so a
running proxy keeps its last-good database. To avoid an idling Pilot pod, run the one-shot as a
Kubernetes `CronJob` with `concurrencyPolicy: Forbid`, which keeps it a singleton. Give the pod
`s3:PutObject` through IRSA or workload identity rather than mounted keys, and schedule it less
often than the proxy polls.

Pin the image by digest and verify its provenance and SBOM attestations before you run it. The
recipe is in [Verifying the image](https://github.com/AlexaDeWit/Ecluse/blob/main/README.md#verifying-the-image).

## The recommended topology

This is the posture the [threat model](@/docs/threat-model.md) treats as
canonical. Aim for it unless you have a specific reason to diverge.

1. **Run three registries, not one.** Give the three roles distinct backends. The publication
   target is a first-party store, the mirror target is a public-derived store, and
   `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` is a pull-through read endpoint that unions both.
   Separating provenance keeps the mirror auditable. **The one hard rule:** the aggregating
   endpoint must union **trusted** stores only, never a direct public upstream. Otherwise raw
   ungated packages reach clients as trusted and bypass the gate. See
   [registry-level composition](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity.** The default forwards each caller's credential to the
   private upstream and publication target. Access then matches your registry IAM exactly, and
   Écluse holds no standing read credential. Nothing to set. See
   [Credential flow and authority](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#credential-flow-and-authority).
3. **Mint the mirror-write token from the container role.** Point
   `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` at a CodeArtifact endpoint. The worker then mints a
   short-lived token under the task or instance role instead of carrying a static secret. Scope
   that role **write-only** to the mirror store and keep
   `ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION` short: it is Écluse's only standing
   credential and it writes the trusted store. Scope the mirror queue the same way. Grant only the
   serve role `SendMessage`, and only the worker
   `ReceiveMessage`/`DeleteMessage`/`ChangeMessageVisibility`. Anyone who can write the queue can
   force a write to the trusted store. `ChangeMessageVisibility` is load-bearing, not optional. The
   worker uses it to hold a long publish. It also backs a **dead-lettered** poison message off so
   the message rides your redrive policy to the DLQ. Without the grant an over-cap artifact
   silently churns on the ordinary visibility cadence instead.
4. **Let the edge own access, and leave `ECLUSE_SERVER__AUTH_TOKEN` off.** Écluse is not your
   access boundary. Front it with a gateway, mesh, or IAP, and restrict reachability **both**
   north-south and east-west (pod-to-pod). An ingress-only allow-list that leaves the pod reachable
   inside the cluster is a common vulnerability. See
   [Edge authentication](@/docs/deployment.md#edge-authentication-and-client-credentials).
5. **Fence egress, keep metadata reachable.** Default-deny outbound. Allow only your upstreams, the
   mirror target, the metadata endpoint, and the advisory bucket when `ECLUSE_ADVISORIES__BUCKET`
   is set (the proxy needs `s3:GetObject` to sync it). Require IMDSv2 with hop limit 1. Do not
   block the metadata endpoint: Écluse needs it to mint credentials. See
   [Network egress](@/docs/deployment.md#network-egress).
6. **Make the proxy unbypassable.** Deny CI runners (and, where practical, workstations) outbound
   access to the public registries. See [Locking down CI egress](@/docs/deployment.md#locking-down-ci-egress).
7. **Verify what you run.** Pin the image by digest and verify its attestations
   ([Verifying the image](https://github.com/AlexaDeWit/Ecluse/blob/main/README.md#verifying-the-image)).

The reasoning behind each choice, and the residual risks it accepts, is in the
[threat model](@/docs/threat-model.md) and
[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#trust-assumptions--credential-posture).

## What a deviation costs

Écluse still runs if you diverge, but each deviation trades away a protection, and one is
**silent** (Écluse cannot detect it, so nothing warns you):

- **Collapsing the registries onto one store** (declaring `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` equal
  to the private upstream, or `ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET` onto either). The perimeter
  holds, but first-party and public-derived packages share one store, so you lose provenance
  separation and clean post-incident scoping. The proxy logs a boot warning for each pair of a
  mount's endpoints that resolve to the same registry. In addition, **Dredger refuses to boot** if
  `MIRROR_TARGET` equals `PUBLICATION_TARGET`, since automated pruning on a shared store risks
  first-party data loss.
- **Pointing the private upstream at a registry that itself draws from public** (say a CodeArtifact
  repo with the stock `npm-store` upstream to npmjs). This is the **dangerous one**, and Écluse
  **cannot detect it**. Raw ungated packages reach clients through the trusted read path, behind
  the gate instead of through it. That nullifies the rules, integrity floor, and freshness
  quarantine. Aggregate **trusted stores only** into the private upstream, and let the gated mirror
  be the only way public content enters.

The [threat model](@/docs/threat-model.md) records both. The other deviations
self-announce. An open edge leans on your network boundary. A static publish credential fails
closed at boot without that edge, and a static mirror-write secret forgoes the minted token.

## Edge authentication and client credentials

Edge authentication to the proxy has two shipped modes:

1. **Open**: `ECLUSE_SERVER__AUTH_TOKEN` unset, so the network layer (VPC, service mesh) owns
   access control. Appropriate only on a closed network.
2. **Static token**: `ECLUSE_SERVER__AUTH_TOKEN` set. Clients send it as
   `Authorization: Bearer <token>`. For an npm-protocol client that is the `_authToken` line, keyed
   by the mount's host and path:

   ```ini
   # .npmrc
   registry=https://ecluse.example.internal/npm/
   //ecluse.example.internal/npm/:_authToken=${ECLUSE_TOKEN}
   ```

The edge token never becomes the upstream one. Reads run **passthrough**: Écluse forwards the
caller's own credential to the private upstream, which stays the authority on what that caller may
see. It strips that credential before the anonymous public fetch, so a client token never leaves
for a public registry. It never caches the private origin across callers, so one caller's read can
never answer another's. By default the only credential of Écluse's own is a mirrored mount's write
to the mirror target, derived from the mirror-target URL.

A `publish` forwards the publisher's own token the same way. Opt into a static
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN` and Écluse publishes as itself instead. That needs
`ECLUSE_SERVER__AUTH_TOKEN`, or the boot refuses (`PublishStaticCredentialNeedsEdge`), because the
pairing would let any unauthenticated client publish under it. `ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW`
limits which package names a client may publish. It authorises _names_, not _callers_, and it is
not authentication. The reasoning is in
[security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#a-static-publish-credential-is-fail-closed) and
[Publishing first-party packages](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#publishing-first-party-packages-the-publication-target).

## Network egress

Écluse fetches from the registries you point it at, and some URLs it follows (a version's
`dist.tarball`) come from upstream responses. Apply least-privilege egress in two layers. Écluse
provides the first in the application, with an origin-aware trust model:

- **Untrusted origins** are the public upstream and every `dist.tarball`. A host+port **allowlist**
  gates them, Écluse fetches them **HTTPS-only** with TLS certificate validation, and response-size
  limits bound them. An upstream URL with no explicit port authorises port 443 alone. Write a
  nonstandard port out (`https://repo.internal:8443`) to authorise exactly that `host:port`. A
  non-HTTPS upstream, or a port outside `1..65535`, fails closed at boot. Certificate validation is
  the guarantor against the resolve-to-internal and DNS-rebinding SSRF class. No address a name
  steers to can present a CA-trusted certificate for the host. A **literal internal-range block**
  adds defence in depth: loopback, link-local including the `169.254.169.254` metadata endpoint,
  RFC1918, CGNAT, and IPv6 ULA. It refuses a `dist.tarball` whose host is an internal-address
  literal. Extend that block with `ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES`.
- **The trusted private origin** (`ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM`) is deliberately **not**
  subject to the internal-range block: a private registry legitimately lives on your internal
  network.

**The `dist.tarball` host gate.** Upstream chooses `dist.tarball`, so Écluse fetches a tarball only
from the same allowlisted host that served the listing. It compares host **and port** as a pair.
Écluse upgrades a plaintext `dist.tarball` to https on its own host. On any other host it drops the
tarball and skips the version. There is no widening knob.

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

**Do not block the metadata endpoint or internal ranges for the proxy itself.** Écluse reaches
metadata through the AWS SDK to mint its instance-role credentials. Denying it breaks those
credentials. IMDSv2 hop limit 1 keeps the minting working while stopping a neighbour or forwarded
request from reaching metadata through extra hops. Grant the proxy only the cloud permissions it
needs: the mirror-write credential and the advisory-bucket read (`s3:GetObject`) when
`ECLUSE_ADVISORIES__BUCKET` is set, nothing more. The trust assumptions behind this are in
[Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#trust-assumptions--credential-posture).

Pilot and Dredger need distinct, tightly scoped egress:

- **Pilot**: no public ingress. Egress to the OSV export host in
  `ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL` (default `osv-vulnerabilities.storage.googleapis.com`),
  the metadata endpoint, and your object store (`s3:PutObject` to upload the advisory database).
  Pilot names the object `<ecosystem>-osv-schema<N>.db` (e.g. `npm-osv-schema3.db`). The key is
  stable per ecosystem, so bucket policies and the proxy's ETag polling can target it. On an
  export-host `5xx`/`408`/`429`, Pilot retries with capped, jittered backoff, so a transient outage
  cannot get your NAT address rate-limited.
- **Dredger**: no public ingress. Egress only to your private mirror for delete requests and to
  the metadata endpoint for credentials. It holds a standing high-privilege delete capability, so
  isolate it from all untrusted networks.

## Locking down CI egress

The controls above secure Écluse's own egress. This one secures your consumers'. If you control CI,
**deny runners outbound access to the public registries** (`registry.npmjs.org` and the equivalents
for other ecosystems), and let them reach only Écluse and your internal services. A misconfigured
job then fails instead of pulling an unvetted package. A stray `--registry` flag, a committed
`.npmrc`, or a tool that ignores your settings cannot route around a network that only reaches
Écluse. That makes the policy _unbypassable_ rather than merely _default_
([MOTIVATION, The bar](https://github.com/AlexaDeWit/Ecluse/blob/main/MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around)). The same idea
extends to developer workstations, a softer control than CI.
