# Écluse operator manual

How to deploy Écluse, configure it, point your tooling at it, and fence its network egress. The
[architecture documents](docs/architecture.md) carry the _why_. Écluse is pre-launch and under
active development.

## Contents

- [What Écluse is](#what-écluse-is)
- [How it works](#how-it-works)
- [Quick start](#quick-start)
- [Deploying Écluse](#deploying-écluse)
- [Configuring Écluse](#configuring-écluse)
- [Operating Écluse](#operating-écluse)
- [Appendix: runtime-sizing arithmetic](#appendix-runtime-sizing-arithmetic)
- [Learn more](#learn-more)

## What Écluse is

Écluse is a proxy you put in front of public package registries to protect the builds that install
from them. Point your CI and developer tooling at Écluse instead of at a public registry. Écluse
fetches from that registry on their behalf and decides which versions a build may install. The npm
registry is the first one supported, and any client that speaks its protocol works, such as npm,
pnpm, yarn, or bun.

A new public version waits in a quarantine, seven days by default, before a build can install it.
Most malicious publishes are found and pulled within days, so the wait alone sidesteps them, with
no attempt to detect malice. A version that an advisory names as the fix for a vulnerability skips
the wait, so the quarantine never delays a security patch. Everything else is deny by default and
opt-in by name.

If you run a private registry, Écluse reads it first and passes your own packages through
untouched. It can also mirror each admitted public version into that registry, so a mirrored
version survives a public outage or yank. AWS CodeArtifact is supported today. Écluse hosts no
packages itself.

Écluse ships as one container image with three roles. `ecluse proxy` serves clients and runs the
mirror worker. `ecluse pilot` compiles the advisory database the fast lane reads. `ecluse dredger`
prunes the mirror store. [Deploying Écluse](#deploying-écluse) covers all three.

## How it works

### Three registry roles

Écluse sees registries by role. Each role is a URL on a mount, and a mount is one ecosystem
(`npm` today), served under its own path prefix (`/npm/`).

- The **public upstream** is the registry Écluse gates, `registry.npmjs.org` by default. Écluse
  never trusts it blindly.
- The **private upstream** is your own registry. Écluse trusts what it holds. It is optional:
  without one, Écluse is a pure gate on the public registry.
- The **mirror target** is where Écluse writes admitted public versions. Declaring one is what
  makes a mount mirror. It is optional.
- The **publication target** receives your own `publish` requests. It is opt-in.

These are roles, not necessarily separate servers. The [recommended topology](#the-recommended-topology)
gives each its own store and explains what you lose when two share one.

### A request, step by step

A client asks for a package's version listing, then for a tarball.

1. **The listing.** Écluse fetches the private and the public registry in parallel. It trusts
   every private version that meets the trusted integrity floor, gates every public version
   through the policy, and serves the merged listing. A public version the policy did not admit is
   absent from it, so a resolver never picks it.
2. **The tarball.** A private hit streams through unfiltered. A private miss is gated on its
   public metadata. When admitted, Écluse streams the tarball from the public registry, and a
   mirror job copies it into the mirror target in the background. Mirroring is demand-driven: only
   a version a client pulls gets mirrored.
3. **A publish.** Off unless you configure a publication target. With one, Écluse refuses any name
   outside your allow-list before it writes upstream.

No rule ever re-gates a version your private registry already holds. Only the trusted integrity
floor applies to it.

### The policy

The policy is deny by default: a public version reaches a build only when a rule admits it. Rules
run in precedence order and the first decisive one wins. The revoke and the install-time deny sit
above every allow by default. Two rules ship on:

- **`min-age`** admits a public version older than seven days. This is the quarantine.
- **`remediation-fast-track`** admits a version a synced advisory names as the exact fix for a
  vulnerability, as long as no other advisory still affects it. It abstains until a first
  advisory database syncs, so without one only the quarantine governs.

Four more rules are off and opt in by name:

- a pin for a package or version, or an allow-list for your own scopes (`AllowByIdentity`)
- a revoke of a package or version (`DenyByIdentity`)
- a deny for packages that run code at install time (`DenyInstallTimeExecution`)
- a deny for versions with a known vulnerability above a severity you choose (`DenyIfCve`)

[Rule policy](#rule-policy) has their knobs.

Independent of the rules, Écluse serves a public version only if it carries a digest that meets
the public integrity floor, `sha256` by default. One gotcha: on a custom or off-spec public
upstream, versions without such a digest silently disappear and their tarballs `403`. To serve such
a source, give it the private upstream role and loosen the trusted floor below `sha256`. The
mechanics are in [Integrity floors](docs/architecture/security.md#integrity-floors).

## Quick start

The fastest way to put the gate in front of real installs is a **serve-only** deployment. It needs
no private registry, no mirror, no queue, and no cloud account.

```bash
ECLUSE_MOUNTS__NPM__ENABLED=true \
ECLUSE_SERVER__PUBLIC_URL=http://127.0.0.1:8080 \
ecluse proxy
```

Then point your package manager at the npm mount. For an npm-protocol client:

```ini
# .npmrc
registry=http://127.0.0.1:8080/npm/
```

Every rule, advisory gate, integrity floor, and egress control applies exactly as on a mirrored
deployment. Only the mirror write is missing. The trade: the public leg is permanent, availability
stays coupled to the public registry, and no mirrored copy survives an upstream yank. Use it to
evaluate the gate, then graduate to the [recommended topology](#the-recommended-topology) by
declaring a `mirrorTarget`.

## Deploying Écluse

### The image and its roles

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
recipe is in [Verifying the image](README.md#verifying-the-image).

### The recommended topology

This is the posture the [threat model](https://ecluse-proxy.com/threat-model.html) treats as
canonical. Aim for it unless you have a specific reason to diverge.

1. **Run three registries, not one.** Give the three roles distinct backends. The publication
   target is a first-party store, the mirror target is a public-derived store, and
   `ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM` is a pull-through read endpoint that unions both.
   Separating provenance keeps the mirror auditable. **The one hard rule:** the aggregating
   endpoint must union **trusted** stores only, never a direct public upstream. Otherwise raw
   ungated packages reach clients as trusted and bypass the gate. See
   [registry-level composition](docs/architecture/registry-model.md#registry-level-composition-the-recommended-topology).
2. **Let callers use their own identity.** The default forwards each caller's credential to the
   private upstream and publication target. Access then matches your registry IAM exactly, and
   Écluse holds no standing read credential. Nothing to set. See
   [Credential flow and authority](docs/architecture/registry-model.md#credential-flow-and-authority).
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
   [Edge authentication](#edge-authentication-and-client-credentials).
5. **Fence egress, keep metadata reachable.** Default-deny outbound. Allow only your upstreams, the
   mirror target, the metadata endpoint, and the advisory bucket when `ECLUSE_ADVISORIES__BUCKET`
   is set (the proxy needs `s3:GetObject` to sync it). Require IMDSv2 with hop limit 1. Do not
   block the metadata endpoint: Écluse needs it to mint credentials. See
   [Network egress](#network-egress).
6. **Make the proxy unbypassable.** Deny CI runners (and, where practical, workstations) outbound
   access to the public registries. See [Locking down CI egress](#locking-down-ci-egress).
7. **Verify what you run.** Pin the image by digest and verify its attestations
   ([Verifying the image](README.md#verifying-the-image)).

The reasoning behind each choice, and the residual risks it accepts, is in the
[threat model](https://ecluse-proxy.com/threat-model.html) and
[Security posture](docs/architecture/security.md#trust-assumptions--credential-posture).

### What a deviation costs

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

The [threat model](https://ecluse-proxy.com/threat-model.html) records both. The other deviations
self-announce. An open edge leans on your network boundary. A static publish credential fails
closed at boot without that edge, and a static mirror-write secret forgoes the minted token.

### Edge authentication and client credentials

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
[security posture](docs/architecture/security.md#a-static-publish-credential-is-fail-closed) and
[Publishing first-party packages](docs/architecture/registry-model.md#publishing-first-party-packages-the-publication-target).

### Network egress

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
[Security posture](docs/architecture/security.md#trust-assumptions--credential-posture).

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

### Locking down CI egress

The controls above secure Écluse's own egress. This one secures your consumers'. If you control CI,
**deny runners outbound access to the public registries** (`registry.npmjs.org` and the equivalents
for other ecosystems), and let them reach only Écluse and your internal services. A misconfigured
job then fails instead of pulling an unvetted package. A stray `--registry` flag, a committed
`.npmrc`, or a tool that ignores your settings cannot route around a network that only reaches
Écluse. That makes the policy _unbypassable_ rather than merely _default_
([MOTIVATION, The bar](MOTIVATION.md#the-bar-a-chokepoint-you-cant-step-around)). The same idea
extends to developer workstations, a softer control than CI.

## Configuring Écluse

### Two layers, one spelling rule

Configuration has two layers. **Environment variables** carry process and secret values. An
optional **config document** (YAML) carries the two things flat variables express badly: the rule
policy and the mount map. A value resolves as defaults < config document < environment variable,
so the environment wins. The boot log carries one `config:` line per resolved key, naming the layer
that supplied it and redacting secrets. `ecluse check-config` prints the same dump.

> **One spelling rule.** Environment variables are the mechanical transliteration of the document
> schema: `__` descends into an object and `_` joins a camelCase word. So `ECLUSE_CACHE__MAX_BYTES`
> spells `cache.maxBytes`, and `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` spells `mounts.npm.mirrorTarget`.

A mount serves only when you declare it. Any `ECLUSE_MOUNTS__<ECOSYSTEM>__*` variable, or any key
under `mounts.<ecosystem>` in the document, activates that mount. A mount you never mention stays
off. Declaring `mirrorTarget` makes an active mount **mirror**, and a mirrored mount then requires
its private upstream, so the mirror reads back. Omit `mirrorTarget` and the mount is serve-only.
Each boot logs one posture line per mount and warns on any pair of a mount's endpoints that resolve
to the same registry. The design rationale is in
[Configuration and authentication](docs/architecture/configuration.md#configuration).

Three ambient AWS-SDK variables are read from the process environment and are not document keys.
`AWS_REGION` scopes SQS only under an `AWS_ENDPOINT_URL_SQS` override (a real SQS URL carries its
own region) and the S3 advisory client, never CodeArtifact. `AWS_ENDPOINT_URL_SQS` overrides the
SQS endpoint and forces the SQS interpretation of `queue.url`. `AWS_ENDPOINT_URL` overrides the S3
advisory client only, never SQS. Neither endpoint value may carry a credential in its authority:
Écluse refuses one, so `AWS_ENDPOINT_URL_SQS` fails the boot and `AWS_ENDPOINT_URL` is ignored.

### The configuration reference

The defaults the binary embeds, with every key, its default, and its meaning:

[config/default.yaml](config/default.yaml)

### The configuration document

A YAML file at `/etc/ecluse/config.yaml`. `ECLUSE_CONFIG` relocates it, and is the one
process-level setting with no document key. With it set, a missing file is a boot error. At the
default path an absent document is fine. The document carries only what you change: the **rule
policy** (see [Rule policy](#rule-policy)) and, for multi-mount deployments, the **mount map**. A
single-mount npm deployment on the default policy needs none. The schema is the
[embedded default](#the-configuration-reference) above, and an unknown key anywhere in the
document is a boot error.

A worked document for a mirrored npm deployment. Reads resolve against a private CodeArtifact
endpoint, approved public packages mirror into a separate CodeArtifact store, and the quarantine
widens to fourteen days. That distinct read endpoint and mirror store is the
[recommended topology](#the-recommended-topology).

```yaml
server:
  publicUrl: https://ecluse.example.internal
  helpMessage: Contact the ACME platform team for access

queue:
  url: https://sqs.us-east-1.amazonaws.com/123456789012/ecluse-mirror

advisories:
  bucket: acme-ecluse-advisories

mounts:
  npm:
    privateUpstream: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/npm/internal/
    publicUpstream: https://registry.npmjs.org
    mirrorTarget: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/npm/mirror/

rules:
  min-age:
    ageSeconds: 1209600
```

Delete the `mirrorTarget` line and the same mount is serve-only. It still merges the private
upstream with the gated public registry, it never writes, and `queue` then goes unread. Delete
`privateUpstream` as well and the mount is the pure public gate of the
[quick start](#quick-start) in document form. `enabled: true` is then the only key it needs,
because `publicUpstream` already has a default.

No token appears above. Écluse mints the mirror-target write credential from the CodeArtifact host.
Every other secret is an environment variable.

### Secrets

Secrets never live in the config document. Client and registry tokens are always env vars. A
CodeArtifact mirror target needs none: Écluse mints its short-lived write token from the
container's ambient AWS credentials. Any other mirror-target host needs
`ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN`. A **mirrored** mount therefore holds one write
credential, and a serve-only mount never writes and holds none.

The secret-typed variables also accept the container-secret file pattern. Set the `_FILE` form
(`ECLUSE_SERVER__AUTH_TOKEN_FILE`, `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN_FILE`,
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN_FILE`) to a file path. The file's contents, with
trailing newlines stripped, become the value, so the token never enters the environment. Setting
both a variable and its `_FILE` form, or naming an unreadable file, is a fail-loud boot error.

A registry URL never carries a token either. Écluse refuses an endpoint written with userinfo
(`https://user:token@host/`), a query string, or a fragment at boot, and the error names the key.
The same refusal covers `server.publicUrl`, `advisories.osvExportBaseUrl`, and `queue.url`. That
is why the `config:` boot echo and `ecluse check-config` print each endpoint in full. What Écluse
does with a client's own token is under
[Edge authentication](#edge-authentication-and-client-credentials). The credential model is in
[Credential flow and authority](docs/architecture/registry-model.md#credential-flow-and-authority) and
[Outbound registry credentials](docs/architecture/configuration.md#outbound-registry-credentials).

### Validation

Écluse validates the configuration in full at startup and refuses to start on any problem. An
unknown rule type, a bad URL, or an unresolved policy reference all stop the boot. A
misconfiguration is then a loud, immediate failure rather than a quietly mis-enforced policy.
`ecluse check-config` runs the same validation without starting anything. The validation model is
in [Validation: fail fast, reject the unknown](docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown).

### Rule policy

The policy is a named map of rules over the deny-by-default gate described in
[The policy](#the-policy). It lives in the config document's `rules` object. `ECLUSE_RULES`
carries the same object as JSON, which suits a one-rule tweak, and the document stays the
reviewable home for a real policy. The shipped default:

- **`min-age`** (`AllowIfOlderThan`): admit public versions older than a quarantine window (7 days
  by default), the core defence against race-to-publish typosquatting and dependency confusion.
- **`remediation-fast-track`** (`AllowIfRemediatesCve`): admit a release a synced advisory names as
  its exact fixed version ahead of the quarantine, provided no other advisory still affects it. It
  abstains until a first advisory database syncs (set `ECLUSE_ADVISORIES__BUCKET` and run Pilot),
  so without one only the quarantine governs.

Every other built-in rule is off by default and opts in by name:

- **`AllowByIdentity`**: admit a specific package or `package@version` past the quarantine, at the
  top of the allow band but still below every deny.
- **`DenyByIdentity`** (the `revoke` shape): a hard deny for a specific package or `package@version`.
- **`DenyInstallTimeExecution`**: deny install-time code execution (off because many legitimate
  packages ship install scripts).
- **`DenyIfCve`**: block a version a synced advisory records as affected at or above a CVSS
  `minSeverity` (0-10). The npm malware feed carries no score and counts as above every threshold,
  so enabling it also blocks known-malicious packages. It sits just below `AllowByIdentity`, so an
  identity pin overrides it. Its `onUnavailable` knob (`deny` by default, or `skip`) decides what
  happens when the advisory database cannot answer. Read
  [Onboarding DenyIfCve](#onboarding-denyifcve) before enabling.

A shipped name patches that rule, `enabled: false` suppresses it, and a new name with a `type`
adds a rule. Precedence defaults per type, and an integer `precedence` overrides it:

```yaml
rules:
  min-age:
    ageSeconds: 1209600
  remediation-fast-track:
    enabled: false
  deny-scripts:
    type: DenyInstallTimeExecution
  revoke-bad:
    type: DenyByIdentity
    identity: bad-package
  pin-fix:
    type: AllowByIdentity
    identity: left-pad@1.3.0
  deny-known-cves:
    type: DenyIfCve
    minSeverity: 8
```

The precedence values, the patch/add/suppress merge model, and the strict validation are in
[Rule policy](docs/architecture/configuration.md#rule-policy) and
[Rules engine](docs/architecture/rules-engine.md#evaluation-model).

### Onboarding DenyIfCve

`DenyIfCve` can break a cold deployment. On a freshly stood-up mirror it can deny historical
versions your existing builds still depend on that an advisory has since covered. Enable it *after*
you warm your private mirror:

1. Leave `DenyIfCve` out of your policy and run Écluse normally, so your CI and developers pull the
   versions you depend on. Each lands in the trusted store, which the rules never re-gate once the
   version is there.
2. Once your must-have builds have mirrored, add `DenyIfCve` with a `minSeverity` you are
   comfortable with. A threshold of 8 blocks high and critical CVEs, and malware blocks regardless
   of the threshold.
3. If Écluse then denies a specific version you must keep, pin it with an `AllowByIdentity` rule,
   which outranks `DenyIfCve`. That covers a false positive or a risk you accept.

Set `onUnavailable: skip` if you would rather the gate fail open (skip itself, logging loudly) than
refuse traffic when the advisory database is briefly unavailable. The default `deny` fails closed.

## Operating Écluse

### Health probes

`GET /livez` reports process liveness: `200` while the process is healthy and `503` when it is
not. On a mirroring deployment a stalled mirror worker fails it. On a serve-only deployment
liveness is the listener alone.

`GET /readyz` reports config loaded and the listener serving. It is deliberately lenient about
public-upstream reachability, so a transient blip does not pull a healthy pod from rotation. It
answers `503` in exactly two cases: the instance is draining, or it is still starting up. With an
advisory bucket configured, that startup gate also waits for each ecosystem's first advisory sync,
a one-way flip that never flaps back. Give a cold pod room for that first database download: a
Kubernetes `startupProbe`, or a readiness `failureThreshold` sized for it. Mounting an ecosystem
whose artifact Pilot never publishes leaves the pod never ready.

The npm liveness probe `GET /npm/-/ping` answers locally with `200 {}`. `GET /npm/-/v1/search`
returns `501` by design: search is a discovery convenience, not an install path. Pilot and Dredger
export the same `/livez` and `/readyz` on `ECLUSE_SERVER__PORT`.

### Graceful shutdown and pod drain

On `SIGTERM`/`SIGINT` Écluse drains in-flight work rather than dropping it. `GET /readyz` flips to
`503`, the signal a load balancer or mesh watches to stop routing new traffic here, while
`GET /livez` stays `200`. An orchestrator therefore does not kill a still-draining instance early.
Every response then carries `Connection: close`, so a keep-alive pool reconnects to a ready
instance. The process finishes in-flight requests and in-progress artifact streams before exiting,
so a half-delivered tarball runs to completion.

`ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT` bounds the drain at 30 seconds by default. **Set the
platform's termination grace period above it**, so the orchestrator does not `SIGKILL` mid-drain.
On Kubernetes that is `terminationGracePeriodSeconds`. On an interactive terminal a second `Ctrl+C`
(or `Ctrl+D`) forces an immediate halt that bypasses the drain. That halt needs standard input to
be a TTY, so production has no such bypass.

### Exit codes

The exit status states how a run ended, so an orchestrator can branch without parsing logs:

| Code | Meaning |
|---|---|
| `0` | Graceful shutdown: the drain completed and the services returned. |
| `1` | A service exited abnormally. The last `ecluse: service exited:` line on standard error carries the detail. |
| `2` | The boot aborted: Écluse rejected the configuration or wiring and reported every problem. A restart without changes fails identically. |
| `3` | Something outside cancelled the run: a kill that bypassed the graceful path. |
| `130` | The local-development halt (Ctrl-D on an interactive terminal). |

### Logs

One JSON object per line by default (`ECLUSE_OBSERVABILITY__LOG_FORMAT=json`), or `console` for
local development. Each JSON line carries `timestamp` (RFC 3339 UTC), `status` (`debug`, `info`,
`warn`, `error`), `message`, and the `service`/`env`/`version` identity. While a span is in scope
the line also carries a `dd` object with `trace_id` and `span_id`. The emitting call's own fields
sit under `data`, and the `katip` emitter fields under `katip`. Those include the emitting
process's hostname (`katip.host`), so a collector's own host attribution governs the line's
`host`. `timestamp`, `status`, `message`, and `service` are Datadog's reserved log attributes, and
its JSON preprocessing reads them unmodified. `env` and `version` are ordinary attributes any
backend indexes. `ECLUSE_OBSERVABILITY__LOG_LEVEL` sets the floor (`info` by default).

Bearer tokens render as a redacted placeholder, and on every running path Écluse reduces a URL to
its host and port. Neither token material nor a signed query string reaches a log field. The
boot-time configuration echo prints each configured endpoint as you gave it. That is safe, because
the boot refuses a URL that carries a credential (see [Secrets](#secrets)).

### Telemetry (opt-in)

Set `ECLUSE_OBSERVABILITY__TELEMETRY=on`, then `DD_*` (`DD_SERVICE`, `DD_ENV`, `DD_VERSION`,
`DD_AGENT_HOST`) for Datadog or the standard `OTEL_*` for any other backend. `DD_*` wins where both
are set, and the resolved identity stamps both traces and every log line. With no `DD_VERSION` or
`service.version` set, exported traces and log lines carry the running binary's own build version.
The version tag is never blank. `DD_API_KEY`/`DD_SITE` have no effect, because Écluse exports only
to a node-local collector or Agent, at `http://localhost:4318` by default or wherever
`DD_AGENT_HOST`/`OTEL_EXPORTER_OTLP_ENDPOINT` points. Authenticate a remote collector out of band
with `OTEL_EXPORTER_OTLP_HEADERS`.

### Memory plan and runtime sizing

Every byte-valued bound is a named tenant of the effective heap ceiling, not an independent
multiplier. The tenants are the cache, response cap, publish aggregate, and in-memory queue. Each
one boot-logs as a `memory plan:` line. A pod too small for the tenants' floors **degrades
gracefully instead of refusing**. It sheds the mirror-artifact cap first, then the cache, each to
zero if needed, then serves uncached. Each step is a loud warning, and it always boots. Only an
explicit override that breaks the plan refuses (exit `2`). The model is in
[Runtime sizing](docs/architecture/configuration.md#runtime-sizing-cores-and-heap-ceiling).

Cores and the heap ceiling resolve at boot from config, else the cgroup, else the runtime's own
posture. The boot log records each decision with its provenance. The whole-cores guidance and
the per-pod memory arithmetic are in the [appendix](#appendix-runtime-sizing-arithmetic).

A cold install against an empty cache hits the proxy with dozens of heavy requests at once, which
causes latency spikes or `503` backpressure. Run one install after starting Écluse and before
production traffic. Once warm, request coalescing absorbs spikes.

### Revoking a mirrored version (internal yank)

The mirror store deliberately resists upstream yanks, so a benign yank does not break your
installs. A version later found malicious therefore stays, because Écluse never re-gates trusted
content. Usually this resolves itself: once the public registry yanks the bad version, re-mirroring
cannot reproduce its bytes and you purge the stale copy at leisure. When your own scanning is ahead
of the public yank, revoke in order. First **deny the identity** with a `DenyByIdentity` rule, so
the serve path stops admitting the version and the worker stops re-mirroring it. Then **purge that
version** from the mirror. That **order matters:** purge alone is a treadmill, since the next
install re-admits and re-mirrors a version still live upstream.

### Poison mirror jobs

Some mirror jobs can never succeed. Examples: an artifact past `ECLUSE_LIMITS__MAX_ARTIFACT_BYTES`,
a payload that no longer decodes, or a publish target that refuses it every time. On SQS, **attach
a redrive policy with a dead-letter queue** to the mirror queue. The worker leaves such a message
undeleted. Your policy then moves it to the dead-letter queue, where you can read it and work out
what happened. At boot, Écluse reads the queue's redrive configuration. If the queue has no policy,
start-up logs a loud `WARNING` that poison messages have no terminus. If the probe itself fails,
that warning names the missing `sqs:GetQueueAttributes` permission. Either way the process boots.

Without a dead-letter queue, nothing captures that message. SQS redelivers it, and the worker
re-fetches the artifact each time, until the retention window (up to 14 days) drops it unseen. So
Écluse retires the job itself after `ECLUSE_QUEUE__MAX_RECEIVE_COUNT` deliveries. It writes an
error log naming the job and the reason. The `ecluse.mirror.jobs.processed` counter records it at
`result="discarded"`. **Alert on that series.** Every discard is a job nothing else caught. With a
redrive policy attached, Écluse runs one delivery above its `maxReceiveCount`. Your dead-letter
queue always captures first, and the discard path stays dormant. Either way you lose nothing.
Mirroring is demand-driven, so the next client request for that artifact re-enqueues the job. It
fails the same way until you fix the cause.

## Appendix: runtime-sizing arithmetic

**Give Écluse whole cores.** A fractional CPU limit, say 3.5, has no good option. Claiming 4
capabilities overruns the CFS quota during stop-the-world GC and freezes the process mid-pause.
Flooring to 3 strands the fraction. So pair an integer limit with `requests = limits` (and
exclusive cores where offered) to remove throttling structurally, since Écluse floors the derived
count. A CPU **limit** does not shrink the processor count the runtime sees. Without
`ECLUSE_RUNTIME__CORES` a 2-CPU pod on a 32-core node would claim 32 capabilities and 32
nurseries.

**Memory arithmetic (proxy pod).** The binary ships `-A64m -n4m`, a 64 MiB per-core allocation area
in 4 MiB chunks. That trades bounded extra memory for far fewer GCs under load. Budget roughly
`cores x 64 MiB` of nursery, plus the live heap, which the metadata cache dominates. Add up to one
live-heap of copying headroom during a major GC. Worked shapes:

- a 2-CPU / 512 MiB pod runs as-is
- a 2-CPU / 256 MiB pod also needs `GHCRTS="-A16m"`
- a 4-CPU pod wants ~750 MiB on defaults, or 512 MiB with `-A32m`

Taller pods amortise the cache and coalescing better, so prefer 4-CPU-ish shapes. Tune the
allocation area with `GHCRTS`. The boot log prints the effective value. Pilot and Dredger run
different workloads, so tune their allocation area separately.

## Learn more

The internal design, for when you need the _why_:

- [Architecture overview](docs/architecture.md)
- [Configuration and authentication](docs/architecture/configuration.md)
- [Security posture](docs/architecture/security.md)
- [Threat model](https://ecluse-proxy.com/threat-model.html), the STRIDE register, generated from the OWASP Threat Dragon model ([`threat-modelling/ecluse.json`](threat-modelling/ecluse.json))
- [Rules engine](docs/architecture/rules-engine.md)
- [Multi-ecosystem hosting and URL rewriting](docs/architecture/web-layer.md#web-layer)
- [Release and supply-chain operations](docs/architecture/release-supply-chain.md)
