# Security posture

> Part of the [Écluse architecture overview](../architecture.md).

Écluse builds outbound HTTP requests (private upstream, public upstream, mirror target) from
client-supplied package identifiers and upstream-supplied artifact locations. The operator
manual states what a deployment must fence around that
([Securing network egress](https://ecluse-proxy.com/docs/deployment/#network-egress)). This document
records the deployment assumptions the threat model rests on and the credential posture. It also
records the two floors that fail closed: the integrity digest and a static publish credential.

> The full STRIDE threat register lives in the Saerskriven model
> ([`threat-modelling/ecluse.yaml`](../../threat-modelling/ecluse.yaml)), published readably
> at [Threat model](https://ecluse-proxy.com/docs/threat-model/). The threat statements and
> dispositions live there, not here.

<!--
  Do not re-grow this into a full threat enumeration. The authoritative register is
  the Saerskriven model (threat-modelling/ecluse.yaml), rendered to the Pages site
  from web/content/docs/threat-model.md. Add or revise threats in the model, not here.
-->

## Trust assumptions & credential posture

The [threat model](https://ecluse-proxy.com/docs/threat-model/) records each trust assumption as
its own record, linked to the threats that rest on it. This section explains the posture those
assumptions add up to: per-caller passthrough credentials, the three-registry topology, and
CodeArtifact over VPC endpoints.

**Edge access is an operator concern.** `ECLUSE_SERVER__AUTH_TOKEN` is off by default, so the
deployment's access edge decides who may reach the proxy, east-west as well as north-south.
The private registry decides whether an anonymous caller may read or publish.
Passthrough grants no access beyond that registry's policy. A deployment must therefore enforce
both proxy reachability and the intended private-registry authentication policy.

**Passthrough relocates credential risk to the proxy runtime.** Forwarding each caller's own
credential ([credential flow](registry-model.md#credential-flow-and-authority)) leaves the proxy with no read or publish
credential of its own, unless you configure a static publication token. It does hold every in-transit caller's credential in memory, transiently.
The proxy runtime must protect those credentials. Verify its image through the
[release supply chain](release-supply-chain.md). Token stripping and the prohibition on
credential-bearing redirects prevent credentials from reaching an unintended origin.

**The outbound controls guard the downloads an attacker can influence.** The host allowlist and
the internal-range block apply to the public packument and to every public `dist.tarball`.
That holds on the serve path and on the worker's back-fill fetch alike. They are absent from a trusted,
operator-declared destination (the private upstream, the mirror and publication targets, SQS,
S3, the OTLP collector), which goes where the configuration says. Https-only with certificate
validation applies to every registry endpoint regardless.

**Give each service its own identity.** The mirror worker publishes admitted public copies.
The proxy separately relays first-party publications under the configured publisher credential.
A deployment without mirroring needs no mirror-write identity. Other configured services can
still require workload identity, including CodeArtifact private-topology checks. The per-role grants are in
[Role identities and least privilege](https://ecluse-proxy.com/docs/deployment/#role-identities-and-least-privilege). Scope the mirror worker to publication and the repository reads needed for its presence check. Token-mint permission is separate from both. Prefer container-role minting over a static
secret and minimise its TTL. Dredger needs a different action set: reads and deletion, not
publication. The [operator permission table](https://ecluse-proxy.com/docs/dredger/#permissions)
owns those action and resource scopes.

The mirror queue is part of the same trust boundary. Anyone who can enqueue can request mirror
work. Scope its IAM: only the serve role enqueues, and only the worker consumes.

Schema 2 carries no artifact URL. Legacy decoding validates the old URL and discards it.
The worker resolves source locations from current paired metadata and checks HTTPS authority and
permitted hosts before fetching. It re-decides the version through the shared admission gate before
new mirror writes. Each fetched file must match its re-admitted digest before publication.

**Registry separation is defence-in-depth and auditability, not the perimeter.** The
three-registry topology
([registry-level composition](registry-model.md#registry-level-composition-the-recommended-topology))
keeps first-party and public-derived inventory physically separable. That gives per-provenance
rule-sets, scanning, and clean post-disclosure scoping. Collapsing toward one registry degrades
auditability and mitigation depth but doesn't move the trust perimeter. The public-to-trusted
admission gate is identical at one registry or three. Storage-layer scanning stays the operator's,
as the threat model's assumptions record.

## Integrity floors

**Public artifact admission and metadata listings require a strong digest by default.** Both
trust contexts default to SHA-256 or stronger. The public floor cannot be lowered, while the
trusted listing floor can. A conventional private npm artifact hit bypasses metadata admission
and therefore has no serve-time floor.

The check runs **per file**. A release ships several in most ecosystems, an sdist beside its
wheels on PyPI, so a release keeps the files that clear the floor and loses the ones that do
not, and disappears only when no file of it survives. The surviving set is what the merge plan
carries into the served listing, so the listing and the download gate refuse the same files. A
release whose file set is a singleton, npm's, either survives entire or drops entire.

- The public (untrusted) floor is a hard SHA-256 boundary (`ECLUSE_INTEGRITY__MIN_PUBLIC`,
  default `sha256`). An operator may raise it to `sha384`, `sha512`, or `blake2b` but never
  lower it. Config load rejects a sub-floor or
  unknown value, and never clamps it. The floor refuses a public version with no digest, or
  one below it, such as a legacy SHA-1 `dist.shasum` with no SRI. The artifact gate
  answers `403` and the metadata path filters the file from the listing, so a client
  never sees a file it couldn't verify. SHA-1 and MD5 have practical collisions, so a
  match can't prove the bytes weren't substituted.
- The trusted (private) floor carries the same `sha256` default
  (`ECLUSE_INTEGRITY__MIN_TRUSTED`), so Écluse drops a SHA-1-only or hashless private version
  exactly as a public one. But an operator can loosen it below SHA-256, down to
  `sha1`/`md5`, for a legacy private mirror. There, trust in the operator's own vetted source
  substitutes for cryptographic strength. That's the only way Écluse serves a sub-SHA-256
  digest, and only on the trusted private origin. On the serve path the trusted floor filters
  the private listing. The conventional npm private tarball leg is a
  [conventional stable read](registry-model.md#serving-a-tarball) with no serve-time floor,
  so Écluse still serves a below-floor private artifact. The client and the mirror worker
  verify its bytes.

The trusted floor can be lowered for an operator-controlled source. The public floor cannot.
The types enforce this distinction. No code can
construct `MinIntegrity` (public) below SHA-256, while `MinTrustedIntegrity` (trusted) can go
lower, so no config or constructor path lowers the public floor. The floor admits by
algorithm strength, and the digest is computable for every algorithm it admits. The mirror worker verifies the admitted digest before publication.

RubyGems Compact Index metadata supplies archive SHA-256 values to the supported Bundler clients.
Clients verify streamed downloads. The mirror worker verifies copied bytes before publication.
Digest agreement proves neither source honesty nor harmless archive contents.
The [protocol contract](https://ecluse-proxy.com/docs/protocol-support/#rubygems) defines initial-download and cache limits.

For PyPI metadata companions, the parent distribution must clear its origin's integrity floor.
Companion digest declarations remain separate and impose no new proxy hash floor.
A sidecar cannot borrow an admitted sibling's integrity or a refused duplicate's location.

## A static publish credential is fail-closed

The [first-party publish path](registry-model.md#publishing-first-party-packages-the-publication-target)
relays a client `npm publish` to the publication target. The mount's first-party namespaces
(`ECLUSE_MOUNTS__NPM__FIRST_PARTY`) constrain which package names a client may publish. It
is not authentication and does not verify who is publishing. A deployment may set
a `token` under the publication target's tag, substituting Écluse's own credential for the
authenticated edge token on publishes. If it does, the composition root refuses to boot without a
verifiable inbound edge (`PublishStaticCredentialNeedsEdge`). That makes "static publish
credential plus open edge" unrepresentable. Such a pairing would let any unauthenticated client
publish under the operator's credential within the allowed scopes. `ECLUSE_SERVER__AUTH_TOKEN`
is the verifiable edge Écluse checks today. An external layer (API gateway, mTLS service mesh,
`NetworkPolicy`) is defence-in-depth but can't substitute for it, since Écluse can only verify
its own edge. Pure passthrough (no static token) carries no such floor: the publisher's
forwarded token is the authority.

### The guard-name ≡ write-name ≡ body-name invariant

The npm publish document carries its own declared identity: a top-level `_id` and `name`, and a
`name` per `versions` entry. A publication target that resolves the written package from the
body, the npm-protocol norm, would write a name the scope guard never saw. So an anti-shadowing
guard that validated only the URL-path name while relaying the document byte-for-byte would be
bypassable. A crafted `PUT /@acme/anything` whose body declares `@victim/target` would publish
outside the mount's first-party namespaces, shadowing a public package.

So the guard holds guard-name ≡ write-name ≡ body-name. After the scope check admits the
URL-path name, the guard compares every present declared body name to it: `_id`, top-level
`name`, and each `versions[].name`. Any disagreement is a `403` before any upstream write. The
comparison is `PackageName` equality under the same canonicalisation the route applies
(ecosystem-aware, npm case-sensitive), so an encoding variant (`@acme%2ffoo` vs `@acme/foo`)
can't disagree silently. The guard never decodes the base64 `_attachments`. An absent declared
name is not a bypass-grant. The guard refuses only a present, mismatching name, since a
legitimate client always sends names matching its publish URL. So a target that keys the write
off the URL, or off these declared names, writes the name the guard authorised. The PyPI guard
reads each multipart `name` field and the `:action` field in the same way. A PyPI store can also
read the project and version from the uploaded file's name. So the guard requires the `content`
part's file name to be a wheel or source distribution name of the declared project. The name may
hold only ASCII letters, digits and `-_.!+`. Its version must start with a digit, hold no `-`, and
equal the declared `version` under PEP 440. A source distribution must take the PEP 625 name
`{normalised_name}-{version}.tar.gz`. Its stem then holds exactly one `-`, where the project ends
and the version starts. The guard also refuses a body with a boundary
delimiter inside a part after a bare CR or LF, or with a padded delimiter line. Some multipart
parsers end a part at either. The rule relies on a store that parses the file name as the wheel
and sdist specifications define, and multipart as RFC 2046 defines. Such a store writes the
project the guard authorised, whether it keys the write off the `name` field or off the file name.
