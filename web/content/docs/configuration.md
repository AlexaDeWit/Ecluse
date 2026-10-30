+++
title = "Configuration and policy reference"
description = "Where every Écluse setting lives, which layer wins, where secrets go, and how the rule policy decides what a build may install."
weight = 4
+++

Use this reference to look up a setting, its precedence, or a policy interaction.
For the deployment workflow, see [Package your configuration](@/docs/configure.md).

## Configuration sources {#two-layers-one-spelling-rule}

Écluse combines embedded defaults, an optional YAML file, and environment variables.
The file overrides defaults. Environment variables override the file.
Use YAML for non-secret settings, including the rule policy and mount map.
Supply secrets through the environment or the supported `_FILE` settings.

Startup logs and `ecluse check-config` print each resolved key and its source, with secrets redacted.
Without `advisories.url`, the shipped `deny-known-malicious` and `remediation-fast-track` rules do
not run, so each of their keys says so: `(default, inactive: no advisories.url)`. A key you patched
keeps its own layer, as in `(document, inactive: no advisories.url)`. A rule you activate with
`enabled: true` or a restated `type` carries no marker, and so does a rule you switch off with
`enabled: false`, because a source would not run it either. A shared key under `rules` carries the
marker only while some mount leaves the rule to the source and no mount runs it.

> **One spelling rule.** Environment variables are the mechanical transliteration of the document
> schema: `__` descends into an object and `_` joins a camelCase word. So `ECLUSE_CACHE__MAX_BYTES`
> spells `cache.maxBytes`, and `ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL` spells
> `mounts.npm.mirrorTarget.codeArtifact.url`.

Écluse owns the whole `ECLUSE_` prefix. Every `ECLUSE_*` variable is read as a key of that schema,
and one naming no key aborts the boot rather than resolving to a default, so a typo surfaces at
startup. Only two families are consumed before the resolver sees them and therefore name no key:
`ECLUSE_CONFIG`, and the `_FILE` secret indirections under [Secrets](@/docs/configuration.md#secrets).
Keep your own unrelated settings out of the prefix.

Mounts are off until you declare them. Mentioning one anywhere, whether through an
`ECLUSE_MOUNTS__<ECOSYSTEM>__*` variable or a key under `mounts.<ecosystem>` in the document,
switches it on. Declaring `mirrorTarget` then makes the active mount **mirror**, and a mirrored
mount requires its private upstream, so the mirror reads back. Omit `mirrorTarget` and the mount is
serve-only. Each boot logs one posture line per mount. Endpoint collisions can warn or refuse
startup, as listed below. The design rationale is in
[Configuration and authentication](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#configuration).

Écluse also reads three ordinary AWS-SDK variables from the process environment. They are not
document keys, and each one has a deliberately narrow reach:

| Variable | Écluse reads it for | Never affects |
|---|---|---|
| `AWS_REGION` | Scoping SQS, only under an `AWS_ENDPOINT_URL_SQS` override | CodeArtifact, whose region comes from the mirror-target host |
| `AWS_ENDPOINT_URL_SQS` | The SQS endpoint, and it forces the SQS reading of `queue.url` | S3 |
| `AWS_ENDPOINT_URL` | The S3 advisory and durable mirror-state clients, including against an emulator or a VPC endpoint | SQS |

Écluse ignores `AWS_REGION` for SQS without the override, because a real SQS URL carries its own
region. The AWS SDK still reads `AWS_REGION` on its own to region every other client.

Both endpoint values get the same hygiene as every other URL: whitespace is trimmed, and a value
with userinfo, a query, a fragment, or a malformed port fails the boot. The error names the
variable but never the value, because the value can carry a credential.

## Endpoint collisions

This table describes the implemented checks. Registry comparisons use the configured repository
URLs. Host comparisons also match distinct repository paths on the same host. Any refusal wins
when more than one row applies.

| Compared endpoints | Scope | Comparison | Proxy and mirror | Dredger |
|---|---|---|---|---|
| Mirror target and public upstream | Any mount pair | Host | Refuse | Refuse |
| Publication target and public upstream | Any mount pair | Host | Refuse | Refuse |
| Mirror target and private upstream | Any mount pair | Registry | Warn | Refuse |
| Mirror target and publication target | Same mount | Registry | Warn | Refuse |
| Publication target and private upstream | Same mount | Registry | Allow | Refuse |
| Publication target and another mount's private, mirror, or publication target | Other mounts | Registry | Refuse | Refuse |
| Private and public upstream | Same mount | Registry | Refuse | Refuse |
| One registry declared under different backend tags | Any endpoints | Registry | Refuse | Refuse |

Proxy and mirror serve normally when a publication target equals its own mount's private upstream.
Dredger refuses the pair, because it deletes from the stores it maintains, and a first-party package
can exist only in the publication target.
Distinct repositories sharing a CodeArtifact host are not otherwise collapsed by the registry
comparison. A mount's private and public upstreams must name distinct repositories, because
the private leg forwards caller credentials and admits versions without the public rules.
Boot and `check-config` refuse this collision, naming both configuration keys and the repository.

## The configuration reference

The binary embeds the defaults below, and every key appears with its default and its meaning:

{{ config_reference() }}

## The configuration document

The document is a YAML file at `/etc/ecluse/config.yaml`. `ECLUSE_CONFIG` relocates it, and is the
one process-level setting with no document key. With `ECLUSE_CONFIG` set, a missing file is a boot
error, but an absent document at the default path is fine. Include only the settings you want
to override. The file can hold non-secret process settings as well as the rule policy and mount map.
An environment-only deployment needs no document. The schema is the
[embedded default](@/docs/configuration.md#the-configuration-reference) above, and an unknown key
anywhere in the document is a boot error.

Here is a worked document for a mirrored npm deployment. Reads resolve against a private
CodeArtifact endpoint, approved public packages mirror into a separate CodeArtifact store, and the
quarantine widens to fourteen days. Keeping the read endpoint and the mirror store distinct is the
[recommended topology](@/docs/deployment.md#the-recommended-topology).

```yaml
server:
  publicUrl: https://ecluse.example.internal
  helpMessage: Contact the ACME platform team for access

queue:
  url: https://sqs.us-east-1.amazonaws.com/123456789012/ecluse-mirror

advisories:
  url: s3://acme-ecluse-advisories

mounts:
  npm:
    privateUpstream:
      codeArtifact:
        url: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/npm/internal/
    publicUpstream:
      registry:
        url: https://registry.npmjs.org
    mirrorTarget:
      codeArtifact:
        url: https://acme-123456789012.d.codeartifact.us-east-1.amazonaws.com/npm/mirror/

rules:
  min-age:
    ageSeconds: 1209600
```

Delete the `mirrorTarget` block and the same mount is serve-only. It still merges the private
upstream with the gated public registry, but it never writes, so `queue` goes unread. Delete
`privateUpstream` as well and you are back to the pure public gate of the
[quick start](@/docs/quick-start.md), in document form. `enabled: true` is then the only key it
needs, because `publicUpstream` already has a default.

No token appears above, because the `codeArtifact` tag mints the mirror-target write credential
from its host. Every other secret is an environment variable.

## Store tags

Each of `publicUpstream`, `privateUpstream`, `mirrorTarget`, and `publicationTarget` is an object
with exactly one key, the **tag**, and under it the keys that tag admits for that endpoint. The tag
names the store backend, and the boot checks the URL against it. There is no bare-URL shorthand,
because one fact with two spellings is one fact too many.

| Tag | The store | Where it applies |
| --- | --- | --- |
| `registry` | Any host that speaks the ecosystem's protocol, such as Artifactory, Nexus, or a public registry | Every endpoint. The only tag `publicUpstream` admits |
| `codeArtifact` | An AWS CodeArtifact repository endpoint. Écluse derives its mint identity from the host | Every endpoint but `publicUpstream` |
| `verdaccio` | A Verdaccio development store, on a static token | Every endpoint but `publicUpstream` |

The keys each tag admits depend on the endpoint, because the endpoints hold different credentials.

| Endpoint | `registry` | `codeArtifact` | `verdaccio` |
| --- | --- | --- | --- |
| `publicUpstream` | `url` | not admitted | not admitted |
| `privateUpstream` | `url` | `url` | `url`, optional maintenance `token` and `permitDeletion` |
| `mirrorTarget` | `url`, `token` | `url`, optional `tokenDuration` | `url`, `token`, optional `permitDeletion` |
| `publicationTarget` | `url`, optional `token` | `url`, optional `token` | `url`, optional `token` |

Native upload fields extend this table as the configuration reference describes.
Cargo mirror and publication targets require distinct sparse read and approved API coordinates.
See [Cargo protocol support](@/docs/protocol-support.md#cargo) and [Cargo mirror setup](@/docs/mirror-setup.md#cargo).

Proxy reads forward the caller's own credential to the private upstream, while public reads
remain anonymous. The mirror write uses Écluse's credential: `codeArtifact` mints it and admits
no `token`, while the two non-minting tags require one. Every publication tag admits an optional
static `token`. With one set, Écluse publishes with its own token, and a client's edge token only
grants access to Écluse. This is not the preferred deployment. The boot refuses a static
publication token unless `ECLUSE_SERVER__AUTH_TOKEN` also closes the edge, because an open edge
would let any caller publish under it ([Edge authentication and client credentials](@/docs/deployment.md#edge-authentication-and-client-credentials)).

Dredger also mints a CodeArtifact credential for private-cache maintenance from that
target's own host identity. It never borrows a caller or publication token. Matching mirror and
private mint identities share one provider. This maintenance credential does not change proxy
caller passthrough. Verdaccio uses the private target's own optional maintenance token and deletion consent.
The deleting role requires both. Preview requires no deletion consent. See [Dredger](@/docs/dredger.md).

`permitDeletion: true` is your consent for `ecluse dredger` to delete from a Verdaccio store, and
it exists on no other tag. Without it the Dredger refuses that store, and every other role ignores
the key. Verdaccio has no vendor API, so the Dredger sweeps it with the ecosystem protocol's own
listing and unpublish requests rather than a control plane of its own.

Two rules govern how the layers combine. A layer fills keys under a tag, it never switches one: an
environment variable written under a tag your document did not use leaves the endpoint carrying two
tags, and the boot refuses it naming both. And two endpoints that name the same registry under
different tags are refused for every role, because one store has one backend.

## PyPI uploads and mirror state

[Complete PyPI setup](@/docs/pypi-setup.md) combines the repositories, roles, credentials and rollout order.

PyPI mirror tags also admit `uploadUrl` and `username`. Publication tags also admit `username`.
The role requires these fields as described below.

A PyPI mirror declares the registry root, from which reads append `simple/`, and an explicit native `uploadUrl`. A generic static
write token also declares its Basic `username`. CodeArtifact supplies `aws` at its backend binding.
A CodeArtifact upload URL must name the same domain, account, region and repository as its read root.
Its supported upload paths are listed in the [configuration reference](#the-configuration-reference).
The refresh provider still owns only the secret. Client publication uses the exact configured
publication URL and retains the caller's Basic pair when no static replacement is configured.

Configure `mirrorState.url` independently from `advisories.url`. Its S3-compatible store must
support strong reads, conditional creation and conditional replacement. The proxy needs reads
and prefix listing to distinguish confirmed absence. The worker also needs writes. No mirror
worker deletes state or package bytes. Do not expire project records or referenced companions.
Losing expected history can hide files that the current public index no longer lists.

Partial uploads remain incomplete through Écluse. A state failure answers unavailable.
A missing recorded version that a named rule denies in the current public read does not block the remaining authorised listing.
Without decisive denial evidence, the listing still requires complete file coverage. An absent state record leaves ordinary
unmanaged private handling intact. Those private reads do not prove original-source restoration.
These controls do not promise atomic direct-registry reads.
Project listings still discover public releases. Already mirrored artifact and companion reads
need no public request or new admission after current private authorisation and identity proof.

`MAL-*` advisory IDs do not impose an unconditional deny at any mirror phase. Public serving
and worker ingest apply the configured admission rules. `DenyIfCve` treats an affecting advisory
without a CVSS score as meeting every threshold, subject to rule precedence. Completed private
copies keep the trusted-read path and do not re-run public advisory admission. Dredger reaping
requires a named decisive deny whose `appliesTo` includes `revocation`, plus that store's deletion
consent and the other [deletion guards](@/docs/dredger.md).

A mount's optional `maxRequestBytes` overrides its inherited upload cap without changing another
mount. Unset, it uses the existing global configured or computed value. The memory plan reserves
aggregate capacity for the largest effective publishing cap and refuses an override that cannot fit.
See the generated configuration reference for defaults and environment spellings.

## First-party namespaces

A mount's `firstParty` key names the namespaces your deployment owns. Set it when your own
packages live behind Écluse. The privilege then reaches every path that could confuse one of your
names with a public one.

| Path | What the privilege decides |
| --- | --- |
| Publish | Only a first-party name may be published through the relay. Every other name is a `403`. |
| Serve | A first-party name resolves from `privateUpstream` alone. Écluse never fetches it from `publicUpstream` or merges a public document into it. |
| Mirror | Nothing first-party is mirrored. The worker drops a job that was already queued when you declared the namespace. |

The shape follows the ecosystem, and an empty or malformed list is refused at boot.

| Ecosystem | An entry is | Example |
| --- | --- | --- |
| npm | A scope | `@acme,@beta` |
| PyPI | A distribution name, or a prefix written as a separator then `*` | `acme,acme-*` |
| Cargo | A crate name, or a prefix ending in `-*` or `_*` | `acme-tools,acme-*` |
| RubyGems | A gem name, or a prefix ending in `.*`, `-*`, or `_*` | `acme-tools,acme-*` |

A PyPI entry reads through PEP 503 normalisation, so `Acme_Tools` and `acme-tools` are one name. A
prefix stops at the separator, so `acme-*` covers `acme-tools` and not `acmeco`, which is a name
you do not own. The star has to follow a separator, and `acme*` is refused, because a prefix that
ran past the separator would privilege names you do not own. Declare the bare `acme` too if you
publish a distribution under that exact name.

Exact PyPI declarations must follow the routed project-name grammar: at most 100 ASCII
characters, with a letter or digit at each end. Internal `.`, `_`, and `-` separators remain
accepted and normalised. An exact declaration longer than 100 characters, or with a leading or
trailing separator, fails configuration loading. Prefix declarations `acme-*`, `acme_*`, and `acme.*` remain
equivalent and accepted.

Cargo declarations use the name grammar in the configuration reference.
Ownership folds ASCII case and hyphen/underscore aliases while preserving separator runs.
It does not apply PyPI period folding. Published spelling and index lookup paths remain separate from ownership matching.
The [private package guide](@/docs/private-packages.md#names) gives an example.

RubyGems declarations preserve case, dots, hyphens, and underscores without normalisation.

Setting `firstParty` on an npm mount narrows what a scoped install reaches. A name under one of
your scopes that the private upstream does not have answers `404`, and Écluse does not fall back to
the public registry for it. That refusal is the point: a public package published under a scope you
own is a dependency-confusion attack.

The private upstream is that name's one authority, so why it did not answer decides what your
client sees. A `404` from it settles the question, and Écluse answers `404`. An upstream Écluse
cannot reach, one that faults, and one whose body does not decode all leave the question open
instead, so Écluse answers `503` and the client retries rather than treating the install as
missing.

This is a privilege over names, not authentication. It says which names are yours, never who may
use them, so the private upstream stays the authority on every caller.

For npm and PyPI, a private `401` or `403` stops the read with a local `403`.
This applies to metadata and artifacts, including HEAD and conditional requests. Public content cannot replace the private
copy, even if it has the same name and version or passes public admission and integrity checks.
This protects a private patch from substitution when the caller's private credential fails.
Écluse forwards no upstream error body or headers. npm receives a fixed explanation.
PyPI keeps its local refusal format: an empty body, or the configured operator help message.

Cargo preserves a native `401` challenge for credential-provider retry and treats a real `403` as refusal.
The private alternate never substitutes a public copy, including after a private `404`.

RubyGems keeps a private `401` or `403` status on metadata and archive reads, and never substitutes
a public copy after either.

For sparse index and archive requests, Cargo's public policy and evaluation refusals include Écluse's reason plus configured `server.helpMessage`.
Private refusals, access refusals on either view, and missing or excluded package replies keep only configured operator help.
Without configured help, those replies have no body.

Check the caller's private credential and access when this refusal appears. Ordinary private
`404` responses and other transient or transport failures keep their existing fallback policy.
First-party names remain private-only. A backend that masks an access refusal as `404` supplies
no proof that authentication succeeded.

## Secrets

Secrets never live in the config document: client and registry tokens are always env vars. A
`codeArtifact` mirror target needs none, because Écluse mints its short-lived write token from the
container's ambient AWS credentials. The two non-minting tags take the `token` key under the target,
so a Verdaccio mirror write reads `ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN`. A
**mirrored** mount's worker writes with one credential, and a serve-only mount needs none unless it
declares a static publication token.

The secret-typed variables also accept the container-secret file pattern. Set the `_FILE` form
(`ECLUSE_SERVER__AUTH_TOKEN_FILE`, `ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN_FILE`,
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__CODE_ARTIFACT__TOKEN_FILE`) to a file path, and the file's
contents, with trailing newlines stripped, become the value, so the token never enters the
environment. Setting both a variable and its `_FILE` form, or naming an unreadable file, is a
fail-loud boot error.

A registry URL never carries a token either. Écluse refuses an endpoint written with userinfo
(`https://user:token@host/`), a query string, or a fragment at boot, and the error names the key.
The same refusal covers `server.publicUrl`, `advisories.osvExportBaseUrl`, `advisories.epssFeedUrl`,
and `queue.url`, and it is why the `config:` boot echo and `ecluse check-config` print each endpoint
in full. What Écluse does with a client's own token is under
[Edge authentication](@/docs/deployment.md#edge-authentication-and-client-credentials). The credential model is in
[Credential flow and authority](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#credential-flow-and-authority) and
[Outbound registry credentials](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#outbound-registry-credentials).

## Validation

Écluse validates the configuration in full at startup and refuses to start on any problem. An
unknown rule type, a bad URL, or an unresolved policy reference all stop the boot, so a
misconfiguration is a loud, immediate failure rather than a quietly mis-enforced policy.
`ecluse check-config` runs the same validation without starting anything, once for every role, so a
refusal only one command earns prints as a warning naming that command instead of failing the
check. It decides everything the configuration alone decides and makes no cloud call. The
validation model is in [Validation: fail fast, reject the unknown](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown).

`integrity.divergencePolicy` is a deprecated key with no effect: `warn` (in any case, with
surrounding whitespace), an absent value, and null all keep private preference and divergence
alarms, so remove the key and its environment variable. The value `fail-closed`
(also spelled `fail_closed` or `failclosed`) refuses startup, at the top level and under
`mounts.<ecosystem>.integrity`. Before you remove it, accept that a digest disagreement keeps the
private version and raises an alarm. Digest disagreement alone never removes a private version or
its tags, blocks a private artifact request, or authorises deletion. Independent integrity checks
still apply.

## Rule policy

The policy is a named map of rules over the deny-by-default gate described in
[The policy](@/docs/how-it-works.md#the-policy). It lives in the config document's `rules` object.
`ECLUSE_RULES` carries the same object as JSON, which suits a one-rule tweak, and the document
stays the reviewable home for a real policy.

Écluse ships eleven built-in rule types, catalogued below. A shipped name patches the rule it names,
`enabled: false` suppresses it, and a new name with a `type` adds a rule. A rule reads only its own
type's knobs, and a knob written under a type that does not read it fails the load. The table lists
the types from the highest default precedence to the lowest, so an earlier row wins over a later
one:

| Type | On by default | Description | Knobs |
|---|---|---|---|
| `DenyByIdentity` | No | Denies a package or `package@version`. This is the revoke. | `identity` |
| `DenyIfDeprecated` | No | Denies a version carrying the npm deprecation flag, including whole packages and release lines. | None |
| `DenyIfYanked` | No | Denies a yanked Cargo version or a PyPI release whose every file is yanked. | None |
| `DenyInstallTimeExecution` | npm only | `deny-scripts` denies npm install scripts at admission. Stored copies stay. | None |
| `DenyIfKnownMalicious` | With `advisories.url` | Denies affected versions named by an active primary `MAL-` advisory. Named `deny-known-malicious`, precedence 300, both phases. | `onUnavailable` |
| `AllowByIdentity` | No | Admits a package or `package@version`, and a bare name matches every version. It overrides scored advisory denies. Override MAL denial with precedence above 300. | `identity` |
| `DenyIfCve` | No | Denies a version that a synced advisory marks as affected at or above a CVSS threshold. | `minCvss` (0-10), `onUnavailable` |
| `DenyIfEpss` | No | Denies a version that a synced advisory marks as affected when its EPSS score meets a threshold. Ties with `DenyIfCve`. | `minEpss` (0-1), `onUnavailable` |
| `AllowScope` | No | Admits every version under an npm scope you trust, past the quarantine, after any higher-precedence advisory rule. | `scope`, without the leading `@` |
| `AllowIfRemediatesCve` | With `advisories.url` | Named `remediation-fast-track`, precedence 150. Admits the exact fixed version a synced advisory names, ahead of the quarantine. A fix that another advisory still affects passes only inside its limits. | `onUnavailable`, `remainingAdvisories`, `maxRemainingCvss` (0-10), `maxRemainingEpss` (0-1), `minCvssGap` (0-10), `minEpssGap` (0-1) |
| `AllowIfOlderThan` | Yes | Provided by default with the name `min-age`. Holds each new public version for the quarantine window, so a malicious publish is usually found and yanked before a build can install it. | `ageSeconds` (7 days by default) |

`DenyIfYanked` and `DenyIfDeprecated` are opt-in and default to precedence 300, above every default allow.
Both read typed flags. Refusals state the flag without quoting an upstream notice.
An empty npm `deprecated` string clears deprecation. A PyPI release with mixed yanked and available
files remains admitted as a whole unless another rule refuses it.

Limit either deny to admission to refuse new public admission and queued mirror writes while keeping
previously mirrored versions retrievable through trusted private reads:

```yaml
mounts:
  npm:
    rules:
      deny-deprecated:
        type: DenyIfDeprecated
        appliesTo: [admission]
  pypi:
    rules:
      deny-yanked:
        type: DenyIfYanked
        appliesTo: [admission]
```

Deprecation also marks obsolete packages and entire release lines. Enabling `DenyIfDeprecated`
can refuse all versions that a dependency range selects, not only isolated withdrawn releases.
The yank rule is the PyPI yank setting. There is no separate configuration key.
PyPI mirroring copies every supported wheel and source distribution admitted for a version.

Without `appliesTo`, either deny also applies at revocation. The Dredger reads the store's own
metadata, so a later public yank or deprecation does not update the stored flag by itself.
An admission-only rule never authorises deletion. Other revocation rules can still condemn the copy.
These flags do not detect a malicious release removed from registry metadata.
`DenyIfKnownMalicious` reads advisory evidence independently of these flags. A publisher
deprecates or yanks a first-party release at the publication target. Écluse answers
`npm deprecate` with `501`, and PyPI's upload protocol has no yank write. Écluse then serves the
result: a first-party npm version keeps its `deprecated` member, and a PyPI file keeps its
`yanked` status.

The npm mount adds `deny-scripts` at precedence 300 with `appliesTo: [admission]`.
The default applies only when you enable the npm mount. It does not enable a mount itself.
PyPI inherits only the shared defaults, so an admitted release keeps its source distributions and wheels.

Disable this default under `mounts.npm.rules`, or admit a named package with an `AllowByIdentity`
pin whose explicit `precedence` is above 300:

```yaml
mounts:
  npm:
    rules:
      deny-scripts:
        enabled: false
```

For a pin, keep the deny enabled and set:

```yaml
mounts:
  npm:
    rules:
      pin-native-build:
        type: AllowByIdentity
        identity: native-package@1.0.0
        precedence: 301
```

`ECLUSE_MOUNTS__NPM__RULES` carries the same patch as JSON, for example
`'{"deny-scripts":{"enabled":false}}'`. Disabling `deny-scripts` in the shared `rules` object
fails the load because that shared list defines no rule by this name.
Mount rules resolve over the shared policy, then the shipped mount rules, then your mount patch.
To extend the npm deny to stored copies, set
`deny-scripts: {appliesTo: [admission, revocation]}` under `mounts.npm.rules`.

Each adapter declares the rule types its ecosystem blocks, with a reason.
Boot and `ecluse check-config` refuse every blocked entry in each active mount's resolved policy.
A higher allow or an admission-only phase does not clear a blocked entry.
Disabled mounts and rules removed with `enabled: false` produce no block refusal.
The following types are blocked:

| Ecosystem | Blocked type | Reason |
|---|---|---|
| pypi | `DenyInstallTimeExecution` | Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release. |
| npm | `DenyIfYanked` | npm reports no yank flag. Removed releases disappear from its metadata. |
| pypi | `DenyIfDeprecated` | The PyPI adapter supplies no deprecation flag. |
| pypi | `AllowScope` | A PyPI project name has no scope, so this rule can never match one. |
| cargo | `DenyInstallTimeExecution` | Cargo's sparse index does not declare build scripts or procedural macros. This rule is unsupported for the ecosystem. |
| cargo | `AllowScope` | Cargo package names have no scope. |
| cargo | `DenyIfDeprecated` | Cargo's sparse index supplies no deprecation flag. |
| rubygems | `DenyInstallTimeExecution` | RubyGems native builds are normal installation behaviour. This rule is unsupported for the ecosystem. |
| rubygems | `DenyIfDeprecated` | RubyGems metadata supplies no supported deprecation flag. |
| rubygems | `AllowScope` | RubyGems package names have no scope. |

Move `DenyInstallTimeExecution`, `AllowScope`, or `DenyIfDeprecated` from shared `rules` into `mounts.npm.rules`:

```yaml
mounts:
  npm:
    rules:
      custom-code:
        type: DenyInstallTimeExecution
      custom-scope:
        type: AllowScope
        scope: acme
```

To keep a shared entry for npm, disable the same configured key under `mounts.pypi.rules`:

```yaml
rules:
  custom-code:
    type: DenyInstallTimeExecution
  custom-scope:
    type: AllowScope
    scope: acme
mounts:
  pypi:
    rules:
      custom-code:
        enabled: false
      custom-scope:
        enabled: false
```

`ECLUSE_MOUNTS__PYPI__RULES` takes that mount patch as JSON, for example
`'{"custom-code":{"enabled":false},"custom-scope":{"enabled":false}}'`.
The shipped npm default remains under `mounts.npm.rules.deny-scripts`.
PyPI's install-code signal and the served source distributions and wheels remain unchanged.

The advisory rules carry details the table leaves out:

- `remediation-fast-track` needs an advisory source to activate. Set `ECLUSE_ADVISORIES__URL` and
  run Pilot to sync one. [The remediation fast lane](@/docs/configuration.md#the-remediation-fast-lane)
  covers its choices and limits.
- Every advisory deny (`DenyIfKnownMalicious`, `DenyIfCve` and `DenyIfEpss`) needs an advisory
  store. A mount that enables one with `advisories.url` unset refuses the boot, whatever
  `onUnavailable` says, and `ecluse check-config` reports the same refusal. So does an
  `AllowIfRemediatesCve` under `onUnavailable: deny`.
- The npm malware feed carries no score, so `DenyIfCve` counts a malware entry as above every
  threshold. Enabling it also blocks known-malicious packages.
- EPSS is FIRST.org's estimate of the probability that attackers exploit a vulnerability within 30
  days. `DenyIfEpss` gates on that likelihood, where `DenyIfCve` gates on severity. A missing EPSS
  score makes the rule abstain for that advisory.
- `onUnavailable` decides what an advisory rule does when the advisory database cannot answer:
  `deny` or `skip`. An advisory deny defaults to `deny`. An `AllowIfRemediatesCve` rule states it.

Before you enable `DenyIfCve` or `DenyIfEpss`, read
[Onboarding the advisory denies](@/docs/configuration.md#onboarding-the-advisory-denies).

A `package@version` identity matches the version as Écluse keys it. For PyPI that is the canonical
PEP 440 spelling, which drops trailing zeros, so the release published as `1.0.0` is written `1`.

Precedence defaults per type, and an integer `precedence` overrides it. The policy below patches
`min-age`, suppresses the fast-track, and adds named rules:

```yaml
rules:
  min-age:
    ageSeconds: 1209600
  remediation-fast-track:
    enabled: false
  revoke-bad:
    type: DenyByIdentity
    identity: bad-package
  pin-fix:
    type: AllowByIdentity
    identity: left-pad@1.3.0
  deny-known-cves:
    type: DenyIfCve
    minCvss: 8
  deny-exploitable-cves:
    type: DenyIfEpss
    minEpss: 0.5
mounts:
  npm:
    rules:
      trust-our-scope:
        type: AllowScope
        scope: acme
```

The precedence values, the patch/add/suppress merge model, and the strict validation are in
[Rule policy](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#rule-policy) and
[Rules engine](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/rules-engine.md#evaluation-model).

### Where a rule applies

A new rule applies at two phases unless you limit it. The shipped npm `deny-scripts` applies only
at admission. Admission is the gate: a listing, an artifact request, and the mirror worker's check before it writes a version to your mirror target.
Revocation is the Dredger's check of the versions your stores already hold. The `appliesTo` list
on a rule names its phases:

| `appliesTo` | The rule is read by | What a deny does |
|---|---|---|
| Not set, or `[admission, revocation]` | The gate, the mirror worker, and the Dredger | Refuses new versions, and the Dredger removes stored copies |
| `[admission]` | The gate and the mirror worker | Refuses new versions. Stored copies stay, and your private stores keep serving them |

Limit a deny to admission to refuse new public admission and mirror writes while retaining
access to stored copies. New and existing clients can still install those trusted copies.
The example narrows the shared rule, then enables revocation for the npm mount:

```yaml
rules:
  deny-known-cves:
    type: DenyIfCve
    minCvss: 8
    appliesTo: [admission]
mounts:
  npm:
    rules:
      deny-known-cves:
        appliesTo: [admission, revocation]
```

A patch that does not restate `appliesTo` keeps the phases of the rule it patches, and a stated
list replaces them whole. `enabled: false` switches a rule off at both phases, whatever `appliesTo`
says beside it. In the environment the list rides inside the rule object, for example
`ECLUSE_MOUNTS__NPM__RULES='{"deny-known-cves":{"appliesTo":["admission","revocation"]}}'`.

The load refuses a setting it would have to guess at, and `ecluse check-config` reports the same
refusals:

| Setting | Why the load refuses it |
|---|---|
| `appliesTo: []` | It names no phase. `enabled: false` is how you switch a rule off |
| `appliesTo: [revocation]` | A deny the gate did not read would delete copies of versions the gate still admits, and the mirror worker would write them again |
| `appliesTo: [admission]` on an allow | The Dredger would not read the allow, so a lower deny could delete a version the gate admits. Limit the deny instead |
| An unknown word, an empty string, an empty value, or a value that is not a list | It names no phase Écluse has. An empty value does not read as both phases |

Every role's boot log, and `ecluse check-config`, print each mount's rules in evaluation order with
the phases each applies at. A `pypi` mount under the policy above, with the advisory store it needs,
keeps the shared `[admission]`, so it logs:

```txt
rule boot order for mount pypi:
rule 1: DenyIfKnownMalicious (precedence 300, applies at admission and revocation)
rule 2: DenyIfCve (precedence 225, applies at admission only)
rule 3: AllowIfRemediatesCve (precedence 150, applies at admission and revocation)
rule 4: AllowIfOlderThan (precedence 100, applies at admission and revocation)
```

The Dredger evaluates the rules that apply at revocation, in that order, and passes over the
rest. So a deny limited to admission cannot cause a revocation, and it does not stop the Dredger's
walk either: a lower deny that applies at revocation still decides the version.

## Known malicious packages and upgrades

Configuring `advisories.url` activates `deny-known-malicious` before any sync succeeds.
Source-free deployments retain their existing defaults and need no opt-out.
Existing deployments with a source gain MAL denial, first-generation readiness gating, and fixed
publication-age refusal. This selection does not depend on the local Pilot role or feed health.

Known means an active primary `MAL-` record in the accepted OSV source. The rule matches its affected
versions through the ecosystem's normal version ordering. It does not require CVSS or EPSS scores.
Aliases, summary text, yank flags, deprecation notices, and missing scores do not classify malware.
A replacement generation that withdraws a MAL record removes that record's authority. Independent
active records still decide. Deleted copies do not return merely because an advisory withdraws.

The default precedence is 300, above `AllowByIdentity` at 250 and below `DenyByIdentity` at 400.
An explicit identity allow above 300 can override it. Equal precedences retain rule-name ordering.
The default applies at admission and revocation. Limit deletion with `appliesTo: [admission]`.
Disable it globally or under a mount:

```yaml
rules:
  deny-known-malicious:
    enabled: false
```

This opt-out also works without a source. Partial knobs preserve the current activation state.
An explicit mount `enabled: true` can override a global disable without restating the type.
An explicit `type: DenyIfKnownMalicious` declaration activates the rule unless disabled.
Any manual activation without a source refuses boot and `check-config`, including under `skip`.
Disabling the shipped name does not remove a separately named manual MAL rule.

The rule ranks above every default allow. An expired or undated push refuses every public
admission on the mount, scope and identity allows included. Under the default `deny`, a faulted
read, an open breaker, or a missing first generation does the same. Only an allow ordered before
the rule still admits: one above 300, or one at 300 whose name sorts first. Trusted private reads
still serve. A deployment that set `onUnavailable: skip` on `DenyIfCve` or `DenyIfEpss` refuses
during an advisory outage unless it also sets `onUnavailable: skip` on `deny-known-malicious` and
`remediation-fast-track`.

Publish a valid artifact for each ecosystem before configuring its source. Roll admission and
writer roles forward before enabling Dredger revocation. Preview deletions and retain target-local
consent, first-party protection, maintenance support, and deletion caps. When relaxing a rule,
update Dredger first, then admission and writer roles. Trusted private copies keep serving until
eligible deletion completes. The rule applies the same way to npm, PyPI, Cargo, and RubyGems admission, mirror
writes, and Dredger revocation.

An absent generation or a faulted read follows `onUnavailable`, defaulting to `deny`.
`skip` permits lower rules to decide but never waives stale or undated generation refusal.
A failed poll retains the last-good generation until its publication age expires.
Missing or late MAL records remain an evidence gap. Use `DenyByIdentity` for an earlier finding.
The configured source can supply false positives. It remains a trusted authority, and the rule
adds no guarantee for client-held bytes or the accepted worker-transfer window.

## The remediation fast lane

Configuring `advisories.url` activates `remediation-fast-track`, on the same terms as
`deny-known-malicious`. `enabled: false` deactivates it, `enabled: true` or a restated `type`
activates it, and a partial patch keeps the current state. Without a source the rule is inactive,
so a source-free deployment boots and decides on the quarantine alone.

The rule admits a version a synced advisory names as its exact fix, ahead of the quarantine. A
version no advisory names as its fix gets no exception. A **partial fix** is an exact fix that
another advisory still affects. These keys decide the rest, and the shipped rule states each one:

| Key | Values | Shipped | What it decides |
|---|---|---|---|
| `onUnavailable` | `deny`, `skip` | `deny` | What the rule does while advisory evidence is unavailable |
| `remainingAdvisories` | `abstain`, `differential` | `differential` | Whether a partial fix can pass. `abstain` leaves it to the quarantine |
| `maxRemainingCvss` | A CVSS score, 0 to 10 | `6.9` | The CVSS every remaining advisory must stay at or below |
| `maxRemainingEpss` | A probability, 0 to 1 | `0.01` | The EPSS every remaining advisory must stay strictly below. `0.01` is one percent |
| `minCvssGap` | CVSS points, 0 to 10 | `1.0` | How far the highest fixed CVSS must exceed the highest remaining one. `0` removes the condition |
| `minEpssGap` | Probability points, 0 to 1 | `0` | The same for EPSS. `0.05` means five points, not a relative 5% |

A patch to `remediation-fast-track` inherits every key it leaves out. A newly named
`AllowIfRemediatesCve` rule must state `onUnavailable` and `remainingAdvisories`, and takes the
shipped limits for those it omits. Switching to `abstain` drops the inherited limits, and the load
refuses a limit written beside `abstain`. It also refuses an unknown value, a limit outside its
range, and a number that is not finite. Each refusal names the rule and the key.

In `differential` mode, every enabled condition must hold. A missing score that a condition needs
makes the rule abstain, because an unknown score is not zero. The conditions compare the highest
score on each side, so repeated advisory rows cannot change the outcome. The admission or the
abstention names the fixed and remaining advisories, the compared scores, and the failed conditions.

The comparison reads the scores the advisory artifact stores. Pilot stores the highest valid CVSS
vector score, or a qualitative label's upper limit when no vector exists. So the differential proves
no exact severity reduction, and no reduced exposure against the version a client already has. The
shipped numbers are chosen product defaults, not validated safety boundaries. Many PyPI advisories
carry no CVSS score, so the differential rarely admits a PyPI partial fix. A partial fix the rule
admits is mirrored and trusted like any admitted version. A later rescoring does not revoke it unless
a deny that applies at revocation decides. `differential` reads EPSS scores, so it makes EPSS
enrichment required ([When the EPSS feed fails](@/docs/configuration.md#when-the-epss-feed-fails)).

Under `deny`, the rule refuses a version it reaches while advisory evidence is unavailable: before
the first sync, after a failed or timed-out read, behind an open breaker, and on an expired or
undated push. During an advisory outage an old public version therefore cannot pass the quarantine
either. Only an allow that ranks above 150 still admits, such as a scope or identity allow. `deny`
also makes an advisory store mandatory and makes readiness wait for the first artifact. Under
`skip`, the rule abstains, lower rules decide, and an admission records the skipped check.
Unavailable evidence never supplies an exception, and the rule never authorises Dredger deletion.

## Onboarding the advisory denies

`DenyIfCve` and `DenyIfEpss` can break a cold deployment, because a freshly stood-up mirror
still needs historical versions your existing builds depend on, and an advisory may since have
covered them. Enable them *after* you warm your private mirror:

1. Leave both out of your policy and run Écluse normally, so your CI and developers pull the
   versions you depend on. Each lands in the trusted store, and Écluse serves it from there without
   gating it again at request time.
2. Once your must-have builds have mirrored, add `DenyIfCve` with a `minCvss` you are
   comfortable with. A threshold of 8 blocks high and critical CVEs. Unscored advisories satisfy
   every CVSS threshold. The source-configured MAL rule denies known MAL evidence independently
   of CVSS.
3. An `AllowByIdentity` pin at its default precedence of 250 overrides the scored `DenyIfCve`
   and `DenyIfEpss` rules. An intentional exception to `DenyIfKnownMalicious` needs explicit
   precedence above 300, for example `precedence: 301`. Use that exception only for a false
   positive or a risk you accept.

`ecluse dredger` checks stored versions against current revocation rules and removes eligible
copies selected by a decisive deny. Consent, first-party protection, backend failures, candidate
selection, and deletion caps can leave a copy in place. Follow the
[revocation procedure](@/docs/operations.md#revoking-a-mirrored-version-internal-yank) to verify both stores.
When Dredger runs, pin every version you must keep before you add a deny, and run
`ecluse dredger --dry-run` to see what it would remove. To refuse new versions and keep the warmed
ones, limit the deny to admission ([Where a rule applies](@/docs/configuration.md#where-a-rule-applies)).

Add `DenyIfEpss` alongside `DenyIfCve`, not instead of it. EPSS estimates exploitation probability,
not severity or proof of exploitation. An individual missing score makes EPSS abstain, including
malware without a CVE alias. `DenyIfCve` still denies on missing CVSS scores. Other affecting scored
advisories and other rules still apply. If all rules abstain, deny-by-default refuses admission.

A valid feed can lose a score or contain a malformed row the parser skips. The EPSS rule then stops
denying that advisory, and another allow can admit the version to public clients and the mirror.
This does not establish that the version is harmless. Missing EPSS alone cannot authorise Dredger
deletion. A different decisive deny can still authorise deletion under the existing guards.

Set `onUnavailable: skip` to let another allow decide when an advisory lookup is unavailable.
The default `deny` refuses instead. This also applies to mirror admission: a skipped check can
precede an admission that remains trusted after the lookup recovers. Removing the allow later
does not revoke that copy. A named deny at revocation must become decisive, and Dredger must
successfully delete every eligible serving copy. Without working, authorised Dredger cleanup,
that exposure can persist indefinitely. Absent consent, first-party protection, or a winning allow
can prevent deletion. A mirror deletion alone does not revoke a private-cache copy. See
[Revoking a mirrored version](@/docs/operations.md#revoking-a-mirrored-version-internal-yank).

A skipped check leaves evidence with the admission. The decision names the rule and the cause, and
the public artifact gate logs one `warn` line per skipped check, carrying the package, the version,
the rule, and the cause. The line is bounded: once per package, version, and skipped rule set for
the life of the advisory outage, cleared when the source recovers, so repeated public serves of one
version during an outage log it once. A trusted read of a mirrored version runs no rules and logs
nothing. A check the winning allow pre-empted is recorded as not reached, never as passed, and
logs no line. The mirror worker also logs the checks skipped during its own re-admission, once
per skipped check per successful re-admission. A job attempt can re-admit more than once as its
expected inventory expands. Retries can also repeat this evidence.
A skipped check never grants an allow by itself, and the worker still verifies artifact integrity. The evidence lives in that decision and in the log only: Écluse writes no metadata
to the mirror, and the log's retention is yours. The outage itself is reported apart from any
request, at `error`, as [Advisory outages](@/docs/operations.md#advisory-outages) describes.

`onUnavailable: skip` does not waive the maximum push age. Once the serving artifact is older
than that maximum, every advisory deny refuses, and `AllowIfRemediatesCve` follows its own
`onUnavailable`: the shipped `deny` refuses too. See
[Advisory push age](@/docs/operations.md#advisory-push-age) for the maximum, its default
derivation, and the alarm that precedes it. Individual missing scores differ from whole-feed
failure, which the rest of this section covers.

### When the EPSS feed fails

Each ecosystem whose resolved policy holds a rule that reads EPSS scores requires
`epss_status=available` in its artifact metadata. Those rules are an active `DenyIfEpss` and an
active `AllowIfRemediatesCve` in differential mode, the shipped `remediation-fast-track` included.
The requirement follows inherited rules and mount additions. Removed rules create no requirement.
`onUnavailable: skip` and a threshold of 1 still require the marker.
The consumer rejects missing, unavailable, or unrecognised markers before installation.
A successful marker establishes whole-feed enrichment, not an individual score or a new timestamp.
Optional feed dates and individual scores can remain absent.

Pilot applies the same requirement when it compiles. It attempts the EPSS feed on every compile,
whatever the rules. When the feed fails:

| The ecosystem's resolved policy | What Pilot does |
|---|---|
| Has a rule that reads EPSS scores | Fails the compile and publishes nothing. Consumers keep their last accepted artifact. |
| Has no rule that reads EPSS scores | Publishes the OSV data with `epss_status=unavailable` and no scores, so new advisories and their fixes still reach consumers. |

The publication that loses the scores may shrink by up to `advisories.epssLossCapPercent` beyond the
usual allowance ([Advisory publication guard](@/docs/operations.md#advisory-publication-guard)).

An artifact without the marker, or with `epss_status=unavailable`, serves only ecosystems with no
EPSS-dependent rule.
A running consumer retains its accepted qualified generation after rejection, subject to its existing maximum age.
A restarted consumer starts with an empty slot and does not recover the local canonical file.

Pilot reads the requirement from its own loaded configuration. It cannot see what a consumer runs,
so give Pilot, the proxy, the mirror worker, and Dredger the same rule configuration. If Pilot's
copy lacks a consumer's EPSS-reading rule, Pilot can publish an artifact that consumer refuses, and
the consumer keeps serving its last accepted one until that one ages out. A one-shot `pilot compile`
with no `advisories.url` requires the feed: no source activates the shipped remediation rule, so the
compile cannot tell which rules its consumers run. Pilot's boot log and `ecluse check-config` print
this requirement for each mount. Any other role prints it only when it reads an artifact from
`advisories.url`.

To add a `DenyIfEpss` to a running deployment:

1. Add the rule to Pilot's configuration first, so Pilot requires the feed for that ecosystem.
2. Confirm that Pilot published an artifact with `epss_status=available`. The compile's
   `Compiled ... epss_status=available` log line shows it, and so does a run of
   `ecluse pilot compile --ecosystem <ecosystem> --out DIR --upload` that exits `0`.
3. Roll the same configuration out to the proxy, the mirror worker, and Dredger.
