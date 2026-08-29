+++
title = "Configuring Écluse"
description = "Where every Écluse setting lives, which layer wins, where secrets go, and how the rule policy decides what a build may install."
weight = 4
+++

Every choice Écluse makes at the gate traces back to a setting on this page. The binary embeds
every default, so you write down only what you change: a wider quarantine, a mirrored mount, a
policy of your own. Start with where a setting lives, because there are two places and one always
wins.

## Two layers, one spelling rule

Configuration has two layers. **Environment variables** carry process and secret values. An
optional **config document** (YAML) carries the two things flat variables express badly: the rule
policy and the mount map. A value resolves as defaults < config document < environment variable,
so the environment wins. The boot log carries one `config:` line per resolved key, naming the layer
that supplied it and redacting secrets, and `ecluse check-config` prints the same dump.

> **One spelling rule.** Environment variables are the mechanical transliteration of the document
> schema: `__` descends into an object and `_` joins a camelCase word. So `ECLUSE_CACHE__MAX_BYTES`
> spells `cache.maxBytes`, and `ECLUSE_MOUNTS__NPM__MIRROR_TARGET` spells `mounts.npm.mirrorTarget`.

Mounts are off until you declare them. Mentioning one anywhere, whether through an
`ECLUSE_MOUNTS__<ECOSYSTEM>__*` variable or a key under `mounts.<ecosystem>` in the document,
switches it on. Declaring `mirrorTarget` then makes the active mount **mirror**, and a mirrored
mount requires its private upstream, so the mirror reads back. Omit `mirrorTarget` and the mount is
serve-only. Each boot logs one posture line per mount and warns on any pair of a mount's endpoints
that resolve to the same registry. The design rationale is in
[Configuration and authentication](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#configuration).

Écluse also reads three ordinary AWS-SDK variables from the process environment. They are not
document keys, and each one has a deliberately narrow reach:

| Variable | Affects | Never affects |
|---|---|---|
| `AWS_REGION` | The S3 advisory client, and SQS only under an `AWS_ENDPOINT_URL_SQS` override (a real SQS URL carries its own region) | CodeArtifact |
| `AWS_ENDPOINT_URL_SQS` | The SQS endpoint, and it forces the SQS reading of `queue.url` | S3 |
| `AWS_ENDPOINT_URL` | The S3 advisory client | SQS |

Both endpoint values get the same hygiene as every other URL: whitespace is trimmed, and a value
with userinfo, a query, a fragment, or a malformed port fails the boot. The error names the
variable but never the value, because the value can carry a credential.

## The configuration reference

The binary embeds the defaults below, and every key appears with its default and its meaning:

{{ config_reference() }}

## The configuration document

The document is a YAML file at `/etc/ecluse/config.yaml`. `ECLUSE_CONFIG` relocates it, and is the
one process-level setting with no document key. With `ECLUSE_CONFIG` set, a missing file is a boot
error, but an absent document at the default path is fine. The document carries only what you
change: the **rule policy** (see [Rule policy](@/docs/configuration.md#rule-policy)) and, for
multi-mount deployments, the **mount map**. A single-mount npm deployment on the default policy
needs no document at all. The schema is the
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
upstream with the gated public registry, but it never writes, so `queue` goes unread. Delete
`privateUpstream` as well and you are back to the pure public gate of the
[quick start](@/docs/quick-start.md), in document form. `enabled: true` is then the only key it
needs, because `publicUpstream` already has a default.

No token appears above, because Écluse mints the mirror-target write credential from the
CodeArtifact host. Every other secret is an environment variable.

## Secrets

Secrets never live in the config document: client and registry tokens are always env vars. A
CodeArtifact mirror target needs none, because Écluse mints its short-lived write token from the
container's ambient AWS credentials. Any other mirror-target host needs
`ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN`. A **mirrored** mount therefore holds one write
credential, and a serve-only mount never writes, so it holds none.

The secret-typed variables also accept the container-secret file pattern. Set the `_FILE` form
(`ECLUSE_SERVER__AUTH_TOKEN_FILE`, `ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN_FILE`,
`ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN_FILE`) to a file path, and the file's contents, with
trailing newlines stripped, become the value, so the token never enters the environment. Setting
both a variable and its `_FILE` form, or naming an unreadable file, is a fail-loud boot error.

A registry URL never carries a token either. Écluse refuses an endpoint written with userinfo
(`https://user:token@host/`), a query string, or a fragment at boot, and the error names the key.
The same refusal covers `server.publicUrl`, `advisories.osvExportBaseUrl`, and `queue.url`, and it
is why the `config:` boot echo and `ecluse check-config` print each endpoint in full. What Écluse
does with a client's own token is under
[Edge authentication](@/docs/deployment.md#edge-authentication-and-client-credentials). The credential model is in
[Credential flow and authority](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#credential-flow-and-authority) and
[Outbound registry credentials](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#outbound-registry-credentials).

## Validation

Écluse validates the configuration in full at startup and refuses to start on any problem. An
unknown rule type, a bad URL, or an unresolved policy reference all stop the boot, so a
misconfiguration is a loud, immediate failure rather than a quietly mis-enforced policy.
`ecluse check-config` runs the same validation without starting anything. The validation model is
in [Validation: fail fast, reject the unknown](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown).

## Rule policy

The policy is a named map of rules over the deny-by-default gate described in
[The policy](@/docs/how-it-works.md#the-policy). It lives in the config document's `rules` object.
`ECLUSE_RULES` carries the same object as JSON, which suits a one-rule tweak, and the document
stays the reviewable home for a real policy.

Écluse ships seven built-in rule types, catalogued below. A shipped name patches the rule it names,
`enabled: false` suppresses it, and a new name with a `type` adds a rule:

| Name | Type | On by default | What it decides | Key knobs |
|---|---|---|---|---|
| `min-age` | `AllowIfOlderThan` | Yes | Admits public versions older than the quarantine window, the core defence against race-to-publish typosquatting and dependency confusion. | `ageSeconds` (7 days by default) |
| `remediation-fast-track` | `AllowIfRemediatesCve` | Yes | Admits a release a synced advisory names as its exact fixed version ahead of the quarantine, provided no other advisory still affects it. Abstains until a first advisory database syncs (set `ECLUSE_ADVISORIES__BUCKET` and run Pilot), so without one only the quarantine governs. | (none) |
| yours to add | `AllowByIdentity` | No | Admits a specific package or `package@version` past the quarantine. Sits at the top of the allow band but still below every deny. | `identity` |
| yours to add | `DenyByIdentity` | No | Hard-denies a specific package or `package@version` (the `revoke` shape). | `identity` |
| yours to add | `DenyInstallTimeExecution` | No, because many legitimate packages ship install scripts | Denies install-time code execution. | (none) |
| yours to add | `DenyIfCve` | No | Blocks a version a synced advisory records as affected at or above the CVSS threshold. The npm malware feed carries no score and counts as above every threshold, so enabling it also blocks known-malicious packages. Sits just below `AllowByIdentity`, so an identity pin overrides it. | `minSeverity` (0-10). `onUnavailable` (`deny` by default, or `skip`) decides what happens when the advisory database cannot answer. |
| yours to add | `DenyIfEpssExceeds` | No | Blocks a version a synced advisory records as affected when that advisory's EPSS score is at or above the threshold. EPSS is FIRST.org's estimate of the probability a vulnerability is exploited in the wild within 30 days, so this gates on likelihood where `DenyIfCve` gates on severity. An advisory with no EPSS score counts as above every threshold. Shares `DenyIfCve`'s precedence. | `minEpss` (0-1). `onUnavailable` as for `DenyIfCve`. |

Before you enable `DenyIfCve` or `DenyIfEpssExceeds`, read
[Onboarding the advisory denies](@/docs/configuration.md#onboarding-the-advisory-denies).

Precedence defaults per type, and an integer `precedence` overrides it. The policy below patches
`min-age`, suppresses the fast-track, and adds the other four by name:

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
  deny-exploitable-cves:
    type: DenyIfEpssExceeds
    minEpss: 0.5
```

The precedence values, the patch/add/suppress merge model, and the strict validation are in
[Rule policy](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#rule-policy) and
[Rules engine](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/rules-engine.md#evaluation-model).

## Onboarding the advisory denies

`DenyIfCve` and `DenyIfEpssExceeds` can break a cold deployment, because a freshly stood-up mirror
still needs historical versions your existing builds depend on, and an advisory may since have
covered them. Enable them *after* you warm your private mirror:

1. Leave both out of your policy and run Écluse normally, so your CI and developers pull the
   versions you depend on. Each lands in the trusted store, which the rules never re-gate once the
   version is there.
2. Once your must-have builds have mirrored, add `DenyIfCve` with a `minSeverity` you are
   comfortable with. A threshold of 8 blocks high and critical CVEs, and malware blocks regardless
   of the threshold.
3. If Écluse then denies a specific version you must keep, pin it with an `AllowByIdentity` rule,
   which outranks both. That covers a false positive or a risk you accept.

Add `DenyIfEpssExceeds` alongside `DenyIfCve`, not instead of it. It reads the same advisory
database and denies on exploitability rather than severity, so it catches a merely moderate CVE
that attackers are actually using. Because an advisory with no EPSS score counts as above every
threshold, and most npm advisories carry no CVE alias for EPSS to key on, a low `minEpss` is not a
gentler gate than `DenyIfCve`: expect it to deny about as much.

Set `onUnavailable: skip` if you would rather a gate fail open (skip itself, logging loudly) than
refuse traffic when the advisory database is briefly unavailable. The default `deny` fails closed.
