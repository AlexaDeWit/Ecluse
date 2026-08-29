+++
title = "Configuring Écluse"
description = "The environment layer, the configuration document, the secrets, and the rule policy Écluse applies to a public version."
weight = 4
+++

This page covers the two configuration layers, the configuration document, the secrets, and
the rule policy.

## Two layers, one spelling rule

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
[Configuration and authentication](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#configuration).

Three ambient AWS-SDK variables are read from the process environment and are not document keys.
`AWS_REGION` scopes SQS only under an `AWS_ENDPOINT_URL_SQS` override (a real SQS URL carries its
own region) and the S3 advisory client, never CodeArtifact. `AWS_ENDPOINT_URL_SQS` overrides the
SQS endpoint and forces the SQS interpretation of `queue.url`. `AWS_ENDPOINT_URL` overrides the S3
advisory client only, never SQS. Neither endpoint value may carry userinfo, a query, or a fragment,
and neither may write a malformed port. Surrounding whitespace is trimmed before parsing. Écluse
refuses such a value, and either variable then fails the boot. The refusal names the variable and
never the value, which can carry a credential.

## The configuration reference

The defaults the binary embeds, with every key, its default, and its meaning:

{{ config_reference() }}

## The configuration document

A YAML file at `/etc/ecluse/config.yaml`. `ECLUSE_CONFIG` relocates it, and is the one
process-level setting with no document key. With it set, a missing file is a boot error. At the
default path an absent document is fine. The document carries only what you change: the **rule
policy** (see [Rule policy](@/docs/configuration.md#rule-policy)) and, for multi-mount deployments, the **mount map**. A
single-mount npm deployment on the default policy needs none. The schema is the
[embedded default](@/docs/configuration.md#the-configuration-reference) above, and an unknown key anywhere in the
document is a boot error.

A worked document for a mirrored npm deployment. Reads resolve against a private CodeArtifact
endpoint, approved public packages mirror into a separate CodeArtifact store, and the quarantine
widens to fourteen days. That distinct read endpoint and mirror store is the
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
upstream with the gated public registry, it never writes, and `queue` then goes unread. Delete
`privateUpstream` as well and the mount is the pure public gate of the
[quick start](@/docs/quick-start.md) in document form. `enabled: true` is then the only key it needs,
because `publicUpstream` already has a default.

No token appears above. Écluse mints the mirror-target write credential from the CodeArtifact host.
Every other secret is an environment variable.

## Secrets

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
[Edge authentication](@/docs/deployment.md#edge-authentication-and-client-credentials). The credential model is in
[Credential flow and authority](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/registry-model.md#credential-flow-and-authority) and
[Outbound registry credentials](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#outbound-registry-credentials).

## Validation

Écluse validates the configuration in full at startup and refuses to start on any problem. An
unknown rule type, a bad URL, or an unresolved policy reference all stop the boot. A
misconfiguration is then a loud, immediate failure rather than a quietly mis-enforced policy.
`ecluse check-config` runs the same validation without starting anything. The validation model is
in [Validation: fail fast, reject the unknown](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#validation-fail-fast-reject-the-unknown).

## Rule policy

The policy is a named map of rules over the deny-by-default gate described in
[The policy](@/docs/how-it-works.md#the-policy). It lives in the config document's `rules` object. `ECLUSE_RULES`
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
  [Onboarding DenyIfCve](@/docs/configuration.md#onboarding-denyifcve) before enabling.

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
[Rule policy](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md#rule-policy) and
[Rules engine](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/rules-engine.md#evaluation-model).

## Onboarding DenyIfCve

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
