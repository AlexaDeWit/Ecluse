+++
title = "Running the Dredger"
description = "The role that deletes mirrored versions your rules now deny: what one cycle does, what bounds it, and the permissions it needs."
weight = 6
+++

`ecluse dredger` is the only role that deletes. It walks each mount's mirror store and removes
versions the mount's own rules now deny. Run it when your mirror must not keep serving a version a
new advisory condemns, and read this page before you point it at a store, because deletion is
permanent.

The current Dredger targets the mirror repository only. A CodeArtifact private read repository
can retain another copy after mirror deletion. Follow
[the revocation procedure](@/docs/operations.md#revoking-a-mirrored-version-internal-yank) to
account for those retained copies. Automated cleanup of both locations is planned in
[#1227](https://github.com/AlexaDeWit/Ecluse/issues/1227), not implemented.

Dredger refuses to boot when a mount's `privateUpstream` and `publicationTarget` name the same
registry. Preview applies the same refusal. The private read cache must remain separate from
user publications. Distinct repository paths on one host remain valid. Proxy and mirror still
accept this same-mount pair.

The Dredger takes no ingress. It exposes only `/livez` and `/readyz` on `ECLUSE_SERVER__PORT`.

## What one cycle does

A mirrored version's metadata never changes after it is published. So a version Écluse once
admitted can become a denied one for only three reasons: a new or changed advisory, an operator
identity deny, or a change to your rule configuration.

The **default cycle** covers the first two, and it is what runs unless you turn the full walk on.
Each cycle:

1. Reads the store's consent marker and its classification. Both are read again every cycle, so
   withdrawing the marker on a `codeArtifact` store stops the next cycle with no restart. On a
   `verdaccio` store consent is a configuration key, so withdrawing it takes a restart. Where a
   rule reads the advisory database, the first cycle also waits for the first advisory sync, for
   at most one `cyclePause`.
2. Lists the store's package names, a page at a time.
3. Keeps only the names the synced advisory database covers, plus the names an identity-deny rule
   pins. Both sides are read through the ecosystem's own name parser, so a spelling difference
   between the advisory database and the store cannot miss a match.
4. Reads each of those packages' metadata **back from the store**, one read per package, and
   evaluates every version the store holds against the mount's whole rule set.
5. Deletes only what a named decisive deny condemns.

A cycle reads listings page by page and metadata for candidate names. A newly covered package can
wait until the next cycle if its name was absent from the current candidate set. Store failures,
consent, and the cap can delay or prevent deletion. A metadata failure narrows the facts a rule
has to decide on rather than stopping the package.

Set the pace with the `dredger` group in your configuration. `chunkSize` and `chunkPause` set how
many packages one chunk examines and how long it waits between chunks. `cyclePause` sets the wait
between cycles.

Chunk progress carries across listing pages, prefix buckets, and mounts within a cycle.
The pause comes before examining the next package after a completed chunk, never after the last package.

`chunkPause` has a floor of two seconds, and the Dredger refuses to boot beneath it, naming the key,
your value, and the floor. The pause is what leaves you time to stop a mistaken sweep, so you may
raise it and never lower it.

## What is deleted, and what never is

A version is deleted **only** on a named decisive deny. Everything else keeps it:

| The rules said | What happens |
|---|---|
| A named rule denies the version | Deleted |
| No rule was decisive (deny by default) | Kept |
| A rule could not be evaluated | Kept |
| A higher-precedence rule reads a fact this cycle could not | Kept |

Deny by default is how the serve path refuses an unknown version, and it is the right answer
there. Here it keeps, because your mirror may hold the only surviving copy of a version the public
registry has already removed.

Each rule is evaluated against the facts the cycle holds. The store's listing establishes the
package name and version on its own, so a rule that reads only identity decides even when the
package metadata is unreadable or omits that version. That covers `DenyByIdentity` and
`AllowByIdentity`, and the advisory denies as well, because identity and the advisory database are
all they read. A rule that reads anything further, such as the publish age or the install-time
execution signal, cannot decide without that metadata: the evaluation stops there and the version
is kept. Precedence still governs, so an undecided rule above a deny keeps the version rather than
letting the deny remove it. Missing metadata on its own never deletes anything.

The first-party belt shields every version under a namespace your `firstParty` key names, and the
Dredger never even reads their metadata.

Removing an allow is not itself a deny. A version stays when no named rule condemns it, including
during a full walk. If removing an override exposes an existing winning deny, normal pruning
applies.

## Consent, and what the store is

The Dredger deletes from a store only when that store carries the operator's own consent marker,
and only when deleting from it destroys something. It reads both at the start of every cycle,
through the store backend's own handle.

| Store tag | How you attach consent | How you withdraw it |
|---|---|---|
| `codeArtifact` | a repository resource tag, key `ecluse-dredger-consent`, value `true` | remove the tag, and the next cycle halts with no restart |
| `verdaccio` | `permitDeletion: true` under the mirror target's tag | unset the key and restart, because the boot reads it |
| `registry` | no consent form and no control plane, so the Dredger refuses the store at boot and names the tag | |

The Dredger never writes a consent marker. Placing one and removing it are yours alone, and the
full walk's resumption marker is a separate tag key so a marker write cannot reach your consent.
`ecluse dredger --dry-run` boots without consent on either backend and reports what it found
instead ([Preview](#preview)).

A store classified as able to refill from an upstream is not swept. Deleting its local copy does
not prevent another upstream fetch from recreating it, and the current cycle halts on that
classification. The halt line names the backend.

The Dredger also applies the [endpoint collision checks](@/docs/configuration.md#endpoint-collisions).
Their comparison scope differs by endpoint role. A shared host alone is not a registry collision,
except for the explicit public-host safeguards.

## The deletion cap

`deletionCap` bounds how many versions one cycle may hand over for deletion. It is the breaker
against an advisory database that denies far more than it should.

Left unset, it is computed at boot as 100 per sweepable mirror store, because one cycle covers
every store in turn. That default is deliberately small. Preview first: a dry run reports the
count a real sweep would reach, which is the number to write into `deletionCap`.

Reaching it **halts the Dredger for the life of the process**, whether or not there was more it
would have deleted. No further cycle runs, the readiness probe answers `503`, liveness stays
healthy, and an error line repeats at each cycle interval naming the advisory generation and the
count. The process stays up on purpose: exiting would bring a pod restart, and the restart would
begin sweeping the same poisoned generation again.

Investigate the generation that filled the cap. Then either restart the Dredger, or raise the cap
deliberately and restart it.

## The full walk

A rule-configuration change is the one cause a default cycle cannot see, because no advisory and no
identity deny points at the versions it newly denies. The full walk covers it. Turn it on with
`fullWalk: true`.

While it is on, the walk **replaces** the default cycle rather than running beside it, because a
walk over every name is a superset of a candidate cycle. Each completed walk starts a fresh one.
Turn it off once a walk has completed, and the Dredger drops back to candidate cycles with nothing
lost.

The walk covers the name space in prefix buckets, and records each completed bucket in the store
itself, so a restart re-does at most one bucket. A bucket holding more names than the walk may hold
at once is split into narrower ones; where no narrower bucket divides them, the cycle halts naming
that bucket rather than skipping it. **Enabling the full walk is also your decision to
let the Dredger write one thing to your store.** A `codeArtifact` store keeps the record in one
repository tag per ecosystem. A `verdaccio` store has nowhere to keep one, so a walk over it starts
from the beginning after every restart, and the Dredger says so at boot.

Advisory latency during a walk is bounded by the walk's own pace, so do not leave it on
indefinitely.

## Preview

`ecluse dredger --dry-run` boots a role of its own. It is built from each backend's observing calls
alone, so it holds no delete and no walk-marker write: the run cannot delete, because nothing it
holds can.

A preview needs no deletion consent. It reads the consent marker and the store classification,
reports each one per target, and keeps enumerating either way. Those findings print above the
counts, because a count says what your rules reach, never that this deployment may delete.
Everything else the deleting role needs still applies: the endpoint collision checks, the mirror
target's own parsing, a backend this build can sweep, and the credential the store answers to.

A full walk under a preview starts at the first bucket every time. It neither reads nor replaces
the marker a real walk records, so a preview leaves a walk in progress where it was.

The cap applies as logging only: passing it writes one line naming where a real run would have
halted, and the preview counts on, so its closing tally reports the full reach. Its counter is
`would_delete`, never `deleted`.

Under `--once` the exit status follows completeness alone:

| What the preview did | Exit |
|---|---|
| Read the whole store, and every fact the rules that decided it needed | `0`, whatever the prerequisites said |
| Stopped on a store fault, or decided without an advisory generation or a package's own metadata | non-zero, with partial counts and every candidate it did gather |

Exit `0` means complete, not authorised. A preview that exits `0` with the consent marker absent
has told you both what a real sweep would remove and that a real sweep would refuse to start.

Use it before the first real sweep of a store, and after any rule change you are unsure of.

## `--once`

`ecluse dredger --once` runs one cycle and exits. It exits `0` when the cycle completed and `1`
when it halted, with the reason on the same line, so a scheduler reads the outcome from the status.
It composes with `--dry-run`, whose status follows completeness rather than permission, as the
[preview](#preview) describes. A preview does not prove that the credentials can perform real
deletion.

A **cycling** Dredger reports nothing through its exit status. It stops when it is asked to,
whatever its last cycle did, so a restart-on-failure supervisor does not resume dredging on its
own after a halt.

## What the Dredger tells you

Every deletion writes a line naming the package, version, denying rule, and advisory generation
marker. A decision without a loaded database records `none`. During a concurrent database swap,
that marker can differ from the lookup used by the rule.
[#1204](https://github.com/AlexaDeWit/Ecluse/issues/1204) tracks the attribution correction.

Whatever stops a cycle repeats an error line at **each cycle interval** until it clears or an
operator restarts the Dredger. Nothing halts silently. That covers a withheld consent marker, a
store that refills itself, a store that stopped answering, and the latched deletion cap.

A store fault is retried once after the delay the fault itself advises. That retry logs at `WARN`,
because it may clear on its own. A fault that survives it halts the cycle and logs at `ERROR`. The
next cycle re-attempts, so an outage reports once per cycle interval for as long as it lasts and
the sweep resumes on its own when the store answers.

A delete the backend refuses, or one that never reached it, leaves the version in the store. It
counts as kept and writes an error line carrying the backend's own code and message.

The `ecluse.dredger.versions` counter carries one label, `result`, one of `examined`, `deleted`,
`would_delete`, `kept`, or `guard_skipped`. Every version a cycle examines counts once as
`examined` and once more under what the cycle did with it. **Alert on a jump in `deleted`.**

## Permissions

Scope the Dredger to its configured mirror. Token minting does not grant repository access by
itself. The CodeArtifact role needs these permissions for a default candidate cycle:

| Action | Resource scope | Purpose |
|---|---|---|
| `codeartifact:GetAuthorizationToken` | Domain ARN | Mint the repository token |
| `sts:GetServiceBearerToken` | `*` in the role's identity policy, restricted by `sts:AWSServiceName = codeartifact.amazonaws.com` | Permit token minting |
| `codeartifact:ListPackages` | Mirror repository ARN | Enumerate package names |
| `codeartifact:ListPackageVersions` | Package ARNs within the mirror | Enumerate versions |
| `codeartifact:DescribeRepository` | Mirror repository ARN | Read store classification |
| `codeartifact:ListTagsForResource` | Mirror repository ARN | Read consent and cursor tags |
| `codeartifact:ReadFromRepository` | Mirror repository ARN | Read package metadata for rule evaluation |
| `codeartifact:DeletePackageVersions` | Package ARNs within the mirror | Delete selected versions |

The mirror worker needs the same token-mint permissions, repository reads for its presence probe, and
`codeartifact:PublishPackageVersion` on package ARNs. It does not need Dredger's deletion grant.
On CodeArtifact, Dredger does not need publication permission. See the
[action/resource reference](https://docs.aws.amazon.com/service-authorization/latest/reference/list_codeartifact.html)
and [token requirements](https://docs.aws.amazon.com/codeartifact/latest/ug/tokens-authentication.html).

The full walk also needs cursor-write permissions:

- `codeartifact:TagResource` and `codeartifact:UntagResource` on the mirror repository ARN,
  conditioned on the key family the Dredger writes:

```json
"Condition": {
  "Null": { "aws:TagKeys": "false" },
  "ForAllValues:StringLike": { "aws:TagKeys": "ecluse-dredger-cursor-*" }
}
```

`ForAllValues` is required. `aws:TagKeys` is multivalued, and `ForAnyValue` would admit a request
that also carried the consent tag key. `TagResource` adds and updates the keys it names and
replaces no others, so a marker write cannot disturb your consent tag. Granting neither action
leaves the consent tag outside the Dredger's reach entirely.

On a store reached through the ecosystem protocol alone, least privilege is an account of the
store's own: a user whose package rights cover the mirror store and nothing else.

## Known limits

**One Dredger per store.** There is no lease. Two Dredgers against one store double the cap's blast
radius and interleave their marker writes. On a `verdaccio` store, unpublish deletes the whole
package when its last version is removed. Otherwise it edits the package document and deletes
the version's tarball. Verdaccio does not enforce the document revision on these writes, so
either path can lose a concurrent publish of another version of the same package.

**Pre-declaration public copies remain served and protected from Dredger.** Before declaring a
namespace first-party, review its existing copies in both the mirror and the private read
repository. Distinguish public-derived copies from genuine private releases and remove only the
unwanted versions you identify. Apply the declaration to every role, then verify both stores again
after older writes settle. Dredger cannot remove shielded leftovers for you, even under an identity
deny. Do not delete the whole namespace merely because it now has first-party status.

**A store that answers a metadata read with an error decides its package on identity alone.**
The read distinguishes an absent document (`404`) from other HTTP failures and decode failures.
An absent document carries no retry advice. `408`, `429`, and server errors carry retry advice.
The current manifest consumer does not use that advice. It reads the manifest again on the next
cycle, without a retry within the current cycle, and until then every rule reading more than
identity leaves the package's versions in place.

**An advisory swap does not give the whole bucket one immutable rule snapshot.** Candidate names
come from the bucket's acquired database, while each version's rule evaluation can see a newer
generation. A newly covered name absent from the candidate set waits for a later cycle.

**A cycle needs no advisory database.** With an advisory rule active and no generation loaded, the
advisory half of the candidate set is empty, the identity half still sweeps, and the cycle writes
one error line saying the advisory half is unavailable.
