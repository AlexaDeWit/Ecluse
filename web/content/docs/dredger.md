+++
title = "Running the Dredger"
description = "The role that deletes denied versions from your mirror target and its private cache: what one cycle does, what bounds it, and the permissions it needs."
weight = 6
+++

`ecluse dredger` is the only role that deletes. It walks each mount's mirror target and the
`privateUpstream` cache paired with it, and removes versions the mount's own rules deny. Run it when
those stores must not keep serving a version a new advisory condemns, and read this page before you
point it at a store, because deletion is permanent.

`privateUpstream` is a cache. Your builds read through it, the mirror worker writes to
`mirrorTarget`, and your publishers write to `publicationTarget`. Dredger cleans the first two and
never the third. Because the cache only holds copies of what those stores already carry, Dredger
needs no proof that a cached version came from the mirror: an unknown or absent origin does not
shield a copy. Pointing publishes at a store you also declare as `privateUpstream` breaks that
premise, so treat it as a deployment fault rather than a supported topology.

Dredger reads actual inventories from each mount's mirror target and private cache independently.
It removes eligible mirror versions before their eligible cache copies, under separate consent.
It preserves a source that policy keeps, even when that source can refill the cache.
Restarted and later cycles rediscover cache-only residuals without a pending-work ledger.
`--dry-run` evaluates both inventories but constructs no deletion or cursor-write capability.

Dredger refuses to boot when a mount's `privateUpstream` and `publicationTarget` name the same
registry. Preview applies the same refusal. Dredger is destructive, and a first-party package can
exist only in the publication target, so Dredger never runs against a store that also receives
publications. Distinct repository paths on one host remain valid. Proxy and mirror still
accept this same-mount pair.

Both cleanup targets have to be sweepable. Dredger refuses a mount whose `mirrorTarget` or whose
`privateUpstream` names a store this build has no maintenance backend for, and it refuses a mirrored
mount whose private cache it cannot observe at all. A `registry` target fails that test on either
side, because it offers no control plane. The refusal names the mount key and the reason.

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
2. Lists the mirror target's and the private cache's package names, one name-space bucket at a
   time, and joins the two inventories so a version held in either is decided once.
3. Keeps only the names the synced advisory database covers, plus the names an identity-deny rule
   pins. Both sides are read through the ecosystem's own name parser, so a spelling difference
   between the advisory database and the store cannot miss a match.
4. Reads each of those packages' metadata **back from the store** and evaluates every version the
   store holds against the mount's whole rule set. A package whose versions nothing condemns is
   read once. A condemned version's package is read again before the delete leaves, so the
   decision that destroys it is taken on the evidence standing at that moment.
5. Deletes only what a named decisive deny condemns.

A cycle reads each bucket's listing whole and metadata for candidate names. A newly covered package can
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

## The target cycle window

`chunkSize` and `chunkPause` set how fast one chunk of packages is examined. They say nothing about
how long a whole cycle takes, and a cycle's length is what decides how long an advisory takes to
reach every affected mirrored version. `targetCycleWindow` is that time, in seconds.

A name an advisory newly covers can miss the running cycle's selection. The window therefore has to
cover the rest of that cycle, one `cyclePause`, and the next whole cycle. One complete cycle gets
`(targetCycleWindow - cyclePause) / 2`. Left unset, the window is computed at boot as three
`cyclePause`, which grants an active cycle the same allowance as the idle interval between cycles.
On the shipped `cyclePause: 3600` that is a window of 10800 seconds and an allowance of 3600.

The sweep measures what each cycle asks of the store: every listing page, every version
enumeration, every metadata read, every delete call, every permission read, and every walk-marker
read or write. It then paces the next cycle to land inside the allowance. A candidate cycle scales
with the listing and the candidate count; a full walk scales with the whole store in prefix buckets.
Pacing only ever comes from a cycle that completed. A cycle that halted read part of the store, so
its counts are discarded rather than allowed to slow the next cycle on partial evidence.

## The request budget

A sweep shares a store with the proxy's own reads and writes, so it is held to a share of that
store's request capacity. `requestBudgetFraction` is that share, above 0 and below 1.

Left unset, the share is computed for each capacity pool as the smaller of one half and
`chunkSize / chunkPause` divided by the pool's tightest quota. On the shipped values against a
CodeArtifact account that is `min(0.5, 25/100) = 0.25`, which allows the sweep 50 listing calls,
200 reads, and 25 writes per second. The budget covers the store being dredged alone. The public
upstream is never read by a sweep.

A `codeArtifact` store needs no declaration. Its capacity is taken from the per-Region service
quotas AWS publishes, read from that documentation rather than discovered from your account, and
the account and Region come from the repository endpoint you declared. Every repository of one
account and Region shares one pool. Charging a listing call to its named quota and an account read
or write dimension is a conservative reading: AWS publishes no exhaustive operation-to-quota map.

A `verdaccio` store publishes no quota at all. Its capacity is **derived** from the sweep's own
package pace, `chunkSize / chunkPause` requests per second, and the same share rule then applies.
At the shipped `chunkSize: 50` and `chunkPause: 2` that is 25 requests a second, a share of one
half, and a ceiling of 12.5 requests a second. Raising `chunkPause` lowers the derived ceiling, so
the keys you already have stay the dial. Nothing is required of you.

Declare a capacity under `quotaOverrides` when you know the store's real one, keyed by the store
URL:

```yaml
dredger:
  quotaOverrides:
    https://verdaccio.example.com/:
      quotas:
        storeRequests: 100
```

100 requests a second is an example, not a Verdaccio default and not a measured guarantee. An entry
can also carry a `scope`, which joins two endpoints of one capacity pool so the sweep paces them
together, and `requestWeights`, which scale what one kind of request costs. A weight naming a kind
the backend charges nothing for scales nothing. An entry naming a store no mount declares warns at
boot and paces nothing, and two entries that give one `scope` different quotas or weights refuse the
boot, naming both.

Stores can also share a pool without you saying so: a `mirrorTarget` and a `privateUpstream` on one
Verdaccio host land in that host's pool together. Where two stores in one pool describe it
differently, the sweep takes the tightest quota on each dimension and the dearest cost for each
kind, so a shared pool is paced by the narrower of what the two claim rather than by whichever the
boot read last.

Each store's boot line records the pool it runs in, where its capacity came from (derived, the
backend's documented quotas, or your configuration), the share in force and where that came from,
and the per-dimension ceilings the share yields.

**An unattainable window warns and carries on.** When the measured cycle needs more of the store's
capacity than the share allows, the Dredger logs a warning naming the share the window would need
and the share in force, then runs the next cycle at its ceiling. When the cycle's own work already
fills the allowance, the warning says that no request budget reaches the window. The sweep never
refuses over pacing, because a refused cycle leaves the denied version served.

**One Dredger per store.** The budget reserves capacity from this sweep alone. It guarantees
nothing against unrelated workloads, and two Dredger processes sharing one account need shares you
allocate between them.

**Not every physical attempt is counted.** The sweep counts the calls it makes. A retry the AWS SDK
makes inside one call is not counted, and a version enumeration counts as one request however many
pages the store takes to answer it. A maintenance client replays no request on a reused connection
below that accounting, so every physical attempt is one the budget counted, and a connection that
fails reaches the store-fault policy: a metadata read that fails keeps its package until a later
cycle.

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
| `verdaccio` | `permitDeletion: true` under each target's tag | unset the key and restart, because the boot reads it |
| `registry` | no consent form and no control plane, so the Dredger refuses the store at boot and names the tag | |

The Dredger never writes a consent marker. Placing one and removing it are yours alone, and the
full walk's resumption marker is a separate tag key so a marker write cannot reach your consent.
`ecluse dredger --dry-run` boots without consent on either backend and reports what it found
instead ([Preview](#preview)).

An ordinary mirror with upstream refill remains ineligible. The separately vetted CodeArtifact
cache capability permits refill and removes retained local versions under its own consent.
Verdaccio's `permitDeletion` retains the operator-declared standalone classification used by the
development backend. Its maintenance token belongs to that target. Boot cannot inspect its uplinks.
A standalone Verdaccio declares no uplinks. Metadata from an uplink-enabled Verdaccio does not
establish a complete local version inventory.

Before each backend batch, Dredger rechecks policy, consent, classification and both inventories.
CodeArtifact requests require `Published` status. Revision changes during evidence collection defer
the version. The API has no revision precondition, so this does not make deletion atomic.
An uncertain call triggers fresh observation before at most one retry, with bounded backoff.
The backend stops later batches after that fault. Confirmed results, uncertain outcomes and unsent
versions remain distinct. Residual local versions make the cycle incomplete and remain eligible for later scans.

The Dredger also applies the [endpoint collision checks](@/docs/configuration.md#endpoint-collisions).
Their comparison scope differs by endpoint role. A shared host alone is not a registry collision,
except for the explicit public-host safeguards.

## The deletion cap

`deletionCap` counts logical package versions within each associated mirror/cache group.
A version consumes one unit before its first destructive attempt. A second target and retries consume no extra unit.
Failed and uncertain attempts still count. Associated work finishes before the cap stops new selections.
The counter resets each cycle, while the cap halt remains latched until restart. It is the breaker
against an advisory database that denies far more than it should.

Left unset, it is computed at boot as 100 per sweepable mirror store, because one cycle covers
every store in turn. That default is deliberately small. Preview first: a dry run reports the
count a real sweep would reach, which is the number to write into `deletionCap`.

Reaching it **halts the Dredger for the life of the process**, whether or not there was more it
would have deleted. No further cycle runs, the readiness probe answers `503`, liveness stays
healthy, and an error line repeats at each cycle interval naming the advisory generation and the
count. The process stays up on purpose: exiting would bring a pod restart, and the restart would
begin sweeping the same poisoned generation again.

The halt credits the exact denial that reached the cap. A preview crossing inside a batch credits
that same threshold item, even when later denials use other generations.

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

`ecluse dredger --dry-run` observes actual inventories from `mirrorTarget` and `privateUpstream`
within each mirrored mount. Mirror-only, cache-only and shared versions all participate.
Each location uses its own metadata and the same configured rules. First-party names remain
excluded before metadata reads. Missing metadata still permits a decisive identity deny when
that identity supplies sufficient evidence. Origin metadata does not veto a selected cache version.

The preview constructs only observing calls. It cannot delete, publish, or write a cursor or
consent marker. Ordinary metadata reads can cause a cache to retain upstream content, so those
reads do not prove local presence. Only the actual inventory determines which versions participate.

CodeArtifact resolves each repository from its own declared URL and authenticates through its
own domain identity. Matching mint identities share a credential provider. Verdaccio preview uses
its own optional maintenance token. It supports anonymous reads and reports missing consent separately. A generic `registry`
private target has no inventory backend and refuses both Dredger modes. An inventory read that cannot
authenticate makes the preview incomplete. No mirror, publication, or caller token is borrowed
for a private target.

A preview needs no deletion consent. It reads the consent marker and the store classification,
reports each one per target, and keeps enumerating either way. Those findings print above the
counts, because a count says what your rules reach, never that this deployment may delete.
Everything else the deleting role needs still applies: the endpoint collision checks, the mirror
target's own parsing, a backend this build can sweep, and the credential the store answers to.

Combined names remain within the existing 10000-name bucket budget. Oversized buckets split
within the existing depth bound of 4. An unsplittable overflow reports incomplete evidence.
The combined versions of one package remain within `limits.maxVersionCount`. CodeArtifact
enforces that limit while reading version pages. No overflow becomes a complete truncated scan.

A full walk under a preview starts at the first bucket every time. It neither reads nor replaces
the marker a real walk records, so a preview leaves a walk in progress where it was.

The cap applies as logging only: passing it writes one line naming where a real run would have
halted, and the preview counts on, so its closing tally reports the full reach. Its metric is
`would_delete`. The closing tally's `deleted` column counts
logical selected versions once per mount, while target-labelled audit lines retain each copy's outcome.
The derived default cap still counts mounts, so a second location does not double it.

Under `--once` the exit status follows completeness alone:

| What the preview did | Exit |
|---|---|
| Read the whole store, and every fact the rules that decided it needed | `0`, whatever the prerequisites said |
| Stopped on a store fault, or decided without an advisory generation or a package's own metadata | non-zero, with partial counts and every candidate it did gather |

Exit `0` means complete, not authorised. A preview that exits `0` with the consent marker absent
reports its selections and the missing permission. The deleting command acts on both configured
stores, with independent consent and fresh evidence for each target.

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

Every deletion names the package, version, denying rule, and the ETag acquired with the advisory
lookup that supplied its winning evidence. An identity-only denial records `none`, even when a
database is loaded. Concurrent rules and retries retain their own evidence.

Whatever stops a cycle repeats an error line at **each cycle interval** until it clears or an
operator restarts the Dredger. Nothing halts silently. That covers a withheld consent marker, a
store that refills itself, a store that stopped answering, and the latched deletion cap.

A store fault is retried once after the delay the fault itself advises. That retry logs at `WARN`,
because it may clear on its own. A fault that survives it halts the cycle and logs at `ERROR`. The
next cycle re-attempts, so an outage reports once per cycle interval for as long as it lasts and
the sweep resumes on its own when the store answers.

A delete the backend refuses, or one that never reached it, leaves the version in the store. It
counts as kept and writes an error line carrying the backend's own code and message.

The `ecluse.dredger.versions` counter carries `target` (`mirrorTarget` or `privateUpstream`)
and `result`, one of `examined`, `deleted`,
`would_delete`, `kept`, or `guard_skipped`. Every version a cycle examines counts once as
`examined` and once more under what the cycle did with it. **Alert on a jump in `deleted`.**

## Permissions

Scope the Dredger independently to its configured mirror and private cache. Token minting does not grant repository access by
itself. An **approved target** below is one of two repositories: the mount's `mirrorTarget` and its
`privateUpstream` cache. Write both sets of ARNs into the policy, because a grant on one repository
authorises nothing in the other. The CodeArtifact role needs these permissions for a default
candidate cycle:

| Action | Resource scope | Purpose |
|---|---|---|
| `codeartifact:GetAuthorizationToken` | Domain ARN | Mint the repository token |
| `sts:GetServiceBearerToken` | `*` in the role's identity policy, restricted by `sts:AWSServiceName = codeartifact.amazonaws.com` | Permit token minting |
| `codeartifact:ListPackages` | Each approved repository ARN | Enumerate package names |
| `codeartifact:ListPackageVersions` | Package ARNs within each approved target | Enumerate versions |
| `codeartifact:DescribeRepository` | Each approved repository ARN, and every repository in the private upstream's chain | Read store classification, and what the private upstream aggregates |
| `codeartifact:ListTagsForResource` | Each approved repository ARN | Read consent and cursor tags |
| `codeartifact:ReadFromRepository` | Each approved repository ARN | Read package metadata for rule evaluation |
| `codeartifact:DeletePackageVersions` | Package ARNs within each approved target | Delete selected versions |

The mirror worker needs the same token-mint permissions, repository reads for its presence probe, and
`codeartifact:PublishPackageVersion` on package ARNs. It does not need Dredger's deletion grant.
On CodeArtifact, Dredger does not need publication permission. See the
[action/resource reference](https://docs.aws.amazon.com/service-authorization/latest/reference/list_codeartifact.html)
and [token requirements](https://docs.aws.amazon.com/codeartifact/latest/ug/tokens-authentication.html).

Preview needs the listed token-mint and observation permissions independently for both approved
repositories. Apply the read resource scopes to each repository and its packages. Preview needs
neither `DeletePackageVersions` nor tag-write permissions on either target.

The full walk also needs cursor-write permissions when both targets share the mirror's bucket alphabet:

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
store's own: a user whose package rights cover that one store and nothing else. Give the mirror
target and the private cache separate accounts, and declare each one's token under its own key
(`mirrorTarget.verdaccio.token` and `privateUpstream.verdaccio.token`), so neither target's
credential reaches the other.

## Known limits

**One Dredger per store.** There is no lease. Two Dredgers against one store double the cap's blast
radius and interleave their marker writes. On a `verdaccio` store, unpublish deletes the whole
package when its last version is removed. Otherwise it edits the package document and deletes
the version's tarball. Verdaccio does not enforce the document revision on these writes, so
either path can lose a concurrent publish of another version of the same package.

**Deleting a version does not keep its bytes.** Lifting the deny permits the version again, but the
next install still needs a source that holds it: the public registry, or a copy another store
retained. Where no source remains, the version stays unavailable after your policy agrees to it. A
read against the private cache can itself restore a copy there, and the next cycle finds and
removes that copy. The [threat model](@/docs/threat-model.md) carries this residual as accepted
risk 109, over both the mirror target and the private cache.

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
