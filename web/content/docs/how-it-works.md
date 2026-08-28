+++
title = "What Écluse is"
description = "What Écluse does for a build, the three registry roles it sees, and the deny-by-default policy it applies."
weight = 1
+++

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
prunes the mirror store. [Deploying Écluse](@/docs/deployment.md) covers all three.

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

These are roles, not necessarily separate servers. The [recommended topology](@/docs/deployment.md#the-recommended-topology)
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

[Rule policy](@/docs/configuration.md#rule-policy) has their knobs.

Independent of the rules, Écluse serves a public version only if it carries a digest that meets
the public integrity floor, `sha256` by default. One gotcha: on a custom or off-spec public
upstream, versions without such a digest silently disappear and their tarballs `403`. To serve such
a source, give it the private upstream role and loosen the trusted floor below `sha256`. The
mechanics are in [Integrity floors](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md#integrity-floors).
