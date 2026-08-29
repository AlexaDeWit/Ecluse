+++
title = "Écluse"
description = "A supply-chain policy proxy for package registries. It applies a deny-by-default policy before a package reaches a build."
template = "index.html"
sort_by = "weight"

[extra]
tagline = "A supply-chain policy proxy for package registries."
+++

Écluse is a proxy you put in front of public package registries to protect the builds that
install from them. Point your CI and developer tooling at Écluse instead of at a public
registry. Écluse fetches from that registry on their behalf and decides which versions a
build may install. The npm registry is the first one supported, and any client that speaks
its protocol works, such as npm, pnpm, yarn, or bun.

A new public version waits in a quarantine, seven days by default, before a build can
install it. Most malicious publishes are found and pulled within days, so the wait alone
sidesteps them, with no attempt to detect malice. A version that an advisory names as the
fix for a vulnerability skips the wait, so the quarantine never delays a security patch.
Everything else is deny by default and opt-in by name.

If you run a private registry, Écluse reads it first and passes your own packages through
untouched. It can also mirror each admitted public version into that registry, so a mirrored
version survives a public outage or yank. AWS CodeArtifact is the private registry supported
today, and Écluse hosts no packages itself.
