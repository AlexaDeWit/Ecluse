# Security Policy

**Écluse** (package `ecluse`) is a supply-chain resilience tool against malicious-package
attacks, so I take security reports seriously and handle them with priority.

## Reporting a Vulnerability

Please report vulnerabilities **privately**. Don't open a public issue for a security
problem.

- **Preferred:** GitHub private vulnerability reporting, the **"Report a vulnerability"**
  button under this repository's **Security** tab.
- **Alternative:** reach me via my GitHub profile, [@AlexaDeWit](https://github.com/AlexaDeWit).

Please include a description of the issue, steps to reproduce or a proof of concept, and the
affected commit or version. You can expect an initial acknowledgement within a few business
days, and I'll keep you updated as I investigate and prepare a fix.

**Do not submit AI-generated reports you haven't verified.** A report that an AI produced and
you haven't reproduced and confirmed yourself isn't welcome: it consumes scarce maintainer
time and will be closed without a detailed response. Confirm the issue is real before
reporting it.

## Deliberate admission policies (and their gotchas)

Some of Écluse's behaviours are intentional security refusals rather than bugs. They can
surprise an operator who expects a transparent passthrough, so I'm calling them out here as
designed behaviour.

### A public version must carry an integrity digest

A package version served from a **public** (untrusted) upstream **must carry an integrity
digest whose algorithm meets the integrity floor** (`PROXY_MIN_PUBLIC_INTEGRITY`, default
**SHA-256**). A public version whose strongest digest is **absent**, or **below the floor** —
for example only a legacy SHA-1 `dist.shasum`, with no `sha256`/`sha512` SRI `dist.integrity`
— is **inadmissible**: it's refused outright, never served.

- On the **artifact** (tarball) path, a request for such a version is refused with a `403`
  before the tarball is ever fetched.
- On the **packument** (metadata) path, it is **filtered out of the served listing**, so a
  client never sees a version it couldn't safely fetch. (If a package's *only* public versions
  are inadmissible, the packument request is a `403`.)

**Why.** SHA-1 and MD5 have practical collisions, so a match on one can't prove an artifact
wasn't substituted; a version with no strong digest can't be tied to a tamper-evident
fingerprint. Écluse detects supply-chain tampering by comparing a version's integrity across
upstreams; two differing-byte copies that share only a weak (or no) digest could
fingerprint-collide and a divergence would go undetected. Refusing the version at admission
closes that gap: it never reaches the serve path nor contributes a weak fingerprint to the
cross-upstream merge. The floor may be raised (`sha512`, `blake2b`) but never set below
SHA-256 — a sub-floor value is rejected at startup.

**The trusted private upstream is exempt.** Versions from the configured private upstream are
trusted and enter unfiltered, so a SHA-1-only *private* version is still served (trust
substitutes for cryptographic strength). The floor applies only to untrusted public upstreams.

**Gotcha.** If a public registry serves a version whose strongest digest is below the floor
(no `integrity`, or only a legacy `shasum` — rare for npm, but possible for an off-spec mirror
or custom upstream), that version will be missing from what Écluse serves and a direct fetch
of it will `403`. This is deliberate: point such a source at the *private* (trusted) upstream
slot if you genuinely intend to serve it.

## Supported Versions

The project is pre-1.0 and under active development; only the latest `main` branch is
supported.
