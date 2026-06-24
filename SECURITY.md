# Security Policy

**Écluse** (package `ecluse`) is a supply-chain resilience tool against
malicious-package attacks, so security reports are taken seriously and handled
with priority.

## Reporting a Vulnerability

Please report vulnerabilities **privately** — do not open a public issue for a
security problem.

- **Preferred:** GitHub private vulnerability reporting — the **"Report a
  vulnerability"** button under this repository's **Security** tab.
- **Alternative:** reach the maintainer via their GitHub profile,
  [@AlexaDeWit](https://github.com/AlexaDeWit).

Please include a description of the issue, steps to reproduce or a proof of
concept, and the affected commit or version. You can expect an initial
acknowledgement within a few business days, and we will keep you updated as we
investigate and prepare a fix.

**Do not submit AI-generated reports you have not verified.** A report that an AI
produced and you have not reproduced and confirmed yourself is not welcome — it
consumes scarce maintainer time and will be closed without a detailed response.
Confirm the issue is real before reporting it.

## Deliberate admission policies (and their gotchas)

Some of Écluse's behaviours are intentional security refusals rather than bugs.
They can surprise an operator who expects a transparent passthrough, so they are
called out here as designed behaviour.

### A public version must carry an integrity digest

A package version served from a **public** (untrusted) upstream **must carry at
least one integrity digest** — an SRI `dist.integrity` *or* a legacy `dist.shasum`.
A public version whose `dist` carries **neither** is **inadmissible**: it is refused
outright, never served.

- On the **artifact** (tarball) path, a request for a hashless public version is
  refused with a `403` before the tarball is ever fetched.
- On the **packument** (metadata) path, a hashless public version is **filtered out
  of the served listing**, so a client never sees a version it could not safely
  fetch. (If a package's *only* public versions are hashless, the packument request
  is a `403`.)

**Why.** A version with no integrity check cannot be tied to a tamper-evident
fingerprint. Écluse detects supply-chain tampering by comparing a version's integrity
across upstreams; two differing-byte copies that both lack a digest would fingerprint
identically and a divergence would go undetected. Refusing the version at admission
closes that gap — it never reaches the serve path nor contributes a hashless
fingerprint to the cross-upstream merge.

**The trusted private upstream is exempt.** Versions from the configured private
upstream are trusted and enter unfiltered, so a hashless *private* version is still
served. The policy applies only to untrusted public upstreams.

**Gotcha.** If a public registry serves a version without `integrity`/`shasum` (rare
for npm, but possible for an off-spec mirror or a custom upstream), that version will
be missing from what Écluse serves and a direct fetch of it will `403`. This is
deliberate: point a hashless source at the *private* (trusted) upstream slot if you
genuinely intend to serve it.

## Supported Versions

The project is pre-1.0 and under active development; only the latest `main`
branch is supported.
