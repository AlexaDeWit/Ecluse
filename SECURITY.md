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
digest whose algorithm meets the integrity floor** (`ECLUSE_MIN_PUBLIC_INTEGRITY`, default
**SHA-256**). A public version whose strongest digest is **absent**, or **below the floor**
(for example only a legacy SHA-1 `dist.shasum`, with no `sha256`/`sha512` SRI `dist.integrity`)
is **inadmissible**: it's refused outright, never served.

For detailed information on configuring the public integrity floor, its boundaries, and how it interacts with the trusted upstream, please refer to [USAGE.md](USAGE.md).

## Supported Versions

The project is pre-1.0 and under active development; only the latest `main` branch is
supported.
