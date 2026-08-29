# Security policy

How to report a vulnerability in Écluse (package `ecluse`), a supply-chain policy proxy for
package registries. I take security reports seriously and handle them with priority.

## Reporting a vulnerability

Report vulnerabilities privately. Don't open a public issue for a security problem.

- Preferred: GitHub private vulnerability reporting, the "Report a vulnerability" button under
  this repository's Security tab.
- Alternative: reach me via my GitHub profile, [@AlexaDeWit](https://github.com/AlexaDeWit).

Include a description of the issue, steps to reproduce or a proof of concept, and the affected
commit or version. Expect an initial acknowledgement within a few business days. I'll keep you
updated as I investigate and prepare a fix.

Known risks and their dispositions live in the
[threat model](https://ecluse-proxy.com/docs/threat-model/). Check it before reporting a
risk that's already recorded and accepted.

Do not submit AI-generated reports you haven't verified. An unreproduced AI report consumes
scarce maintainer time, and I close it without a detailed response. Confirm the issue is real
first.

## Intentional refusals, not bugs

Some of Écluse's security refusals can surprise an operator expecting a transparent
passthrough. One example is the public integrity-digest floor, which drops weakly-hashed
public versions. These refusals are deliberate. The
[operator manual](https://ecluse-proxy.com/docs/configuration/#rule-policy) and the
[security invariants](docs/architecture/security.md) document them. Confirm a surprising
refusal against those before reporting it.

## Supported versions

The project is pre-1.0 and under active development. I support only the latest `main` branch.
