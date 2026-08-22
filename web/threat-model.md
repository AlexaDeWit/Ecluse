# Threat model

Écluse's threat model is an [OWASP Threat
Dragon](https://owasp.org/www-project-threat-dragon/) model,
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json). The site build
generates the register below from that file on every deploy, so the register and
the model cannot drift. The model also records the canonical deployment posture,
the trust assumptions it rests on, and the operator responsibilities it places
out of scope, such as edge access control and storage-layer scanning.

Each threat carries one status:

- **Mitigated:** the code carries the compensating control, or Écluse delegates
  it to a mandatory operator boundary.
- **Accepted:** Écluse retains the risk on purpose. It follows from a trust
  assumption or a deliberate operator trade-off, not from a missing
  implementation.
- **Open:** Écluse plans a fix but the code does not carry it yet. A threat
  stays Open until its code lands, so a milestone entry never reads as done.

The security invariants the code upholds, and why, are in [Security
posture](docs/architecture/security.md).

## Threat register

```threat-register
```
