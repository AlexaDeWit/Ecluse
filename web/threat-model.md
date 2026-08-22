# Threat model

Écluse's threat model is an [OWASP Threat
Dragon](https://owasp.org/www-project-threat-dragon/) model,
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json). That file is the
single source of truth. The site build generates the register below from the
model, so the register never drifts and nobody hand-copies a threat into prose.
Edit the model, not this page. The next Pages build re-renders the register.

The model also records the canonical deployment posture and the trust
assumptions it rests on. That includes the operator responsibilities it places
out of scope, such as edge access control and storage-layer scanning.

Each threat carries one status:

- **Mitigated:** the code carries the compensating control, or Écluse delegates
  it to a mandatory operator boundary.
- **Accepted:** Écluse retains the risk on purpose. It follows from a trust
  assumption or a deliberate operator trade-off, not from a missing
  implementation.
- **Open:** Écluse plans a fix but the code does not carry it yet. A threat
  stays Open until its code lands, so a milestone entry never reads as done.

For the security invariants and posture the code upholds, and why, see [Security
architecture](docs/architecture/security.md).

## Threat register

```threat-register
```
