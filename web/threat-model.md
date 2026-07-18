# Threat model

Écluse's threat model is an [OWASP Threat
Dragon](https://owasp.org/www-project-threat-dragon/) model,
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json), the single source
of truth. The register below is generated from that model when this site is built,
so it never drifts and is never hand-copied into prose. Edit the model, not this
page; the next Pages build re-renders the register.

The model also records the canonical deployment posture and the trust assumptions
it rests on, including the operator responsibilities it places out of scope, such
as edge access control and storage-layer scanning.

Each threat carries one status:

- **Mitigated:** the compensating control is implemented in code or delegated to a
  mandatory operator boundary.
- **Accepted:** the risk is intentionally retained, following from a trust
  assumption or a deliberate operator trade-off rather than a missing
  implementation.
- **Open:** a fix is planned or roadmapped but not yet merged. A threat stays Open
  until its code lands, so a milestone entry never reads as done.

For the security invariants and posture the code upholds, and why, see [Security
architecture](docs/architecture/security.md).

## Threat register

```threat-register
```
