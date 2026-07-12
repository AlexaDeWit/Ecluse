# Threat model

Écluse's threat model is maintained as an [OWASP Threat
Dragon](https://owasp.org/www-project-threat-dragon/) model —
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json) — which is the
**single source of truth**. The register below is generated *from that model*
when this site is built, so it cannot drift from the model and is never
hand-copied into prose. Edit the model, not this page: the next Pages build
re-renders the register.

The model also records the canonical deployment posture and the trust assumptions
it rests on — including the operator responsibilities, such as edge access control
and storage-layer scanning, that it places out of scope — rather than restating
them here.

**Status Policy:** This model represents the strict reality of the `main` branch codebase today. A threat is only marked as `Mitigated` if its compensating control is physically implemented in code or formally delegated to a mandatory operator boundary. `Accepted` means the risk is intentionally retained as residual risk, usually because it follows from a foundational trust assumption or a deliberate operator trade-off rather than from a missing implementation. Threats with planned or roadmapped fixes (even if tracked in a milestone) remain `Open` until the corresponding code is merged, preventing any false sense of security.

For the security *invariants and posture* — the outbound-request and
input-validation contract the code upholds, and why — see [Security
architecture](docs/architecture/security.md). That document is the narrative;
this page is the register, and the two must not duplicate each other.

## Threat register

```threat-register
```
