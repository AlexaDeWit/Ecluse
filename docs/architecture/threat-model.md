# Threat model

Écluse's threat model is maintained as an [OWASP Threat
Dragon](https://owasp.org/www-project-threat-dragon/) model —
[`threat-modelling/ecluse.json`](threat-modelling/ecluse.json) — which is the
**single source of truth**. The register below is generated *from that model*
when this site is built, so it cannot drift from the model and is never
hand-copied into prose. Edit the model, not this page: the next Pages build
re-renders the register.

The model captures the canonical deployment posture — per-caller passthrough
credentials, the three-registry topology (first-party private store,
public-derived mirror store, and a pull-through read endpoint), and the
demand-driven mirror worker minting its own write token under the container
role. Edge access control and storage-layer scanning are operator
responsibilities, recorded in the model as out-of-scope trust assumptions.

For the security *invariants and posture* — the outbound-request and
input-validation contract the code upholds, and why — see [Security
architecture](docs/architecture/security.md). That document is the narrative;
this page is the register, and the two must not duplicate each other.

## Threat register

```threat-register
```
