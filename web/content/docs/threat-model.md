+++
title = "Threat model"
description = "The generated STRIDE threat register and deployment assumptions for Écluse."
weight = 7
+++

One file holds both what Écluse defends against and what it leaves to you: an [OWASP Threat
Dragon](https://owasp.org/www-project-threat-dragon/) model,
[`threat-modelling/ecluse.json`](https://github.com/AlexaDeWit/Ecluse/blob/main/threat-modelling/ecluse.json).
The site build generates the register below from that file on every deploy, so the register
and the model cannot drift. The model also records the canonical deployment posture, the
trust assumptions it rests on, and the operator responsibilities it places out of scope,
such as edge access control and storage-layer scanning.

Each threat carries one status:

- **Mitigated:** The code carries the compensating control, or Écluse delegates
  it to a mandatory operator boundary.
- **Accepted:** Écluse retains the risk on purpose. It follows from a trust
  assumption or a deliberate operator trade-off, not from a missing
  implementation.
- **Open:** Écluse plans a fix but the code does not carry it yet. A threat
  stays Open until its code lands, so a milestone entry never reads as done.

[Security
posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md)
explains the security invariants the code upholds, and why.

## Threat register

{{ threat_register() }}
