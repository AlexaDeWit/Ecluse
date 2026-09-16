+++
title = "Threat model"
description = "The generated STRIDE threat register and deployment assumptions for Écluse."
weight = 7
+++

One file holds both what Écluse defends against and what it leaves to you: a
[Saerskriven](https://github.com/AlexaDeWit/Saerskriven) model,
[`threat-modelling/ecluse.yaml`](https://github.com/AlexaDeWit/Ecluse/blob/main/threat-modelling/ecluse.yaml).
The site build renders the diagram and the register below from that file on every deploy, so
neither can drift from the model. Each number is a lifetime identifier from the model, not a row
count, so the sequence keeps its gaps. The model also records the canonical deployment posture, the
trust assumptions it rests on, and the operator responsibilities it places out of scope,
such as edge access control and storage-layer scanning.

Each threat carries one status:

- **Mitigated:** The code carries the compensating control, or Écluse delegates
  it to a mandatory operator boundary.
- **Accepted risk:** Écluse retains the risk on purpose. It follows from a trust
  assumption or a deliberate operator trade-off, not from a missing
  implementation.
- **Open:** Écluse plans a fix but the code does not carry it yet. A threat
  stays Open until its code lands, so a milestone entry never reads as done.

[Security
posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md)
explains the security invariants the code upholds, and why.

## Data-flow diagram

{{ diagram(name="threat-model", alt="The STRIDE data-flow diagram: npm clients reach the Écluse proxy inside the operator trust zone. The proxy fetches from the public npm registry, reads and publishes through the private registries, and queues mirror jobs. The mirror worker, Pilot, and Dredger write to the stores, and Pilot ingests OSV and EPSS data from the public internet.") }}

## Threat register

{{ generated(name="threat-register") }}
