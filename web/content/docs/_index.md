+++
title = "Documentation"
description = "Deploy, configure, and operate Écluse, and read the threat model and the registry protocol it speaks."
sort_by = "weight"
template = "docs-section.html"
page_template = "docs-page.html"
+++

This section is the operator manual: how to run the proxy, the threats it defends against,
and the registry protocol it speaks. The sidebar lists every page.

If you are reading or extending the code instead, the [API reference](@/api/_index.md)
holds the Haddock for each library.

## Learn more

When you need the _why_, the internal design documents carry it:

- [Architecture overview](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture.md)
- [Configuration and authentication](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/configuration.md)
- [Security posture](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/security.md)
- [Threat model](@/docs/threat-model.md), the STRIDE register, generated from the OWASP Threat
  Dragon model
  ([`threat-modelling/ecluse.json`](https://github.com/AlexaDeWit/Ecluse/blob/main/threat-modelling/ecluse.json))
- [Rules engine](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/rules-engine.md)
- [Multi-ecosystem hosting and URL rewriting](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/web-layer.md#web-layer)
- [Release and supply-chain operations](https://github.com/AlexaDeWit/Ecluse/blob/main/docs/architecture/release-supply-chain.md)
