# Cargo integration

Cargo uses the shared admission, mirror, publication, maintenance, and telemetry paths.
The supported client contract is Cargo 1.74+ with sparse HTTPS indexes. Git index transport is outside scope.

Use these sources for the implemented contract:

| Concern | Authoritative source |
|---|---|
| Client setup and credential providers | [Connect Cargo](../web/content/docs/connect.md#cargo) |
| Routes, retained fields, and protocol limits | [Cargo protocol](../web/content/docs/protocol-support.md#cargo) |
| Configuration keys and defaults | [Default configuration](../config/default.yaml) |
| Discovery, source identity, and durable completion rationale | [Cargo integration](architecture/registry-model.md#cargo-integration) |
| Native fixtures, full contracts, and resource evidence | [Cargo testing](testing.md#cargo-native-provider) |
| Threats, controls, and accepted assumptions | [Canonical threat model](../threat-modelling/ecluse.yaml) |
| Extension process and interface ownership | [Adding an ecosystem](adding-an-ecosystem.md) and the Cargo modules' Haddock |

The [approved design](https://github.com/AlexaDeWit/Ecluse/issues/1883#issuecomment-6400735787)
records the client baseline, trust boundary, and delivery requirements.
[#1895](https://github.com/AlexaDeWit/Ecluse/issues/1895) and [#1892](https://github.com/AlexaDeWit/Ecluse/issues/1892)
later replaced its two-view serving and execution-denial decisions.
[Cargo protocol](../web/content/docs/protocol-support.md#cargo) holds the current serving contract.
The runtime adapter requires measured native charge factors.
Ordinary arm64 CI must supply those observations. A missing factor remains an explicit failure.
