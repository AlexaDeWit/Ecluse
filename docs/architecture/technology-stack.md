# Technology Stack

> Part of the [Écluse architecture overview](../architecture.md).

| Concern | Choice | Rationale |
|---------|--------|-----------|
| Language | Haskell (GHC 9.6) | Type safety, strong concurrency, fits the rule engine well. |
| Prelude | `relude` | Safer defaults: `Text` over `String`, partial functions hidden, re-exports `containers`/`text`/`bytestring`/`stm`. Wired in as the implicit prelude (see below). |
| Effect style | `ReaderT Env IO` (+ `unliftio`) | Simple, standard, testable without exotic dependencies. `unliftio` lifts `bracket`/`async` into the reader for the worker/service layer; request handlers stay in plain `IO` taking `Env`. See [Web Layer](web-layer.md#web-layer). |
| HTTP server | `warp` + `wai` (+ `wai-extra`) | Fast, battle-tested. Raw WAI routing rather than a framework — see [Web Layer](web-layer.md#web-layer). `wai-extra` supplies cross-cutting middleware (size limits, real-IP, timeouts). |
| HTTP client | `http-client` + `http-client-tls` | The data plane: streams artifacts and fetches metadata, including the CodeArtifact / Artifact Registry npm endpoints. Kept off `amazonka`'s `ResourceT` streaming path — see [Web Layer](web-layer.md#web-layer). |
| JSON | `aeson` | Metadata parsing, rule config, queue payloads, denial bodies. |
| Cloud — AWS | `amazonka` | Split packages: `amazonka-sqs` (mirror queue), `amazonka-codeartifact` (registry token), `amazonka-sts` (workload identity). Mature and comprehensive. |
| Cloud — GCP | `gogol` *or* a hand-rolled REST client (TBD) | Pub/Sub mirror queue + Artifact Registry token. GCP's Haskell story is weaker than AWS's, so the choice is gated on a spike — see [Cloud Backends](cloud-backends.md#cloud-backends). |
| Logging | `katip` | Structured, contextual JSON logging. Denials are an audit trail — package/version/rule context attaches to every event. |
| Config | `envparse` | Applicative env-var parser; aggregates all missing/invalid vars into one error rather than failing on the first. |
| Caching | `cache` | STM-backed TTL cache for the short-TTL packument metadata cache; handles expiry/eviction. (Advisory data is a synced in-memory index, not a TTL cache — see [CVE Subsystem](rules-engine.md#cve-subsystem).) |
| Concurrency | `async` + `stm` | Non-blocking mirror enqueue; shared cache/state. |
| Time | `time` | `AllowIfPublishedBefore` age calculations. |
| Unit tests | `hspec` (+ `hspec-wai`) | `hspec-wai` drives the proxy `Application` end-to-end. |
| Property tests | `hedgehog` (+ `hspec-hedgehog`) | Integrated shrinking; used heavily against the pure rules engine. |
| Integration tests | `testcontainers` | Launches ephemeral Docker containers from the test suite (lifecycle + readiness). GHC 9.6-compatible, actively maintained. |
| Cloud emulation (tests) | `ministack` · Pub/Sub emulator | AWS via `ministack` (image `ministackorg/ministack`, port 4566, SQS/STS); GCP via Google's official Pub/Sub emulator. Both run as containers through `testcontainers` — no real cloud or credentials. |
| Dev environment | Nix flakes + `direnv` | Fully reproducible; all tooling from `nix develop`. |
| Build | Cabal | Natural Nix pairing; `flake.lock` provides reproducibility. |

## Key Decisions

**`relude` as the implicit prelude.** Rather than `NoImplicitPrelude` plus a manual
`import Relude` in every module, it is wired through cabal mixins in the shared
`common` stanza so it replaces the default prelude transparently:

```cabal
build-depends: base, relude
mixins:
    base hiding (Prelude)
  , relude (Relude as Prelude)
```

Note: this rules out `-Wunused-packages`. GHC cannot attribute prelude usage
through the mixin rename, so it reports `base` and `relude` as unused in every
component — a false positive. The flag is therefore omitted; reach for `weeder`
if dependency-hygiene checking is wanted later.

**Raw WAI, not a web framework.** A proxy is a passthrough over an irregular URL
surface (URL-encoded slashes, reserved meta-routes), and memory-bounded artifact
streaming needs direct control over the response body's lifetime. Both point away
from servant/Scotty/Yesod and toward a raw `Application`. The full rationale —
routing, the control/data-plane split, streaming, and the middleware stance — is
in [Web Layer](web-layer.md#web-layer).

**The effect model: `IO` seams, `App` orchestration.** `App = ReaderT Env IO`
(with `unliftio`) is the orchestration monad for the worker/service layer;
request handlers run in plain `IO` taking `Env`, so the hot path carries no
transformer lifting (see [Web Layer](web-layer.md#web-layer)). The seam records
— `RegistryClient`, `MirrorQueue`, `CredentialProvider` — return **`IO`, not
`App`**: each adapter closes over its own backend state and never imports the
core's `Env`/`App`, so backends stay decoupled (no import cycle, and no recursive
reference from `Env` holding seams whose methods would need `Env`). App-level code
calls a seam through a single `liftIO`. `Env` is the composition-root record
holding the seams plus the shared HTTP manager, caches, and logger.
