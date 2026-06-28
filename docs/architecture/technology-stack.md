# Technology Stack

> Part of the [Écluse architecture overview](../architecture.md).

| Concern | Choice | Rationale |
|---------|--------|-----------|
| Language | Haskell (GHC 9.10) | Type safety, strong concurrency, fits the rule engine well. |
| Prelude | `relude` | Safer defaults: `Text` over `String`, partial functions hidden, re-exports `containers`/`text`/`bytestring`/`stm`. Wired in as the implicit prelude (see below). |
| Effect style | `ReaderT Env IO` (+ `unliftio`) | Simple, standard, testable without exotic dependencies. `unliftio` lifts `bracket`/`async` into the reader; the server, worker, and request handlers all read through it (handlers over a per-request `RequestCtx`). Shared mutable state lives as `TVar`s in `Env`, never a `StateT` layer. See [Web Layer](web-layer.md#web-layer). |
| HTTP server | `warp` + `wai` (+ `wai-extra`) | Fast, battle-tested. Raw WAI routing rather than a framework; see [Web Layer](web-layer.md#web-layer). `wai-extra` supplies cross-cutting middleware (size limits, real-IP, timeouts). |
| HTTP client | `http-client` + `http-client-tls` | The data plane: streams artifacts and fetches metadata, including the CodeArtifact / Artifact Registry npm endpoints. Kept off `amazonka`'s `ResourceT` streaming path; see [Web Layer](web-layer.md#web-layer). |
| JSON | `aeson` | Metadata parsing (lenient **inbound** wire decoding), rule config, queue payloads, denial bodies. |
| API manifest / schemas | `autodocodec` + `openapi3` | The [capability manifest](api-surface.md) and config JSON Schema: **owned** types (error envelope, synthesized packument, config) derive their `aeson` codec **and** the OpenAPI / JSON-Schema from one `autodocodec` codec, no drift; `openapi3` assembles the document. Inbound npm wire decoding stays lenient `aeson`. |
| Cloud, AWS | `amazonka` | Split packages: `amazonka-sqs` (mirror queue), `amazonka-codeartifact` (registry token), `amazonka-sts` (workload identity). Mature and comprehensive. |
| Cloud, GCP | `gogol` *or* a hand-rolled REST client (TBD) | Pub/Sub mirror queue + Artifact Registry token. GCP's Haskell story is weaker than AWS's, so the choice is gated on a spike; see [Cloud Backends](cloud-backends.md#cloud-backends). |
| Logging | `katip` | Structured, contextual JSON logging. Denials are an audit trail, package/version/rule context attaches to every event. |
| Config | `envparse` | Applicative env-var parser; aggregates all missing/invalid vars into one error rather than failing on the first. |
| Caching | `cache` | STM-backed TTL cache for the short-TTL packument metadata cache; handles expiry/eviction. (Advisory data is a synced in-memory index, not a TTL cache; see [CVE Subsystem](rules-engine.md#cve-subsystem).) |
| Concurrency | `async` + `stm` | Non-blocking mirror enqueue; shared cache/state. |
| Time | `time` | `AllowIfOlderThan` age calculations. |
| Unit tests | `hspec` (+ `hspec-wai`) | `hspec-wai` drives the proxy `Application` end-to-end. |
| Property tests | `hedgehog` (+ `hspec-hedgehog`) | Integrated shrinking; used heavily against the pure rules engine. |
| Integration tests | `testcontainers` | Launches ephemeral Docker containers from the test suite (lifecycle + readiness). GHC 9.10-compatible, actively maintained. |
| Cloud emulation (tests) | `ministack` · Pub/Sub emulator | AWS via `ministack` (image `ministackorg/ministack`, port 4566, SQS/STS); GCP via Google's official Pub/Sub emulator. Both run as containers through `testcontainers`, no real cloud or credentials. |
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
component, a false positive. The flag is therefore omitted; reach for `weeder`
if dependency-hygiene checking is wanted later.

**Raw WAI, not a web framework.** A proxy is a passthrough over an irregular URL
surface (URL-encoded slashes, reserved meta-routes), and memory-bounded artifact
streaming needs direct control over the response body's lifetime. Both point away
from servant/Scotty/Yesod and toward a raw `Application`. The full rationale,routing, the control/data-plane split, streaming, and the middleware stance, is
in [Web Layer](web-layer.md#web-layer).

**The effect model: `IO` handles, `App` orchestration.** `App = ReaderT Env IO`
(with `unliftio`) is the orchestration monad; the server and worker read `Env`
through it, and **request handlers run in the reader too**, over a per-request
`RequestCtx { ctxEnv, ctxMount }` pairing `Env` with the matched mount's
[`MountBinding`](hosting.md#mounts), built once at dispatch so the per-mount deps
(registry set, rules, renderer, derived prefix) are read from context rather than
re-threaded through the pipeline (see [Web Layer](web-layer.md#web-layer)). Shared
mutable state (credential refresh, circuit-breaker, in-flight sets) lives as
`TVar`/`IORef` **in `Env`** under that single reader, **not a `StateT` layer**,
which would lose state across `forkIO`/`async` and give no shared state. The handle records:
`RegistryClient`, `MirrorQueue`, `CredentialProvider`, return **`IO`, not
`App`**: each adapter closes over its own backend state and never imports the
core's `Env`/`App`, so backends stay decoupled (no import cycle, and no recursive
reference from `Env` holding handles whose methods would need `Env`). App-level code
calls a handle through a single `liftIO`. `Env` is the composition-root record
holding the handles plus the shared HTTP manager, caches, and logger.

**Capability manifest, not a client contract.** Écluse speaks registry protocols,
not a bespoke API, so its OpenAPI document is a *capability manifest*, generated
from the closed `Route` enumeration × the configured mounts. Owned / synthesized
responses (the error envelope and the merged-and-filtered packument) are modelled
**code-first via `autodocodec`** so the schema cannot drift from the wire format;
opaque pass-through bodies (tarballs) are linked out rather than re-specified; and
unsupported routes (`Search`) are documented as `501`. Full rationale, schema
strategy, and node-free CI rendering in
[API Surface & Capability Manifest](api-surface.md).
