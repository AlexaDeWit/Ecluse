# Technology stack

> Part of the [Écluse architecture overview](../architecture.md).

| Concern | Choice | Rationale |
|---------|--------|-----------|
| Language | Haskell (GHC 9.10) | Type safety, strong concurrency, suits the rule engine. |
| Prelude | `relude` | Safer defaults: `Text` over `String`, partial functions hidden. Wired as the implicit prelude via cabal mixins (see below). |
| Effect style | `ReaderT Env IO` (+ `unliftio`) | The orchestration monad; handlers read a per-request `RequestCtx` (see [key decisions](#key-decisions)). Shared mutable state is `TVar`s, never `StateT`. |
| HTTP server | `warp` + `wai` (+ `wai-extra`) | Raw WAI routing, not a framework; `wai-extra` supplies real-IP recovery and timeouts. See [Web layer](web-layer.md#web-layer). |
| HTTP client | `http-client` + `http-client-tls` | The data plane: streams artifacts and fetches metadata, including the managed-registry npm endpoints. Kept off `amazonka`'s `ResourceT` path. |
| JSON | `aeson` | Metadata parsing (lenient inbound decoding), rule config, queue payloads, denial bodies. |
| API manifest / schemas | `autodocodec` + `openapi3` | Owned types derive their `aeson` codec and the OpenAPI / JSON Schema from one codec, so the schema cannot drift from the wire. See [Capability manifest](web-layer.md#capability-manifest). |
| Observability | `hs-opentelemetry` (OTLP) | Traces and metrics over OTLP, opt-in and off by default. Package roles in [Observability](observability.md). |
| Cloud, AWS | `amazonka` | Split packages: `amazonka-sqs` (queue), `amazonka-codeartifact` (registry token), `amazonka-sts` (workload identity), `amazonka-s3` (advisory object storage), `amazonka-core`. |
| Cloud, GCP | `gogol` *or* a REST client (roadmap) | Pub/Sub queue and Artifact Registry token, gated on a spike; see [Cloud backends](cloud-backends.md#cloud-backends). |
| Logging | `katip` | Structured JSON logging; package, version, and rule context attach to every denial event. |
| Config | `Data.Yaml` | A YAML document with `ECLUSE_*` env overrides deep-merged into an AST. Precedence: embedded defaults < config document < environment, with validation aggregated into one error. |
| Caching | `cache` | STM-backed TTL cache for the short-TTL packument metadata. (Advisory data is a synced in-memory index, not a TTL cache; see [CVE subsystem](rules-engine.md#cve-subsystem).) |
| Concurrency | `async` + `stm` | Non-blocking mirror enqueue; shared cache and state. |
| Time | `time` | `AllowIfOlderThan` age calculations. |
| Unit tests | `hspec` (+ `hspec-wai`) | `hspec-wai` drives the proxy `Application` end-to-end. |
| Property tests | `hedgehog` (+ `hspec-hedgehog`) | Integrated shrinking, used heavily against the pure rules engine. |
| Integration tests | `testcontainers` | Ephemeral Docker containers from the test suite (lifecycle and readiness), GHC 9.10-compatible. |
| Cloud emulation (tests) | `ministack` · Pub/Sub emulator | AWS via `ministack` (image `ministackorg/ministack`, port 4566); GCP via the official Pub/Sub emulator. No real cloud or credentials. |
| Dev environment | Nix flakes + `direnv` | Fully reproducible; all tooling from `nix develop`. |
| Build | Cabal | Natural Nix pairing; `flake.lock` provides reproducibility. |

## Key decisions

**`relude` as the implicit prelude.** Wired through cabal mixins in the shared `common`
stanza, it replaces the default prelude without a per-module `import Relude`. That rules out
`-Wunused-packages` (GHC cannot attribute prelude usage through the mixin rename), so the flag
is omitted and `weeder` is the dependency-hygiene substitute.

**Raw WAI, not a web framework.** A proxy is a passthrough over an irregular URL surface
(URL-encoded slashes, reserved meta-routes), and memory-bounded artifact streaming needs
direct control over the response body's lifetime; both point at a raw `Application` rather
than servant or Yesod. Full rationale in [Web layer](web-layer.md#web-layer).

**The effect model.** `App = ReaderT Env IO` (with `unliftio`) is the orchestration monad;
the server, worker, and request handlers read through it. Handlers run over a per-request
`RequestCtx { ctxRuntime :: ServeRuntime, ctxMount :: MountBinding }`, built once at dispatch
so per-mount deps are read from context rather than re-threaded. The handle records
`MirrorPublish`, `MirrorQueue`, and `CredentialProvider` return `IO`, not `App`, so each
adapter closes over its own backend state and never imports the core's `Env` / `App`, keeping
backends decoupled and import-cycle-free. Shared mutable state lives as `TVar` / `IORef` in
the composition-root `Env`, never a `StateT` layer, which would lose state across `async`.

**Capability manifest, not a client contract.** Écluse speaks registry protocols, not a
bespoke API, so its OpenAPI document is a capability manifest, generated from the closed
`Route` enumeration and the configured mounts. Owned responses are modelled code-first via
`autodocodec` so the schema cannot drift from the wire. Full rationale in
[Capability manifest](web-layer.md#capability-manifest).
