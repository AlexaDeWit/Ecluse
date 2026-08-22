-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's worker bundle construction. One function turns the served
mounts, the resolved publish targets, and the adapter registry into the per-ecosystem
'WorkerPolicies' the mirror worker dispatches every job through.

'Ecluse.Proxy.runProxy' consumes this, and a worker-only binary is a thin entry over
the same function. This function assembles everything the worker's dispatch needs,
rather than threading it through the proxy's own wiring. That means the re-evaluation
inputs, the artifact request formation, and the married mirror-write capability. Only
the composition root consumes the adapter registry, per the standing rule. The worker
itself receives plain handles and never resolves an adapter.

Each bundle reuses its mount's __own__ 'PackumentDeps': the same prepared rules,
floors, host gate, and request formation the serve path gates with. The ingest
decision therefore cannot diverge from the serve decision. It also marries its
ecosystem's publish codec to the shared publish transport at the mount's declared
mirror target.
A mount that serves no packument contributes no bundle, and neither does an ecosystem
without a resolved publish target or adapter. A job for it is fail-closed at the
worker rather than half-wired here.
-}
module Ecluse.Composition.Worker (
    workerPoliciesFor,
    mirrorTransportFor,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition (PublishTarget (ptCredentials, ptEcosystem, ptMirrorUrl))
import Ecluse.Core.Credential (AuthToken (authSecret), currentToken)
import Ecluse.Core.Ecosystem (Ecosystem, parseEcosystem)
import Ecluse.Core.Registry.Adapter (adapterFor, adapterPublish, publishCodec)
import Ecluse.Core.Registry.Metadata (fetchVersionDetails)
import Ecluse.Core.Registry.Publish (
    MirrorPublish,
    MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken),
    newMirrorPublish,
 )
import Ecluse.Core.Security (Limits (maxBodyBytes), Origin (UntrustedOrigin), defaultLimits, thgPublicHostPort)
import Ecluse.Core.Server.Cache (Source (Source))
import Ecluse.Core.Server.Context (
    PackumentDeps,
    pdBuildArtifactRequestByUrl,
    pdLimits,
    pdMinIntegrity,
    pdNewMetadataClient,
    pdNow,
    pdPublicBaseUrl,
    pdRules,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached))
import Ecluse.Core.Telemetry.Metrics (Upstream (Public))
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (..))
import Ecluse.Runtime.Env (Env, envManager, envMetadataCache, envMetrics, envPrivateManager, envTelemetry)
import Ecluse.Runtime.Server (MountBinding (bindingPackumentDeps, bindingPrefix))
import Ecluse.Runtime.Telemetry.Instruments (metricsPortOf)
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf)

{- | Build the worker's per-ecosystem bundles from the served mounts and the resolved
publish targets, keyed by the ecosystem each mount's path prefix names. A job for an
ecosystem absent here is fail-closed at the worker.
-}
workerPoliciesFor :: Env -> [MountBinding] -> [PublishTarget] -> Int -> WorkerPolicies
workerPoliciesFor env bindings targets artifactMaxBytes =
    Map.fromList
        [ (eco, workerPolicyFor env deps publish artifactMaxBytes)
        | binding <- bindings
        , let prefixHead :| _ = bindingPrefix binding
        , let deps = bindingPackumentDeps binding
        , Just eco <- [parseEcosystem prefixHead]
        , Just publish <- [mirrorPublishFor env deps targetsByEcosystem eco]
        ]
  where
    targetsByEcosystem = Map.fromList [(ptEcosystem target, target) | target <- targets]

{- Marry one ecosystem's mirror write to the shared publish transport. 'Nothing' when it
resolves no publish target or no adapter, so the caller wires no half-publish bundle. -}
mirrorPublishFor :: Env -> PackumentDeps -> Map.Map Ecosystem PublishTarget -> Ecosystem -> Maybe MirrorPublish
mirrorPublishFor env deps targets eco = do
    target <- Map.lookup eco targets
    adapter <- adapterFor eco
    pure (newMirrorPublish (mirrorTransportFor env deps target) (ptMirrorUrl target) (publishCodec (adapterPublish adapter)))

{- | The shared mirror-write transport for one mount. The presence probe reads under the
mount's own 'pdLimits', because the shipped metadata-path default would let a larger
mirror packument overrun the bound and defeat duplicate suppression.
-}
mirrorTransportFor :: Env -> PackumentDeps -> PublishTarget -> MirrorTransport
mirrorTransportFor env deps target =
    MirrorTransport
        { ptManager = envPrivateManager env
        , ptMintToken = Just . authSecret <$> currentToken (ptCredentials target)
        , ptLimits = pdLimits deps
        }

{- Build one mount's worker bundle. Every decision input comes from the mount's own
'PackumentDeps', so the ingest decision cannot diverge from the serve decision. The
metadata client is anonymous, so no client credential reaches the public origin, and the
host allowlist gates it with certificate validation authenticating the dialled host. The
no-op callbacks elide the client's own failure and dropped-entry logs, because the worker
logs its re-evaluation outcome per job, and the metrics still record. -}
workerPolicyFor :: Env -> PackumentDeps -> MirrorPublish -> Int -> WorkerPolicy
workerPolicyFor env deps publish artifactMaxBytes =
    WorkerPolicy
        { wpResolveVersion = fetchVersionDetails client
        , wpRules = pdRules deps
        , wpMinIntegrity = pdMinIntegrity deps
        , wpArtifactHostHonoured =
            -- The same host gate the serve path applies before its public artifact fetch, closed
            -- against the public upstream authority.
            tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps))
        , -- The mount's own request formation (the adapter's artifact capability,
          -- projected onto these deps at the composition root), so the worker fetches
          -- a job's bytes exactly as the serve path would.
          wpBuildArtifactRequest = pdBuildArtifactRequestByUrl deps
        , wpPublish = publish
        , -- The artifact fetch cap comes from the memory plan's mirror-artifact tenant,
          -- not the metadata-path default, because a tarball far exceeds the packument cap. The
          -- other limits do not apply to an opaque tarball, so they stay at their defaults.
          wpArtifactLimits = defaultLimits{maxBodyBytes = artifactMaxBytes}
        , wpNow = pdNow deps
        }
  where
    client =
        pdNewMetadataClient
            deps
            (tracingPortOf (envTelemetry env))
            (metricsPortOf (envMetrics env))
            Public
            (Cached (envMetadataCache env) (Source (pdPublicBaseUrl deps)))
            (\_ _ -> pure ())
            (\_ _ -> pure ())
            (\_ -> pure ())
            (pdLimits deps)
            (envManager env)
            (pdPublicBaseUrl deps)
            Nothing
