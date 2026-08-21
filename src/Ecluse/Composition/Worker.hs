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
publish targets. A mount earns a bundle when it serves a packument (it carries
'PackumentDeps') and its ecosystem resolves both a publish target and an adapter. Its
key is the ecosystem its path prefix names. A mount left at the
recognised-but-unserved stub contributes none, and a job for an ecosystem absent here
is fail-closed at the worker.

The bundles reuse each mount's __own__ prepared rules. The serve gate and the ingest
re-evaluation therefore share one prepared rule set, with any per-source breaker
state, rather than preparing a second. The publish leg is that ecosystem's codec
married to the shared transport at its declared mirror target.
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

{- Marry one ecosystem's mirror write: its adapter's protocol codec over the shared
publish transport, bound to the mount's declared mirror target. That transport is the
trusted private-origin manager, the target's credential mint, and the mount's own
plan-resolved response bound on the probe. 'Nothing' when the ecosystem resolved no
publish target or no adapter, so the caller wires no half-publish bundle. -}
mirrorPublishFor :: Env -> PackumentDeps -> Map.Map Ecosystem PublishTarget -> Ecosystem -> Maybe MirrorPublish
mirrorPublishFor env deps targets eco = do
    target <- Map.lookup eco targets
    adapter <- adapterFor eco
    pure (newMirrorPublish (mirrorTransportFor env deps target) (ptMirrorUrl target) (publishCodec (adapterPublish adapter)))

{- | The shared mirror-write transport for one mount: the trusted private-origin
manager, the target's credential mint, and the mount's __own__ 'pdLimits' as the
probe's response bound. The presence probe therefore reads under the same
boot-computed, operator-overridable bound every other metadata read on the mount
honours. The shipped metadata-path default would not do: a larger mirror packument
would silently overrun it and defeat duplicate suppression.
-}
mirrorTransportFor :: Env -> PackumentDeps -> PublishTarget -> MirrorTransport
mirrorTransportFor env deps target =
    MirrorTransport
        { ptManager = envPrivateManager env
        , ptMintToken = Just . authSecret <$> currentToken (ptCredentials target)
        , ptLimits = pdLimits deps
        }

{- Build one mount's worker bundle from its packument-serve dependencies and its
married publish capability. The bundle takes:

  * The single-version resolver over the guarded public origin through the shared
    metadata cache. This is the same fetch-and-project the serve path runs.
  * The mount's prepared rules, its configured integrity floor, and its tarball-host
    gate.
  * Its ecosystem's artifact request formation, its injected clock, and the mirror
    write.

Every decision input comes from the mount's __own__ 'PackumentDeps', so the ingest
decision cannot diverge from the serve decision.

This builds the metadata client through the same injected constructor the serve path
uses ('pdNewMetadataClient', over the same shared manager 'srPublicManager' is wired
to). It is anonymous, so no client credential reaches the public origin, and the host
allowlist gates it with certificate validation authenticating the dialled host. The
bundle elides the client's own failure and dropped-entry logs, because the worker
logs its own re-evaluation outcome per job. The upstream-fetch metrics still record
through the shared instruments. -}
workerPolicyFor :: Env -> PackumentDeps -> MirrorPublish -> Int -> WorkerPolicy
workerPolicyFor env deps publish artifactMaxBytes =
    WorkerPolicy
        { wpResolveVersion = fetchVersionDetails client
        , wpRules = pdRules deps
        , wpMinIntegrity = pdMinIntegrity deps
        , wpArtifactHostHonoured =
            -- The same host-gate composition the serve path applies before its public
            -- artifact fetch. It is closed against the public upstream authority: the
            -- reference host:port the public leg gates dist.tarball targets by.
            tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps))
        , -- The mount's own request formation (the adapter's artifact capability,
          -- projected onto these deps at the composition root), so the worker fetches
          -- a job's bytes exactly as the serve path would.
          wpBuildArtifactRequest = pdBuildArtifactRequestByUrl deps
        , wpPublish = publish
        , -- The artifact fetch cap comes from the memory plan's mirror-artifact tenant,
          -- not the metadata-path default: a real tarball far exceeds the packument cap,
          -- while this cap bounds the transient publish envelope against the heap
          -- ceiling. The other limits (version count, nesting depth) stay at their
          -- defaults. They do not apply to an opaque tarball.
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
