-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test fixtures for a mount's serve dependencies.

This mirrors the module under test, under the @Ecluse.X -> Ecluse.Test.X@ convention this
support library follows.

'npmServeDeps' is the one shared builder for an npm mount's 'PackumentDeps'. It fills the
standard production wiring once (the metadata-client, artifact-request, and assembly
capabilities, the derived tarball-host gate, and the policy defaults), leaving each call
site to pass only its own axes (the two upstream base URLs, the mirror plan, the prepared
rules, and the clock) and to record-update the few fields unique to it (the mount base URL,
the egress former, an inbound token). Every affected suite and the load bench build their
deps through it, so a 'PackumentDeps' schema change lands in one place.

'inertPackumentDeps' is a complete but __unreachable__ 'PackumentDeps': every upstream it
names is a closed port. A 'Ecluse.Core.Server.Context.MountBinding' always carries packument
dependencies (a mount exists only for an ecosystem with a registered adapter, and the
composition root builds the deps from that adapter), so a spec that is not exercising the
data plane at all still has to supply them. This is what it supplies: enough to bind a
mount, and nothing that will answer.

A spec that /does/ drive the data plane builds its own deps through 'npmServeDeps' against a
live stub upstream; this fixture is for the specs that only care about routing, the
meta-routes, the edge gate, or the publish path.

'consistentGate' / 'consistentGateWith' re-derive the cached tarball-host gate after a test
record-updates one of the URL fields it projects from, so the gate never goes stale.
-}
module Ecluse.Test.Server.Mount (
    npmServeDeps,
    inertPackumentDeps,
    mirrorUrlOf,
    consistentGate,
    consistentGateWith,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Core.Package.Merge (DivergencePolicy (Warn))
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedDocument, serialiseMergedDocument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (defaultLimits, tarballHostGate)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Context (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite), PackumentDeps (..))
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)

{- | An npm mount's serve dependencies with the standard production wiring filled once,
parameterised only on the per-site axes: the private upstream base URL ('Nothing' for a
pure public gate), the public upstream base URL, the mirror plan, the prepared rule set,
and the clock. The tarball-host gate is derived from those URLs, so it never goes stale.

The remaining varying fields carry sensible defaults (the mount base URL, no inbound
token, and the production https-only egress former); a site record-updates just the ones
it needs to differ on. The egress former defaults to the production 'mkRegistryUrl'
because the loopback dev former lives behind the @dev-http-egress@ flag, which this
support library is not built with; a hermetic-upstream site (built with the flag)
record-updates @pdEgressUrl@ to its loopback former. The artifact-by-URL builder passes
the origin base through for symmetry with the by-file builder, though the production
former ignores it (the authoritative @dist.tarball@ URL is what it fetches).
-}
npmServeDeps :: Maybe Text -> Text -> MirrorServePlan -> [PreparedRule] -> IO UTCTime -> PackumentDeps
npmServeDeps privateBaseUrl publicBaseUrl mirror rules clock =
    PackumentDeps
        { pdPrivateBaseUrl = privateBaseUrl
        , pdPublicBaseUrl = publicBaseUrl
        , pdMountBaseUrl = "https://proxy.test"
        , pdMirror = mirror
        , pdRules = rules
        , pdAdditionalBlockedRanges = []
        , pdTarballHostGate = tarballHostGate [] privateBaseUrl publicBaseUrl (mirrorUrlOf mirror)
        , pdLimits = defaultLimits
        , pdInboundToken = Nothing
        , pdNow = clock
        , pdAdvisoryEtag = pure Nothing
        , pdHelp = Nothing
        , pdMinIntegrity = defaultMinIntegrity
        , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
        , pdDivergencePolicy = Warn
        , pdNewMetadataClient = \tracing metrics upstream caching logFailure logInvalid logFetch limits manager baseUrl token ->
            newNpmMetadataClient tracing metrics upstream caching logFailure logInvalid logFetch (NpmClientConfig baseUrl manager token limits)
        , pdBuildArtifactRequestByFile = \_limits _manager base token -> artifactRequestByFile base token
        , pdBuildArtifactRequestByUrl = \_limits _manager base token -> artifactRequestByUrl base token
        , pdAssemble = assembleMergedDocument
        , pdSerialise = serialiseMergedDocument
        , pdEgressUrl = mkRegistryUrl
        }

{- | A mount's serve dependencies wired to nowhere: every base URL is a closed loopback
port, the rule set is empty (so the deny-by-default engine admits nothing), and the clock is
fixed. It inherits the builder's production https-only egress former, since it is
production-faithful, not a live-upstream fixture.

Complete enough to bind a 'Ecluse.Core.Server.Context.MountBinding', inert enough that a
spec which is not testing the data plane cannot accidentally reach an upstream. A packument
or artifact request served through it fails to connect rather than being answered.
-}
inertPackumentDeps :: PackumentDeps
inertPackumentDeps =
    (npmServeDeps (Just closedPort) closedPort (MirrorOnAdmit closedPort) [] (pure fixedNow))
        { pdMountBaseUrl = "http://proxy.invalid"
        }
  where
    -- Port 1 is reserved and never listening, so a fetch through these deps fails to
    -- connect rather than reaching anything.
    closedPort = "http://localhost:1"

    fixedNow :: UTCTime
    fixedNow = UTCTime (fromGregorian 2020 1 1) 0

-- | The mirror-target URL a serve plan carries, or 'Nothing' for a serve-only mount.
mirrorUrlOf :: MirrorServePlan -> Maybe Text
mirrorUrlOf = \case
    MirrorOnAdmit url -> Just url
    NoMirrorWrite -> Nothing

{- | Re-derive the precomputed tarball-host gate from a deps value's (possibly
overridden) upstream URLs, so a test that record-updates @pdPrivateBaseUrl@,
@pdPublicBaseUrl@, or @pdMirror@ keeps @pdTarballHostGate@ consistent. The gate is
a cached projection of those three fields (the composition root builds it once), so a
bare record update would leave it stale; the override harness applies this after any tweak.
-}
consistentGate :: PackumentDeps -> PackumentDeps
consistentGate = consistentGateWith []

{- | 'consistentGate' with adapter-declared ecosystem artifact hosts, for a test that
exercises the ecosystem-host equivalence (the PyPI files-host shape).
-}
consistentGateWith :: [Text] -> PackumentDeps -> PackumentDeps
consistentGateWith ecosystemHosts d =
    d{pdTarballHostGate = tarballHostGate ecosystemHosts (pdPrivateBaseUrl d) (pdPublicBaseUrl d) (mirrorUrlOf (pdMirror d))}
