-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test fixtures for a mount's serve dependencies.

The module name follows this support library's @Ecluse.X -> Ecluse.Test.X@ convention.

'npmServeDeps' is the one shared builder for an npm mount's 'PackumentDeps'. It fills the
standard production wiring once: the metadata-client, artifact-request, and assembly
capabilities, the derived tarball-host gate, and the policy defaults. Each call site
passes only its own axes: the two upstream base URLs, the mirror plan, the prepared
rules, and the clock. A call site also record-updates the few fields unique to it: the
mount base URL, the egress former, an inbound token. Every affected suite and the load
bench build their deps through it, so a 'PackumentDeps' schema change lands in one place.

'inertPackumentDeps' is a complete but __unreachable__ 'PackumentDeps': every upstream it
names is a closed port. A 'Ecluse.Core.Server.Context.MountBinding' always carries
packument dependencies. A mount exists only for an ecosystem with a registered adapter,
and the composition root builds the deps from that adapter. A spec that never drives the
data plane must therefore still supply them. This fixture is that supply: enough to bind
a mount, and nothing that answers.

A spec that /does/ drive the data plane builds its own deps through 'npmServeDeps'
against a live stub upstream. This fixture is for the specs that only care about routing,
the meta-routes, the edge gate, or the publish path.

'withPrivateBaseUrl', 'overPrivateBaseUrl', 'withMirrorPlan', and 'withEcosystemHosts'
are how a fixture varies an upstream. Each one rebinds the deps' whole upstream cluster
through 'mountUpstreams', so the tarball-host gate re-derives with the URL. A fixture
cannot express a stale gate, because the cluster's constructor is private and its
selectors are not exported. See "Ecluse.Core.Server.Upstream".
-}
module Ecluse.Test.Server.Mount (
    npmServeDeps,
    inertPackumentDeps,
    withPrivateBaseUrl,
    overPrivateBaseUrl,
    withMirrorPlan,
    withEcosystemHosts,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Core.Package.Merge (DivergencePolicy (Warn))
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedDocument, serialiseMergedDocument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..), pdMirror, pdPrivateBaseUrl, pdPublicBaseUrl)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit), mountUpstreams)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)

{- | An npm mount's serve dependencies with the standard production wiring filled once.
Only the per-site axes are parameters. Those are the two upstream base URLs, the mirror
plan, the prepared rule set, and the clock. The private upstream base URL is 'Nothing'
for a pure public gate. The builder binds the upstream cluster from those URLs, so the
tarball-host gate derives from them and cannot disagree with them.

The remaining varying fields carry defaults: the mount base URL, no inbound token, and
the production https-only egress former. A site record-updates just the ones it needs to
differ on. The egress former defaults to the production 'mkRegistryUrl'. The loopback dev
former lives behind the @dev-http-egress@ flag, and this support library is not built
with that flag. A hermetic-upstream site, built with the flag, record-updates
@pdEgressUrl@ to its loopback former.

The artifact-by-URL builder passes the origin base through for symmetry with the by-file
builder. The production former ignores it and fetches the authoritative @dist.tarball@
URL.
-}
npmServeDeps :: Maybe Text -> Text -> MirrorServePlan -> [PreparedRule] -> IO UTCTime -> PackumentDeps
npmServeDeps privateBaseUrl publicBaseUrl mirror rules clock =
    PackumentDeps
        { pdUpstreams = mountUpstreams [] privateBaseUrl publicBaseUrl mirror
        , pdMountBaseUrl = "https://proxy.test"
        , pdRules = rules
        , pdAdditionalBlockedRanges = []
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

{- | A mount's serve dependencies wired to nowhere. Every base URL is a closed loopback
port, the rule set is empty, and the clock is fixed. An empty rule set means the
deny-by-default engine admits nothing. It inherits the builder's production https-only
egress former, because it is production-faithful, not a live-upstream fixture.

It is complete enough to bind a 'Ecluse.Core.Server.Context.MountBinding'. It is inert
enough that a spec which does not test the data plane cannot reach an upstream by
accident. A packument or artifact request served through it fails to connect instead of
getting an answer.
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

{- | Rebind a fixture's upstreams with the private base URL replaced. The tarball-host
gate re-derives, because the one builder rebuilds the whole cluster. This rebind drops
any declared ecosystem artifact hosts, so a fixture that wants both applies
'withEcosystemHosts' last.
-}
withPrivateBaseUrl :: Maybe Text -> PackumentDeps -> PackumentDeps
withPrivateBaseUrl privateBaseUrl = rebind [] (const privateBaseUrl) id

{- | 'withPrivateBaseUrl' for a fixture that derives the new private base URL from the
old one, by rewriting a host for example. A mount with no private upstream stays without
one. It drops any declared ecosystem artifact hosts, as 'withPrivateBaseUrl' does.
-}
overPrivateBaseUrl :: (Text -> Text) -> PackumentDeps -> PackumentDeps
overPrivateBaseUrl f = rebind [] (fmap f) id

{- | Rebind a fixture's upstreams with the mirror serve plan replaced. It drops any
declared ecosystem artifact hosts, as 'withPrivateBaseUrl' does.
-}
withMirrorPlan :: MirrorServePlan -> PackumentDeps -> PackumentDeps
withMirrorPlan mirror = rebind [] id (const mirror)

{- | Rebind a fixture's upstreams declaring the given ecosystem artifact hosts, for a
test that exercises the ecosystem-host equivalence (the PyPI files-host shape). The
rebind carries the upstream URLs over unchanged. The hosts join the gate's allowlist.
-}
withEcosystemHosts :: [Text] -> PackumentDeps -> PackumentDeps
withEcosystemHosts ecosystemHosts = rebind ecosystemHosts id id

-- The one rebinding point every fixture tweak routes through. It rebuilds the cluster
-- from the given ecosystem hosts and the URL axes read back off the deps. The gate
-- therefore derives from what the result carries. The cluster does not carry the hosts,
-- so each rebind states them or drops them.
rebind :: [Text] -> (Maybe Text -> Maybe Text) -> (MirrorServePlan -> MirrorServePlan) -> PackumentDeps -> PackumentDeps
rebind ecosystemHosts onPrivate onMirror d =
    d{pdUpstreams = mountUpstreams ecosystemHosts (onPrivate (pdPrivateBaseUrl d)) (pdPublicBaseUrl d) (onMirror (pdMirror d))}
