-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test fixtures for a mount's serve dependencies.

The module name follows this support library's @Ecluse.X -> Ecluse.Test.X@ convention.

'npmServeDeps' is the one shared builder for an npm mount's 'PackumentDeps'. It fills the
standard production wiring once: npm's own metadata and artifact capability records, the
derived tarball-host gate, and the policy defaults. Each call site passes only its own axes,
the two upstream base URLs, the mirror plan, the prepared rules, and the clock, and
record-updates the few fields unique to it: the mount base URL, the egress former, an inbound
token. Every affected suite and the load bench build their deps through it, so a
'PackumentDeps' schema change lands in one place.

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
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl, mkRegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..), pdMirror, pdPrivateBaseUrl, pdPublicBaseUrl)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit), mountUpstreams)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)

{- | An npm mount's serve dependencies over 'Ecluse.Core.Registry.Npm.Adapter.npmAdapter'.
The upstreams, mirror plan, rules, and clock are parameters, and the rest carry defaults.
-}
npmServeDeps :: Maybe RegistryUrl -> RegistryUrl -> MirrorServePlan -> [PreparedRule] -> IO UTCTime -> PackumentDeps
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
        , pdMetadata = adapterMetadata npmAdapter
        , pdArtifact = adapterArtifact npmAdapter
        , pdEgressUrl = mkRegistryUrl
        }

{- | A mount's serve dependencies wired to nowhere: a closed loopback port for every base URL, an
empty rule set, and a fixed clock. It is complete enough to bind a
'Ecluse.Core.Server.Context.MountBinding', but a packument or artifact request through it fails to
connect instead of reaching an upstream.
-}
inertPackumentDeps :: PackumentDeps
inertPackumentDeps =
    (npmServeDeps (Just closedPort) closedPort (MirrorOnAdmit closedPort) [] (pure fixedNow))
        { pdMountBaseUrl = "http://proxy.invalid"
        }
  where
    -- Port 1 is reserved and never listening, so a fetch through these deps fails to
    -- connect rather than reaching anything.
    closedPort = loopbackRegistryUrl "http://localhost:1"

    fixedNow :: UTCTime
    fixedNow = UTCTime (fromGregorian 2020 1 1) 0

{- | Rebind a fixture's upstreams with the private base URL replaced. The rebind drops any declared
ecosystem artifact hosts, so a fixture that wants both applies 'withEcosystemHosts' last.
-}
withPrivateBaseUrl :: Maybe RegistryUrl -> PackumentDeps -> PackumentDeps
withPrivateBaseUrl privateBaseUrl = rebind [] (const privateBaseUrl) id

{- | 'withPrivateBaseUrl' deriving the new private base URL from the old. A mount with no
private upstream stays without one, and declared ecosystem artifact hosts are dropped.
-}
overPrivateBaseUrl :: (RegistryUrl -> RegistryUrl) -> PackumentDeps -> PackumentDeps
overPrivateBaseUrl f = rebind [] (fmap f) id

{- | Rebind a fixture's upstreams with the mirror serve plan replaced. It drops any
declared ecosystem artifact hosts, as 'withPrivateBaseUrl' does.
-}
withMirrorPlan :: MirrorServePlan -> PackumentDeps -> PackumentDeps
withMirrorPlan mirror = rebind [] id (const mirror)

{- | Rebind a fixture's upstreams declaring the given ecosystem artifact hosts, which join the
gate's allowlist. The upstream URLs carry over unchanged.
-}
withEcosystemHosts :: [Text] -> PackumentDeps -> PackumentDeps
withEcosystemHosts ecosystemHosts = rebind ecosystemHosts id id

-- The one rebinding point every fixture tweak routes through, so the gate derives from what the
-- result carries. The cluster does not carry the ecosystem hosts, so each rebind states or drops
-- them.
rebind :: [Text] -> (Maybe RegistryUrl -> Maybe RegistryUrl) -> (MirrorServePlan -> MirrorServePlan) -> PackumentDeps -> PackumentDeps
rebind ecosystemHosts onPrivate onMirror d =
    d{pdUpstreams = mountUpstreams ecosystemHosts (onPrivate (pdPrivateBaseUrl d)) (pdPublicBaseUrl d) (onMirror (pdMirror d))}
