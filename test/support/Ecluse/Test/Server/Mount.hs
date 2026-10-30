-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared mount fixtures bind adapter capabilities and derive host gates from their origins.
Inert fixtures use closed loopback ports so accidental data-plane calls cannot reach upstreams.
-}
module Ecluse.Test.Server.Mount (
    serveDepsFor,
    inertDepsFor,
    npmServeDeps,
    pypiServeDeps,
    inertPackumentDeps,
    withPrivateBaseUrl,
    overPrivateBaseUrl,
    withMirrorPlan,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactHosts))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl, mkRegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..), pdMirror, pdPrivateBaseUrl, pdPublicBaseUrl)
import Ecluse.Core.Server.Encoding (MetadataCompression (GzipWhenAccepted))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit), mountUpstreams)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)

-- | A mount's serve dependencies over @adapter@, with the shipped defaults beside its parameters.
serveDepsFor :: RegistryAdapter -> Maybe RegistryUrl -> RegistryUrl -> MirrorServePlan -> [PreparedRule] -> IO UTCTime -> PackumentDeps
serveDepsFor adapter privateBaseUrl publicBaseUrl mirror rules clock =
    PackumentDeps
        { pdUpstreams = mountUpstreams (artifactHosts (adapterArtifact adapter)) privateBaseUrl publicBaseUrl mirror
        , pdFirstParty = const False
        , pdMountBaseUrl = "https://proxy.test"
        , pdRules = rules
        , pdAdditionalBlockedRanges = []
        , pdLimits = defaultLimits
        , pdInboundToken = Nothing
        , pdNow = clock
        , pdAdvisoryEtag = pure Nothing
        , pdNoteAdmission = const (pure True)
        , pdHelp = Nothing
        , pdMinIntegrity = defaultMinIntegrity
        , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
        , pdMetadata = adapterMetadata adapter
        , pdArtifact = adapterArtifact adapter
        , pdEgressUrl = mkRegistryUrl
        , pdMirrorState = Nothing
        , pdMetadataCompression = GzipWhenAccepted
        }

-- | 'serveDepsFor' over 'Ecluse.Core.Registry.Npm.Adapter.npmAdapter'.
npmServeDeps :: Maybe RegistryUrl -> RegistryUrl -> MirrorServePlan -> [PreparedRule] -> IO UTCTime -> PackumentDeps
npmServeDeps = serveDepsFor npmAdapter

-- | 'serveDepsFor' over 'Ecluse.Core.Registry.PyPI.Adapter.pypiAdapter'.
pypiServeDeps :: Maybe RegistryUrl -> RegistryUrl -> MirrorServePlan -> [PreparedRule] -> IO UTCTime -> PackumentDeps
pypiServeDeps = serveDepsFor pypiAdapter

-- | Closed loopback origins prevent fixture reads from reaching an upstream.
inertDepsFor :: RegistryAdapter -> PackumentDeps
inertDepsFor adapter =
    (serveDepsFor adapter (Just closedPort) closedPort (MirrorOnAdmit closedPort) [] (pure fixedNow))
        { pdMountBaseUrl = "http://proxy.invalid"
        }
  where
    -- Port 1 is reserved and never listening, so a fetch through these deps fails to
    -- connect rather than reaching anything.
    closedPort = loopbackRegistryUrl "http://localhost:1"

    fixedNow :: UTCTime
    fixedNow = UTCTime (fromGregorian 2020 1 1) 0

-- | 'inertDepsFor' over 'Ecluse.Core.Registry.Npm.Adapter.npmAdapter'.
inertPackumentDeps :: PackumentDeps
inertPackumentDeps = inertDepsFor npmAdapter

-- | Rebind a fixture's upstreams with the private base URL replaced, dropping declared artifact hosts.
withPrivateBaseUrl :: Maybe RegistryUrl -> PackumentDeps -> PackumentDeps
withPrivateBaseUrl privateBaseUrl = rebind (const privateBaseUrl) id

-- | 'withPrivateBaseUrl' deriving the new private base URL from the old. A mount with none keeps none.
overPrivateBaseUrl :: (RegistryUrl -> RegistryUrl) -> PackumentDeps -> PackumentDeps
overPrivateBaseUrl f = rebind (fmap f) id

-- | Rebind a fixture's upstreams with the mirror serve plan replaced, dropping declared artifact hosts.
withMirrorPlan :: MirrorServePlan -> PackumentDeps -> PackumentDeps
withMirrorPlan mirror = rebind id (const mirror)

-- The one rebinding point every fixture tweak routes through, so the gate derives from what the
-- result carries. The cluster does not carry the ecosystem hosts, so a rebind drops them.
rebind :: (Maybe RegistryUrl -> Maybe RegistryUrl) -> (MirrorServePlan -> MirrorServePlan) -> PackumentDeps -> PackumentDeps
rebind onPrivate onMirror d =
    d{pdUpstreams = mountUpstreams [] (onPrivate (pdPrivateBaseUrl d)) (pdPublicBaseUrl d) (onMirror (pdMirror d))}
