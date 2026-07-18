-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test fixtures for a mount's serve dependencies.

This mirrors the module under test, under the @Ecluse.X -> Ecluse.Test.X@ convention this
support library follows.

'inertPackumentDeps' is a complete but __unreachable__ 'PackumentDeps': every upstream it
names is a closed port. A 'MountBinding' always carries packument dependencies (a mount
exists only for an ecosystem with a registered adapter, and the composition root builds the
deps from that adapter), so a spec that is not exercising the data plane at all still has to
supply them. This is what it supplies: enough to bind a mount, and nothing that will answer.

A spec that /does/ drive the data plane builds its own deps against a live stub upstream;
this fixture is for the specs that only care about routing, the meta-routes, the edge gate,
or the publish path.
-}
module Ecluse.Test.Server.Mount (
    inertPackumentDeps,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Core.Package.Merge (DivergencePolicy (Warn))
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig))
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByFile, artifactRequestByUrl)
import Ecluse.Core.Security (defaultLimits, tarballHostGate)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Context (MirrorServePlan (MirrorOnAdmit), PackumentDeps (..))
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)

{- | A mount's serve dependencies wired to nowhere: every base URL is a closed loopback
port, the rule set is empty (so the deny-by-default engine admits nothing), and the clock is
fixed.

Complete enough to bind a 'Ecluse.Core.Server.Context.MountBinding', inert enough that a
spec which is not testing the data plane cannot accidentally reach an upstream. A packument
or artifact request served through it fails to connect rather than being answered.
-}
inertPackumentDeps :: PackumentDeps
inertPackumentDeps =
    PackumentDeps
        { pdPrivateBaseUrl = Just privateUrl
        , pdPublicBaseUrl = publicUrl
        , pdMountBaseUrl = "http://proxy.invalid"
        , pdMirror = MirrorOnAdmit mirrorUrl
        , pdRules = []
        , pdAdditionalBlockedRanges = []
        , pdTarballHostGate = tarballHostGate [] (Just privateUrl) publicUrl (Just mirrorUrl)
        , pdLimits = defaultLimits
        , pdInboundToken = Nothing
        , pdNow = pure fixedNow
        , pdAdvisoryEtag = pure Nothing
        , pdHelp = Nothing
        , pdMinIntegrity = defaultMinIntegrity
        , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
        , pdDivergencePolicy = Warn
        , pdNewMetadataClient = \tracing metrics upstream caching logFailure logInvalid logFetch limits manager baseUrl token ->
            newNpmMetadataClient tracing metrics upstream caching logFailure logInvalid logFetch (NpmClientConfig baseUrl manager token limits)
        , pdBuildArtifactRequestByFile = \_limits _manager base token -> artifactRequestByFile base token
        , pdBuildArtifactRequestByUrl = \_limits _manager _base token -> artifactRequestByUrl "" token
        , pdAssemble = assembleMergedPackument
        , pdEgressUrl = mkRegistryUrl
        }
  where
    -- Port 1 is reserved and never listening, so a fetch through these deps fails to
    -- connect rather than reaching anything.
    privateUrl = "http://localhost:1"
    publicUrl = "http://localhost:1"
    mirrorUrl = "http://localhost:1"

    fixedNow :: UTCTime
    fixedNow = UTCTime (fromGregorian 2020 1 1) 0
