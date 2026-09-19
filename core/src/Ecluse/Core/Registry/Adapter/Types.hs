-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The capability record an ecosystem registers ('RegistryAdapter') and the serve surface it
carries. A record holds no URL, credential, limit, or policy: it is a static fact of the build.
It sits apart from the registration ("Ecluse.Core.Registry.Adapter") as the cycle-breaking
@.Types@ split of docs/style.md 4.3, so an adapter module never imports that registry. The
embedded slices live in "Ecluse.Core.Registry.Adapter.Capability".
-}
module Ecluse.Core.Registry.Adapter.Types (
    -- * The capability record
    RegistryAdapter (..),

    -- * The serve surface
    AdapterServe (..),
) where

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterArtifact,
    AdapterMaintenance,
    AdapterMetadata,
    AdapterPublish,
    ProjectName,
 )
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Server.Context (MountRouter)
import Ecluse.Core.Server.RouteDescription (RouteSpec)

{- | One ecosystem's complete capability record, which the composition root wires every
consuming pipeline from. 'Ecluse.Core.Registry.Adapter.adapterFor' resolves it.
-}
data RegistryAdapter = RegistryAdapter
    { adapterEcosystem :: Ecosystem
    -- ^ The registry key must agree with it, so no record can register under a foreign ecosystem.
    , adapterServe :: AdapterServe
    -- ^ The web-facing serve surface: the route grammar and response contracts.
    , adapterMetadata :: AdapterMetadata
    -- ^ The metadata capability: the read-handle constructor and the packument assembly.
    , adapterArtifact :: AdapterArtifact
    -- ^ The artifact request formation, by filename and by authoritative URL.
    , adapterProjectName :: ProjectName
    {- ^ The ecosystem's own name parser, which every caller that turns a raw string into a
    'Ecluse.Core.Package.PackageName' reads.
    -}
    , adapterPublish :: Maybe AdapterPublish
    {- ^ 'Nothing' for an ecosystem this build writes nothing for, whose publish route then
    answers @405@ and whose declared write destination refuses the boot.
    -}
    , adapterMaintenance :: AdapterMaintenance
    -- ^ The store maintenance verbs. Either may be absent, and a Dredger then refuses the mount.
    }

{- | The ecosystem's web-facing serve surface. Both routing fields are derived from one
declarative route table, so the routed surface and the documented one cannot drift apart.
-}
data AdapterServe = AdapterServe
    { serveRouter :: MountRouter
    -- ^ Which path a mount-relative request names. An unrecognised one yields the default @404@.
    , serveRoutes :: NonEmpty RouteSpec
    -- ^ The same route table as data, which the OpenAPI spec ("Ecluse.Manifest") renders.
    , serveCredential :: CredentialMapping
    {- ^ How the mount recovers a client's credential and how Écluse carries one upstream. The
    neutral pipeline spells no scheme of its own.
    -}
    }
