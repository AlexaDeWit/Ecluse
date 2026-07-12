-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy's data-plane entry point for package and artifact routes.

This module re-exports the top-level handlers for packument merges (@GET \/{pkg}@),
artifact relays (@GET \/{pkg}\/-\/{file}.tgz@), and first-party publishes (@PUT \/{pkg}@).

== Ecosystem neutrality

These handlers name no ecosystem. A registry's metadata client, its packument
assembly, and its artifact-request formation reach them as __injected capabilities__
on 'Ecluse.Core.Server.Context.PackumentDeps' ('pdNewMetadataClient', 'pdAssemble',
'pdBuildArtifactRequestByFile', 'pdBuildArtifactRequestByUrl'), which the composition
root projects from the mount's 'Ecluse.Core.Registry.Adapter.Types.RegistryAdapter'.
The imports here reach only the __agnostic__ protocol boundary
("Ecluse.Core.Registry", "Ecluse.Core.Registry.Metadata").

So the orchestration is reusable across registries whose URL grammars have nothing in
common: an ecosystem's router ("Ecluse.Core.Server.Context.MountRouter") maps its own
routes onto whichever of these handlers apply, and names its own actions for the
routes that have no counterpart here.
-}
module Ecluse.Core.Server.Pipeline (
    -- * The packument handler
    servePackument,
    headPackument,

    -- * The tarball handler
    serveTarball,
    headTarball,

    -- * The first-party publish handler
    servePublish,
) where

import Ecluse.Core.Server.Pipeline.Packument
import Ecluse.Core.Server.Pipeline.Publish
import Ecluse.Core.Server.Pipeline.Tarball
