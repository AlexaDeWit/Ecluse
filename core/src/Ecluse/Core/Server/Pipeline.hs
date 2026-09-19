-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy's data-plane entry point: the handlers for packument merges (@GET \/{pkg}@),
artifact relays (@GET \/{pkg}\/-\/{file}.tgz@), and first-party publishes (@PUT \/{pkg}@). An
ecosystem's route table names them one module at a time, so nothing but its own spec imports
this hub. It stays as the named entry point the route tables and documents cross-reference.

The handlers name no ecosystem. A registry's metadata client, packument assembly, and
artifact-request formation reach them as injected capabilities on
'Ecluse.Core.Server.Context.PackumentDeps', so a router maps its own routes onto whichever
handlers apply and names its own actions for the rest.
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
