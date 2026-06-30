{- | The proxy's data-plane entry point for package and artifact routes.

This module re-exports the top-level handlers for packument merges (@GET \/{pkg}@),
artifact relays (@GET \/{pkg}\/-\/{file}.tgz@), and first-party publishes (@PUT \/{pkg}@).

== Ecosystem coupling

This is the __npm__ packument pipeline: it reaches for the npm registry
client, projection, and structural filter directly, so it is the one
serve-path module that depends on a concrete adapter. The coupling is
expedient, not intended — the agnostic handles that would let it dispatch through an
adapter (a per-adapter router, and an ecosystem-neutral filter\/projection) would
let a second ecosystem reuse this orchestration unchanged.
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
