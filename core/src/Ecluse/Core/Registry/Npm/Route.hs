-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm router and OpenAPI description share one route table.
Reserved routes take precedence over package captures.
"Ecluse.Core.Registry.Npm.Route.Internal" holds the table and everything it is built from.
-}
module Ecluse.Core.Registry.Npm.Route (
    -- * The mount's router
    npmRouter,

    -- * The table, as OpenAPI reads it
    npmRouteSpecs,

    -- * The served artifact URL (rendered from the route that claims it)
    tarballPath,
) where

import Ecluse.Core.Registry.Npm.Route.Internal (npmRouteSpecs, npmRouter, tarballPath)
