-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI route contracts shared by serving and OpenAPI generation.
Project names must be canonical, and distribution filenames must match their project.
"Ecluse.Core.Registry.PyPI.Route.Internal" holds the table and everything it is built from.
-}
module Ecluse.Core.Registry.PyPI.Route (
    -- * The mount's router
    pypiRouter,

    -- * The table, as OpenAPI reads it
    pypiRouteSpecs,

    -- * The served file URL (rendered from the route that claims it)
    distributionPath,
) where

import Ecluse.Core.Registry.PyPI.Route.Internal (distributionPath, pypiRouteSpecs, pypiRouter)
