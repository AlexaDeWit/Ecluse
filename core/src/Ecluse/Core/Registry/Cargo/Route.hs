-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo sparse views and local archive coordinates share one route table.
module Ecluse.Core.Registry.Cargo.Route (
    cargoRouter,
    cargoRouteSpecs,
    cargoArtifactPath,
) where

import Ecluse.Core.Registry.Cargo.Route.Internal (cargoArtifactPath, cargoRouteSpecs, cargoRouter)
