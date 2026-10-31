-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The Cargo sparse registry and its local archive coordinates share one route table.
module Ecluse.Core.Registry.Cargo.Route (
    cargoRouter,
    cargoRouteSpecs,
) where

import Ecluse.Core.Registry.Cargo.Route.Internal (cargoRouteSpecs, cargoRouter)
