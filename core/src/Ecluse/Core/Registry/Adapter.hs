-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Registered ecosystem capabilities, independent of which mounts the operator configures.
module Ecluse.Core.Registry.Adapter (
    -- * The capability record
    RegistryAdapter (..),
    AdapterServe (..),

    -- * Registration
    adapterFor,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Cargo.Adapter (cargoAdapter)
import Ecluse.Core.Registry.Cargo.Metadata (cargoChargeFactors)
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter)
import Ecluse.Core.Registry.RubyGems.Metadata (rubyGemsChargeFactors)

-- | Resolve adapters with native capabilities and recorded resource factors.
adapterFor :: Ecosystem -> Maybe RegistryAdapter
adapterFor = \case
    Npm -> Just npmAdapter
    PyPI -> Just pypiAdapter
    Cargo -> Just (cargoAdapter cargoChargeFactors)
    RubyGems -> Just (rubyGemsAdapter rubyGemsChargeFactors)
