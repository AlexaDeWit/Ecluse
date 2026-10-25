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
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)

-- | Resolve adapters with native capabilities and calibrated resource factors.
adapterFor :: Ecosystem -> Maybe RegistryAdapter
adapterFor = \case
    Npm -> Just npmAdapter
    PyPI -> Just pypiAdapter
    Cargo -> Nothing
    RubyGems -> Nothing
