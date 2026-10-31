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
    uploadEndpointFor,
    firstPartyFor,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishUploadEndpoint), UploadEndpointRequirement)
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Cargo.Adapter (cargoAdapter)
import Ecluse.Core.Registry.Cargo.Metadata (cargoChargeFactors)
import Ecluse.Core.Registry.FirstParty (FirstPartyDeclaration)
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

-- | Whether an ecosystem uploads at its read root or at a separate endpoint. 'Nothing' without publish support.
uploadEndpointFor :: Ecosystem -> Maybe UploadEndpointRequirement
uploadEndpointFor eco = publishUploadEndpoint <$> (adapterPublish =<< adapterFor eco)

-- | How an ecosystem's @firstParty@ entries read and match. 'Nothing' for an unregistered ecosystem.
firstPartyFor :: Ecosystem -> Maybe FirstPartyDeclaration
firstPartyFor eco = adapterFirstParty <$> adapterFor eco
