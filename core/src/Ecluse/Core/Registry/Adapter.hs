-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ecosystem adapter registry: which ecosystems this build supports, independent of what an
operator configures. An unsupported ecosystem resolves to 'Nothing' here and an unconfigured one is
never activated, so the composition root can tell a missing adapter from an unconfigured mount.
-}
module Ecluse.Core.Registry.Adapter (
    -- * The capability record
    RegistryAdapter (..),
    AdapterServe (..),
    AdapterMetadata (..),
    AdapterArtifact (..),
    AdapterPublish (..),
    AdapterMaintenance (..),
    ProjectName,

    -- * Registration
    adapterFor,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterArtifact (..),
    AdapterMaintenance (..),
    AdapterMetadata (..),
    AdapterPublish (..),
    ProjectName,
 )
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)

{- | Resolve an ecosystem to its registered 'RegistryAdapter'. Every arm is explicit, so an added
'Ecosystem' surfaces here as a compiler error rather than a silent 'Nothing'.
-}
adapterFor :: Ecosystem -> Maybe RegistryAdapter
adapterFor = \case
    Npm -> Just npmAdapter
    PyPI -> Just pypiAdapter
    RubyGems -> Nothing
