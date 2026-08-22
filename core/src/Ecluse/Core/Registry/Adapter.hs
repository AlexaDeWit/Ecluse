-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ecosystem adapter registry: resolve an 'Ecosystem' to its registered
capability record.

The registry answers "which ecosystems does this binary support?", independent of
anything an operator configures. That keeps three situations distinct:

* An ecosystem the build does not support resolves to 'Nothing' here.
* A supported ecosystem with no mount configured is not activated. No error,
  nothing served under its prefix.
* A __configured__ ecosystem that resolves to 'Nothing' is the composition
  root's loud missing-adapter boot error, never a half-wired mount.

'adapterFor' is a total case over the closed 'Ecosystem' sum, one arm per
constructor. A new ecosystem is therefore additive: it brings its own adapter
module (npm's is "Ecluse.Core.Registry.Npm.Adapter") and gains an arm here. It
touches neither another ecosystem's code nor the core engine. Only the composition
root consumes the adapter. The root resolves it once per activation and projects
each consuming pipeline's dependency record from its fields. The pipelines never
import this module.
-}
module Ecluse.Core.Registry.Adapter (
    -- * The capability record
    RegistryAdapter (..),
    AdapterServe (..),
    AdapterMetadata (..),
    AdapterArtifact (..),
    AdapterPublish (..),

    -- * Registration
    adapterFor,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter.Types
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)

{- | Resolve an ecosystem to its registered 'RegistryAdapter', or 'Nothing' for
one this build carries no adapter for. Total over the closed 'Ecosystem' sum,
every arm explicit, so an added ecosystem is a compiler-visible arm here rather
than a fall-through.
-}
adapterFor :: Ecosystem -> Maybe RegistryAdapter
adapterFor = \case
    Npm -> Just npmAdapter
    PyPI -> Nothing
    RubyGems -> Nothing
