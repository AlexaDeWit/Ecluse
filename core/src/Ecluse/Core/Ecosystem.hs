-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Ecosystem tags shared by package identity, version parsing and mount configuration.
module Ecluse.Core.Ecosystem (
    Ecosystem (..),
    ecosystemName,
    parseEcosystem,
    prefixFor,
) where

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

import Ecluse.Core.Wire (WireVocab (..), lookupWire, renderWire)

-- | The package ecosystem an identity, version, or snapshot belongs to.
data Ecosystem
    = Npm
    | PyPI
    | RubyGems
    | Cargo
    deriving stock (Eq, Generic, Ord, Show)

instance Universe Ecosystem where universe = universeGeneric

instance WireVocab Ecosystem where
    wireKind = "ecosystem"
    wireTable =
        (Npm, "npm")
            :| [ (PyPI, "pypi")
               , (RubyGems, "rubygems")
               , (Cargo, "cargo")
               ]

-- | The canonical wire and configuration name of an ecosystem.
ecosystemName :: Ecosystem -> Text
ecosystemName = renderWire

-- | Parse the canonical wire name, refusing an unknown ecosystem.
parseEcosystem :: Text -> Maybe Ecosystem
parseEcosystem = lookupWire

-- | Derive the single mount segment from the ecosystem name.
prefixFor :: Ecosystem -> NonEmpty Text
prefixFor eco = ecosystemName eco :| []
