-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ecosystem tag, the shared vocabulary the rest of the system dispatches on. The
package vocabulary ("Ecluse.Core.Package") keys a @PackageName@ by it, the version
engine ("Ecluse.Core.Version") selects a per-ecosystem parser by it, the registry
adapters dispatch on it, and configuration keys a mount by it.

It sits in its own module to break the import cycle between "Ecluse.Core.Package"
(whose @PackageDetails@ holds a @Version@) and "Ecluse.Core.Version" (whose parsers
dispatch on the ecosystem).
-}
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
    deriving stock (Eq, Generic, Ord, Show)

-- Derived from Generic so the wire-table coverage test needs no hand-maintained list.
instance Universe Ecosystem where universe = universeGeneric

instance WireVocab Ecosystem where
    wireKind = "ecosystem"
    wireTable =
        (Npm, "npm")
            :| [ (PyPI, "pypi")
               , (RubyGems, "rubygems")
               ]

{- | The canonical wire\/config name of an ecosystem: the key an operator writes a
@mounts@ object under, and the inverse of 'parseEcosystem'.

>>> ecosystemName Npm
"npm"
-}
ecosystemName :: Ecosystem -> Text
ecosystemName = renderWire

{- | Parse an 'Ecosystem' from its wire name, 'Nothing' for one the build does not
serve. The config decoder reads the document's @mounts@ keys with it, and rejects an
unknown key loudly rather than skipping it (see "Ecluse.Config").

>>> parseEcosystem "npm"
Just Npm

>>> parseEcosystem "cargo"
Nothing
-}
parseEcosystem :: Text -> Maybe Ecosystem
parseEcosystem = lookupWire

{- | The path prefix a mount serves under, derived from its ecosystem and never
operator-configured, so it can neither collide nor be mistyped. A root mount is unrepresentable.

>>> prefixFor Npm
"npm" :| []
-}
prefixFor :: Ecosystem -> NonEmpty Text
prefixFor eco = ecosystemName eco :| []
