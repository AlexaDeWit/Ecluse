-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ecosystem tag.

'Ecosystem' is the shared vocabulary the rest of the system dispatches on. The package
vocabulary ("Ecluse.Core.Package") records it in a @PackageName@. The version engine
("Ecluse.Core.Version") selects a per-ecosystem parser by it. The registry adapters
("Ecluse.Core.Registry.Adapter") dispatch on it. Configuration keys a mount by it, and
derives that mount's path prefix from it ('prefixFor').

It lives in its own small module on purpose. It is a stable shared type several areas
import. Keeping it here prevents an import cycle between "Ecluse.Core.Package" (whose
@PackageDetails@ holds a @Version@) and "Ecluse.Core.Version" (whose parsers dispatch
on the ecosystem). That is exactly the @.Types@-style extraction STYLE.md → "Module
organization" sanctions.
-}
module Ecluse.Core.Ecosystem (
    Ecosystem (..),
    ecosystemName,
    parseEcosystem,
    prefixFor,
) where

-- | The package ecosystem an identity, version, or snapshot belongs to.
data Ecosystem
    = Npm
    | PyPI
    | RubyGems
    deriving stock (Eq, Ord, Show)

{- | The canonical wire\/config name of an ecosystem: the key an operator writes a
@mounts@ object under, and the inverse of 'parseEcosystem'.

>>> ecosystemName Npm
"npm"
-}
ecosystemName :: Ecosystem -> Text
ecosystemName = \case
    Npm -> "npm"
    PyPI -> "pypi"
    RubyGems -> "rubygems"

{- | Parse an 'Ecosystem' from its wire name, 'Nothing' for one the build does not
serve. The config decoder reads the document's @mounts@ keys with it, and rejects an
unknown key loudly rather than skipping it (see "Ecluse.Config").

>>> parseEcosystem "npm"
Just Npm

>>> parseEcosystem "cargo"
Nothing
-}
parseEcosystem :: Text -> Maybe Ecosystem
parseEcosystem = \case
    "npm" -> Just Npm
    "pypi" -> Just PyPI
    "rubygems" -> Just RubyGems
    _ -> Nothing

{- | The path prefix a mount serves under, derived from its ecosystem and never
operator-configured, so it can neither collide nor be mistyped. A root mount is unrepresentable.

>>> prefixFor Npm
"npm" :| []
-}
prefixFor :: Ecosystem -> NonEmpty Text
prefixFor eco = ecosystemName eco :| []
