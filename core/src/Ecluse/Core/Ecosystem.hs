{- | The ecosystem tag.

'Ecosystem' is the shared vocabulary the rest of the system dispatches on: the
package vocabulary ("Ecluse.Core.Package") records it in a @PackageName@, the version
engine ("Ecluse.Core.Version") selects a per-ecosystem parser by it, the registry
adapters (later) are chosen by it, and configuration both __keys a mount__ by it
and __derives that mount's path prefix__ from it ('prefixFor').

It lives in its own small module on purpose. It is a stable shared type imported
by several areas, and keeping it here breaks what would otherwise be an import
cycle between "Ecluse.Core.Package" (whose @PackageDetails@ holds a @Version@) and
"Ecluse.Core.Version" (whose parsers dispatch on the ecosystem) -- exactly the
@.Types@-style extraction sanctioned by STYLE.md → "Module organization".
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

{- | The canonical wire\/config name of an ecosystem -- the key a @mounts@ object
is written under and the inverse of 'parseEcosystem'.

>>> ecosystemName Npm
"npm"
-}
ecosystemName :: Ecosystem -> Text
ecosystemName = \case
    Npm -> "npm"
    PyPI -> "pypi"
    RubyGems -> "rubygems"

{- | Parse an 'Ecosystem' from its wire name, 'Nothing' for one the build does not
serve. Used to decode the config document's @mounts@ keys, where an unknown key is
rejected loudly rather than skipped (see "Ecluse.Config").

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

{- | The path prefix a mount serves under, __derived__ from its ecosystem (npm →
@\/npm@, PyPI → @\/pypi@) and never operator-configured, so a prefix can neither
collide nor be mistyped (see @docs\/architecture\/hosting.md@ → "Mounts"). A
'NonEmpty' list of path segments: every registry is path-mounted, so a root mount
is unrepresentable.

>>> prefixFor Npm
"npm" :| []
-}
prefixFor :: Ecosystem -> NonEmpty Text
prefixFor eco = ecosystemName eco :| []
