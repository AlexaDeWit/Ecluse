{- | The ecosystem tag.

'Ecosystem' is the shared vocabulary the rest of the system dispatches on: the
package vocabulary ("Ecluse.Package") records it in a @PackageName@, the version
engine ("Ecluse.Version") selects a per-ecosystem parser by it, and the registry
adapters (later) are chosen by it.

It lives in its own small module on purpose. It is a stable shared type imported
by several areas, and keeping it here breaks what would otherwise be an import
cycle between "Ecluse.Package" (whose @PackageDetails@ holds a @Version@) and
"Ecluse.Version" (whose parsers dispatch on the ecosystem) — exactly the
@.Types@-style extraction sanctioned by STYLE.md → "Module organization".
-}
module Ecluse.Ecosystem (
    Ecosystem (..),
) where

-- | The package ecosystem an identity, version, or snapshot belongs to.
data Ecosystem
    = Npm
    | PyPI
    | RubyGems
    deriving stock (Eq, Ord, Show)
