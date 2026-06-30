{- | Outbound-request and response-bound guards for the proxy's data plane.

Écluse builds outbound HTTP requests from two untrusted sources — __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@) — and then parses whatever an upstream
returns. This module is the pure guard layer that keeps those steps from being
steered or exhausted by hostile input. It defends three boundaries:
-}
module Ecluse.Core.Security.Url (
    -- * Identifier → URL safety
    upstreamUrlFor,
    UrlError (..),
) where

import Data.Text qualified as T

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Server.Route (encodeComponent, isSafeComponent)
import Ecluse.Core.Text (joinUrlPath)

-- ── identifier → URL safety ──────────────────────────────────────────────────

-- | Why building an upstream URL from an identifier was refused.
data UrlError
    = {- | A name component (scope or base name) is unsafe to interpolate — see
      'Ecluse.Core.Server.Route.isSafeComponent'. Carries the offending component.
      -}
      UnsafeComponent Text
    | -- | The configured base URL is empty, so no URL can be formed.
      EmptyBaseUrl
    deriving stock (Eq, Show)

{- | Build an upstream URL for a package from a configured base URL and an
__already-parsed__ 'PackageName'.

This is the only sanctioned way to derive an upstream URL for a package: the
target is @{baseUrl}\/{path}@, where @path@ is built from the package's structural
components and @baseUrl@ is __configuration__, never a client-supplied path. The
client never chooses the host or the path prefix — only which (validated) package
— so @..\/@ traversal, an encoded slash, an absolute URL, or a CRLF in the original
request cannot steer the fetch elsewhere (see the module header).

The path is built with two complementary defences. First, although a 'PackageName'
is normally produced by the router's already-safe parse, its smart constructor does
no validation, so this __re-checks every structural component__ (scope and base
name) with the router's own 'Ecluse.Core.Server.Route.isSafeComponent' — a name carrying
a @\'\/\'@, @\'\\\\\'@, control character, or a @"."@\/@".."@ component is refused
with 'UnsafeComponent' rather than interpolated. Second, each accepted component is
then __percent-encoded__ ('Ecluse.Core.Server.Route.encodeComponent') around the
structural @\'\@\'@ sigil and @%2F@ scope separator this builder writes — so a
@\'%\'@, @\'?\'@, @\'#\'@, or other reserved byte the denylist accepts (notably a
once-decoded @%2e%2e%2f@) cannot reach the upstream URL raw. A scoped
@\@scope\/name@ therefore yields exactly one @%2F@ (the separator written here, not
an encoding of a component), with no double-encoding. An empty @baseUrl@ is refused
with 'EmptyBaseUrl'. A single trailing slash on @baseUrl@ is tolerated so the join
never doubles it.
-}
upstreamUrlFor :: Text -> PackageName -> Either UrlError Text
upstreamUrlFor baseUrl name
    | T.null baseUrl = Left EmptyBaseUrl
    | otherwise = case firstUnsafe (componentsOf parts) of
        Just bad -> Left (UnsafeComponent bad)
        Nothing -> Right (joinUrlPath baseUrl (encodePath parts))
  where
    parts = nameParts name

    firstUnsafe :: [Text] -> Maybe Text
    firstUnsafe = find (not . isSafeComponent)

{- The structural decomposition of a package name for URL building: a scope with a
base name, or a single component, recovered by splitting the rendered name on the
@\'\/\'@ scope separator so a legitimate scoped name's own separator is not judged
as unsafe content. One source of truth for both the safety re-check ('componentsOf')
and the encoded path ('encodePath'), so the two cannot disagree about where the
component boundaries are. A leading @\'\@\'@ with no @\'\/\'@ (or an empty base) is
a single component (the @\@foo@ fallback).
-}
data NameParts
    = -- A scoped name: scope and base, each a component to check and encode.
      Scoped Text Text
    | -- An unscoped (or @\@@-leading, separator-free) name: one component.
      Single Text

nameParts :: PackageName -> NameParts
nameParts name =
    let rendered = renderPackageName name
     in case T.stripPrefix "@" rendered of
            Just scopeAndBase ->
                let (scope, base) = T.breakOn "/" scopeAndBase
                 in if T.null base
                        then Single rendered
                        else Scoped scope (T.drop 1 base)
            Nothing -> Single rendered

-- The components each of which must independently pass 'isSafeComponent'.
componentsOf :: NameParts -> [Text]
componentsOf = \case
    Scoped scope base -> [scope, base]
    Single c -> [c]

{- The encoded on-the-wire path for the components: the @\'\@\'@ sigil and the
@%2F@ scope separator are written here, around each percent-encoded component, so a
legitimate scoped name carries exactly one @%2F@ and no component byte can alter the
URL's shape. -}
encodePath :: NameParts -> Text
encodePath = \case
    Scoped scope base -> "@" <> encodeComponent scope <> "%2F" <> encodeComponent base
    Single c -> encodeComponent c
