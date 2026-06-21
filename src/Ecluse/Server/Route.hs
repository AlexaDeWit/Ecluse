-- TupleSections: local convenience for pairing a parsed name with its trailing
-- segments in 'takePackage' ((,rest) / (,more)); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | The pure request router for the npm front door.

'classify' turns an ecosystem-native request path — the already-mount-stripped,
WAI-percent-decoded @pathInfo@ segments — into a small 'Route' sum type, so the
whole routing table is unit-testable with __no server__: feed it segments,
assert the 'Route' (see "Ecluse.Server.RouteSpec").

The model is __deny by default__, mirroring the rules engine ("Ecluse.Rules"):
anything not explicitly recognised is 'Unsupported' (a @404@ at the edge). Three
npm-specific facts shape the matching, all from the protocol research (see
@docs\/research\/reverse-engineering\/npm.md@ §2 and §7):

* __Reserved meta-routes (@\/-\/…@) are matched first.__ A real package name can
  never begin with @\'-\'@, so a leading @"-"@ segment is unambiguously a
  meta-route; an /unknown/ one is 'Unsupported' rather than a package.

* __Scoped names arrive in two encodings.__ WAI percent-decodes @pathInfo@, so a
  scoped name reaches us either as one decoded segment (@\@scope\/pkg@) or as two
  (@\@scope@, @pkg@). Both are normalised to the same 'PackageName' here, so
  nothing downstream re-checks the encoding.

* __A tarball path is @\/{pkg}\/-\/{file}.tgz@.__ The interior @"-"@ segment and
  the @.tgz@ suffix distinguish it from a packument request (@\/{pkg}@); for a
  scoped package the basename drops the scope (@\@babel\/code-frame@ →
  @code-frame-7.0.0.tgz@), which 'classify' carries through verbatim as the file.

Mount dispatch / prefix-stripping and the liveness\/readiness routes are handled
elsewhere (see @docs\/architecture\/web-layer.md@); 'classify' only ever sees the
ecosystem-native path, so it models exactly the five 'Route's below.
-}
module Ecluse.Server.Route (
    -- * Routes
    Route (..),

    -- * Classification
    classify,
) where

import Data.Char (isControl)
import Data.Text qualified as T

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageName, mkPackageName, mkScope)

{- | A classified request. Everything the front door is willing to serve is one
of these; an unrecognised path is 'Unsupported' (deny by default).
-}
data Route
    = -- | A package-metadata request, @GET \/{pkg}@ (the /packument/).
      Packument PackageName
    | -- | An artifact request, @GET \/{pkg}\/-\/{file}.tgz@. The 'Text' is the
      -- tarball filename exactly as requested (scope already dropped from the
      -- basename for scoped packages).
      Tarball PackageName Text
    | -- | @GET \/-\/ping@ — a registry liveness probe, answered locally.
      Ping
    | -- | @GET \/-\/v1\/search@ — package search (unsupported).
      Search
    | -- | Anything unrecognised. Renders as a @404@ — deny by default at the
      -- routing layer.
      Unsupported
    deriving stock (Eq, Show)

{- | Classify an ecosystem-native request path into a 'Route'. __Pure and
total.__

Matching order is significant: reserved meta-routes (a leading @"-"@ segment)
are tried first, since a real package name can never begin with @\'-\'@; only
then is the path read as a package request. See the module header for the npm
conventions this encodes.
-}
classify :: [Text] -> Route
classify ("-" : meta) = classifyMeta meta
classify segments = classifyPackage segments

{- | Classify a reserved meta-route — the segments __after__ the leading @"-"@.
Only the routes the proxy actually serves are recognised; every other meta-route
is 'Unsupported' (never re-interpreted as a package).
-}
classifyMeta :: [Text] -> Route
classifyMeta = \case
    ["ping"] -> Ping
    ["v1", "search"] -> Search
    _ -> Unsupported

{- | Classify a non-meta path as a package request. Splits off the leading
package unit (handling both scoped encodings) and dispatches on what trails it: a
bare package is a 'Packument', @\/-\/{file}.tgz@ is a 'Tarball', anything else is
'Unsupported'.
-}
classifyPackage :: [Text] -> Route
classifyPackage segments =
    case takePackage segments of
        Nothing -> Unsupported
        Just (name, rest) -> dispatch name rest
  where
    dispatch name = \case
        [] -> Packument name
        ["-", file] | isTarballFile file && isSafeComponent file -> Tarball name file
        _ -> Unsupported

{- | Peel the leading package unit off a path, returning its 'PackageName' and
the remaining segments. Handles both wire encodings of a scoped name:

* one decoded segment, @\@scope\/pkg@ — split on the first @\'\/\'@;
* two segments, @\@scope@ then @pkg@ — consume both.

Returns 'Nothing' (so the caller denies it) for anything without a usable
package: an empty path, or a name with an __unsafe component__ — a scope or base
name that 'isSafeComponent' rejects (empty, @"."@\/@".."@, or carrying a
@\'\/\'@, @\'\\\\\'@, or control character). This covers the degenerate scoped
names (@\@\/pkg@, @\@scope\/@ reachable from @\/\@scope%2F@, @\@scope\/a\/b@) and
the hostile unscoped names (@\/foo%2Fbar@ → @"foo\/bar"@, @".."@, @"."@) alike.
'mkScope'\/'mkPackageName' do no validation, so this boundary is where such names
are rejected rather than passed downstream into an interpolated upstream URL.
-}
takePackage :: [Text] -> Maybe (PackageName, [Text])
takePackage [] = Nothing
takePackage (seg : rest)
    | "@" <- T.take 1 seg =
        case T.breakOn "/" (T.drop 1 seg) of
            -- One decoded segment "@scope/pkg": scope before the '/', base after.
            -- 'scopedName' may reject it, propagating 'Nothing' through (,rest).
            (scope, base)
                | not (T.null base) ->
                    (,rest) <$> scopedName scope (T.drop 1 base)
            -- Bare scope "@scope": the package name is the next segment.
            _ -> case rest of
                (base : more) -> (,more) <$> scopedName (T.drop 1 seg) base
                _ -> Nothing
    | isSafeComponent seg = Just (mkPackageName Npm Nothing seg, rest)
    | otherwise = Nothing
  where
    -- A scoped name is usable only when both halves are safe components. The
    -- leading '@' is already stripped from both arguments, so a degenerate or
    -- hostile name ('@/pkg', '@scope/', '@scope/a/b', '@../pkg') is rejected here
    -- rather than passed to the no-op 'mkScope'/'mkPackageName'.
    scopedName :: Text -> Text -> Maybe PackageName
    scopedName scope base
        | isSafeComponent scope && isSafeComponent base =
            Just (mkPackageName Npm (Just (mkScope scope)) base)
        | otherwise = Nothing

{- | Whether a single decoded path component is __safe to interpolate__ into a
downstream upstream URL — the one deny-by-default gate the router applies to
every component it accepts (scope, base name, and tarball filename).

WAI percent-decodes @pathInfo@, so a single segment can carry a @\'\/\'@, a
@\'\\\\\'@, a control character, or be @"."@\/@".."@; any of these enables path
traversal or request smuggling once the name reaches the upstream URL. A
component is UNSAFE iff it is empty, is exactly @"."@ or @".."@, or contains a
@\'\/\'@, a @\'\\\\\'@, or any 'isControl' character. Everything else is
accepted: this is a security boundary, __not__ an npm-policy validator, so
ordinary names with interior dots (@lodash.merge@, @is.odd@), hyphens,
underscores, digits, or uppercase all pass.
-}
isSafeComponent :: Text -> Bool
isSafeComponent c =
    not (T.null c)
        && c /= "."
        && c /= ".."
        && T.all safeChar c
  where
    safeChar ch = ch /= '/' && ch /= '\\' && not (isControl ch)

{- | Whether a tarball-slot filename is an npm tarball — a non-empty name ending
in @.tgz@. Guards the 'Tarball' route (alongside 'isSafeComponent') so a
non-artifact file under @\/-\/@ falls through to 'Unsupported'.
-}
isTarballFile :: Text -> Bool
isTarballFile file = T.isSuffixOf ".tgz" file && T.length file > T.length ".tgz"
