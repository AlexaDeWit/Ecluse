-- TupleSections: local convenience for pairing a parsed name with its trailing
-- segments in 'takePackage' ((,rest) / (,more)); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | The npm path grammar: the request router that maps an npm-native request
path to a shared "Ecluse.Server.Route".

'classify' turns an npm request path — the already-mount-stripped, percent-decoded
path segments — into a 'Route', so the whole npm routing table is unit-testable
with __no server__: feed it segments, assert the 'Route'. The agnostic dispatcher
carries a route classifier per mount; this module is npm's, wired in at the
composition root.

The model is __deny by default__: anything not explicitly recognised is
'Unsupported' (a @404@ at the edge). Three npm-specific facts shape the matching,
all from the protocol research (see @docs\/research\/reverse-engineering\/npm.md@
§2 and §7):

* __Reserved meta-routes (@\/-\/…@) are matched first.__ A real package name can
  never begin with @\'-\'@, so a leading @"-"@ segment is unambiguously a
  meta-route; an /unknown/ one is 'Unsupported' rather than a package.

* __Scoped names arrive in two encodings.__ The path is percent-decoded before it
  reaches us, so a scoped name arrives either as one decoded segment
  (@\@scope\/pkg@) or as two (@\@scope@, @pkg@). Both are normalised to the same
  'PackageName' here, so nothing downstream re-checks the encoding.

* __A tarball path is @\/{pkg}\/-\/{file}.tgz@.__ The interior @"-"@ segment and
  the @.tgz@ suffix distinguish it from a packument request (@\/{pkg}@); for a
  scoped package the basename drops the scope (@\@babel\/code-frame@ →
  @code-frame-7.0.0.tgz@), which 'classify' carries through verbatim as the file.

Mount dispatch / prefix-stripping and the liveness\/readiness routes are handled
in the agnostic web layer (see @docs\/architecture\/web-layer.md@); 'classify'
only ever sees the npm-native path, so it models exactly the five 'Route's the
proxy serves.
-}
module Ecluse.Registry.Npm.Route (
    -- * Classification
    classify,
) where

import Data.Text qualified as T

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Server.Route (Classifier, Route (..), isSafeComponent)

{- | Classify an npm-native request path into a shared 'Route'.

Matching order is significant: reserved meta-routes (a leading @"-"@ segment)
are tried first, since a real package name can never begin with @\'-\'@; only
then is the path read as a package request. See the module header for the npm
conventions this encodes.
-}
classify :: Classifier
classify ("-" : meta) = classifyMeta meta
classify segments = classifyPackage segments

{- Classify a reserved meta-route — the segments __after__ the leading @"-"@.
Only the routes the proxy actually serves are recognised; every other meta-route
is 'Unsupported' (never re-interpreted as a package).
-}
classifyMeta :: [Text] -> Route
classifyMeta = \case
    ["ping"] -> Ping
    ["v1", "search"] -> Search
    _ -> Unsupported

{- Classify a non-meta path as a package request. Splits off the leading
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

{- Peel the leading package unit off a path, returning its 'PackageName' and
the remaining segments. Handles both wire encodings of a scoped name:

\* one decoded segment, @\@scope\/pkg@ — split on the first @\'\/\'@;
\* two segments, @\@scope@ then @pkg@ — consume both.

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

{- Whether a tarball-slot filename is an npm tarball — a non-empty name ending
in @.tgz@. Guards the 'Tarball' route (alongside 'isSafeComponent') so a
non-artifact file under @\/-\/@ falls through to 'Unsupported'.
-}
isTarballFile :: Text -> Bool
isTarballFile file = T.isSuffixOf ".tgz" file && T.length file > T.length ".tgz"
