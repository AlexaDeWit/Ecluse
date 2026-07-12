-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: local convenience for pairing a parsed name with its trailing
-- segments in 'takeScoped' ((,rest) / (,more)); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | The npm path grammar: the request router that maps an npm-native request
path to a shared "Ecluse.Core.Server.Route".

'classify' turns an npm request -- its HTTP method and the already-mount-stripped,
percent-decoded path segments -- into a 'Route', so the whole npm routing table is
unit-testable with __no server__: feed it a method and segments, assert the
'Route'. It is __derived__ from 'npmPatterns', an ordered list of declarative
'Ecluse.Core.Server.RoutePattern.RoutePattern's the generic engine matches, so the
grammar is a value rather than hand-written control flow (which lets the same
description later render its OpenAPI path template for the manifest). The agnostic
dispatcher carries a route classifier per mount; this module is npm's, wired in at
the composition root.

A @PUT \/{pkg}@ is the npm __publish__ request, so the method is part of the match:
a @PUT@ over a bare-package path is a 'Publish', while a __read__ (@GET@, or its
bodiless @HEAD@) over the same path is a 'Packument'. Those three are the only methods
the front door answers; any other (@POST@, @DELETE@, …) matches no route and denies, so
a @DELETE@ over a package path is a @404@ rather than being served a packument.

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
  @code-frame-7.0.0.tgz@). 'classify' is the npm-side parse of the artifact
  coordinate: it checks the @file@'s basename is exactly @{unscoped-name}-{rest}@
  for the requested package and reads @rest@ as the version (@mkVersion@, total),
  yielding @'Tarball' name version ('Filename' file)@ with the file __preserved
  verbatim__. A basename that does not match the package is a path-confusion
  attempt and denies (deny by default), never a fabricated coordinate.

Mount dispatch / prefix-stripping and the liveness\/readiness routes are handled
in the agnostic web layer (see @docs\/architecture\/web-layer.md@); 'classify'
only ever sees the npm-native request, so it models exactly the 'Route's the
proxy serves.

The same grammar is exposed outward as data in 'npmRouteSpecs': the declarative
'RouteSpec' projection the capability manifest renders. 'classify' is the
authoritative parser and 'npmRouteSpecs' its description; each spec's example is
held against 'classify' by a correspondence test, so the documented surface cannot
drift from what the server routes.
-}
module Ecluse.Core.Registry.Npm.Route (
    -- * Classification
    classify,

    -- * Declarative grammar
    npmRouteSpecs,
) where

import Data.Text qualified as T
import Network.HTTP.Types.Method (StdMethod (GET))

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, unscopedName)
import Ecluse.Core.Server.Route (Classifier, Filename (Filename), Route (..), isSafeComponent)
import Ecluse.Core.Server.RoutePattern (
    Capture (Capture),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    RoutePattern (RoutePattern),
    classifyWith,
 )
import Ecluse.Core.Server.RouteSpec (ParamSpec (ParamSpec), PathSeg (Param), RouteSpec (RouteSpec), specOf)
import Ecluse.Core.Version (Version, mkVersion)

{- | Classify an npm-native request (its method and path) into a shared 'Route'.

A @PUT@ is the publish method, so it is dispatched first: a @PUT@ over a
bare-package path is a 'Publish', everything else under @PUT@ denies. Every other
method reads, taking the path through the read grammar where matching order is
significant -- reserved meta-routes (a leading @"-"@ segment) are tried first, since
a real package name can never begin with @\'-\'@; only then is the path read as a
package request. See the module header for the npm conventions this encodes.
-}
classify :: Classifier
classify = classifyWith npmPatterns

{- | npm's route grammar as data: the ordered 'RoutePattern's the front door routes
on ('classify', via 'Ecluse.Core.Server.RoutePattern.classifyWith'). The pattern
__structure__ (the literal segments, the capture arity, the ordering, the
packument-vs-tarball split) lives here; the security-critical __leaf__ parsing stays
in the named functions the captures and builders reference ('takePackage' for a
package unit, 'tarballRoute' for the artifact coordinate).

Ordering follows npm's conventions (see the module header): the reserved meta-routes
(@\/-\/ping@, @\/-\/v1\/search@) are literal and tried first, and the package capture
refuses a bare leading @"-"@ (the meta prefix is never a package name, on any method),
so an unrecognised @\/-\/…@ path denies rather than being read as a package.
-}
npmPatterns :: [RoutePattern NpmCap]
npmPatterns =
    [ RoutePattern MethodRead [SegLit "-", SegLit "ping"] (buildConst Ping) Ping
    , RoutePattern MethodRead [SegLit "-", SegLit "v1", SegLit "search"] (buildConst Search) Search
    , RoutePattern MethodRead [SegCap capPackage, SegLit "-", SegCap capFilename] buildTarball tarballRepr
    , RoutePattern MethodRead [SegCap capPackage] buildPackument (Packument examplePackage)
    , RoutePattern MethodPut [SegCap capPackage] buildPublish (Publish examplePackage)
    ]
  where
    tarballRepr = Tarball examplePackage exampleVersion exampleFilename

{- | The captured values npm's routes produce: a parsed package unit, or a raw,
safety-checked artifact file name. Each pattern's builder consumes these positionally.
-}
data NpmCap
    = NpmPackage PackageName
    | NpmFilename Text

-- | A builder for a route with no captures (the literal meta-routes @ping@ and @search@).
buildConst :: Route -> [NpmCap] -> Maybe Route
buildConst route _ = Just route

-- | @GET \/{package}@: a bare package unit is a packument request.
buildPackument :: [NpmCap] -> Maybe Route
buildPackument = \case
    [NpmPackage name] -> Just (Packument name)
    _ -> Nothing

-- | @PUT \/{package}@: a bare package unit under the write method is a publish.
buildPublish :: [NpmCap] -> Maybe Route
buildPublish = \case
    [NpmPackage name] -> Just (Publish name)
    _ -> Nothing

{- | @GET \/{package}\/-\/{filename}@: an artifact request. 'tarballRoute' applies
the __cross-capture__ path-confusion check (the file's basename must parse for /this/
package) and reads the version, denying a mismatched name (a 'Just' 'Unsupported',
never a fabricated coordinate).
-}
buildTarball :: [NpmCap] -> Maybe Route
buildTarball = \case
    [NpmPackage name, NpmFilename file] -> Just (tarballRoute name file)
    _ -> Nothing

{- | The package capture: one npm package unit, both scoped wire encodings handled by
'takePackage' (which may consume one or two segments).

A bare leading @"-"@ is refused __on every method__: @\/-\/…@ is the reserved
meta-route prefix, and a lone @"-"@ is never a package name. Every other
component-safety rejection is 'takePackage''s.
-}
capPackage :: Capture NpmCap
capPackage =
    Capture
        "package"
        "The package name, URL-encoded; a scoped name is `@scope%2Fname`."
        ( \case
            "-" : _ -> Nothing
            segs -> fmap (first NpmPackage) (takePackage segs)
        )

{- | The artifact-file capture: one segment, accepted only when it is a safe component
('isSafeComponent'); the coordinate parse (the @.tgz@ basename and the version) is
'tarballRoute''s, applied in 'buildTarball'.
-}
capFilename :: Capture NpmCap
capFilename =
    Capture
        "filename"
        "The artifact's on-the-wire file name, e.g. `lodash-4.17.21.tgz`."
        ( \case
            seg : rest | isSafeComponent seg -> Just (NpmFilename seg, rest)
            _ -> Nothing
        )

{- Peel the leading package unit off a path, returning its 'PackageName' and
the remaining segments. A leading segment beginning with @\'\@\'@ is a scoped
name, peeled by 'takeScoped' (which handles both wire encodings).

Returns 'Nothing' (so the caller denies it) for anything without a usable
package: an empty path, or a name with an __unsafe component__ -- a scope or base
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
    | "@" <- T.take 1 seg = takeScoped seg rest
    | isSafeComponent seg = Just (mkPackageName Npm Nothing seg, rest)
    | otherwise = Nothing

{- Peel a scoped package unit -- the leading @\@…@ segment -- handling both wire
encodings of a scoped name:

\* one decoded segment, @\@scope\/pkg@ -- split on the first @\'\/\'@;
\* two segments, @\@scope@ then @pkg@ -- consume both.
-}
takeScoped :: Text -> [Text] -> Maybe (PackageName, [Text])
takeScoped seg rest =
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

-- A scoped name is usable only when both halves are safe components. The
-- leading '@' is already stripped from both arguments, so a degenerate or
-- hostile name ('@/pkg', '@scope/', '@scope/a/b', '@../pkg') is rejected here
-- rather than passed to the no-op 'mkScope'/'mkPackageName'.
scopedName :: Text -> Text -> Maybe PackageName
scopedName scope base
    | isSafeComponent scope && isSafeComponent base =
        Just (mkPackageName Npm (Just (mkScope scope)) base)
    | otherwise = Nothing

{- Parse an npm tarball-slot @file@ into a 'Tarball' coordinate for @name@, or
deny it. The npm convention is @{unscoped-name}-{version}.tgz@, so the file must:

\* end in @.tgz@ over a non-empty name (a bare @.tgz@ is not an artifact), and
\* have a basename of exactly @{unscoped-name}-{version}@ -- the unscoped name (the
  scope dropped, as npm names the file), a @\'-\'@, then a non-empty @version@.

A basename that does not begin with @{unscoped-name}-@ is addressing some other
package's artifact under this package's path -- a path-confusion attempt -- so it
denies rather than fabricating a coordinate. On a match the @version@ run is read
by the total 'mkVersion' (an unparseable version still yields a coordinate, so a
parser gap never drops a real artifact), and the @file@ is preserved verbatim in
the 'Filename'. The caller has already passed @file@ through 'isSafeComponent'.
-}
tarballRoute :: PackageName -> Text -> Route
tarballRoute name file =
    case T.stripSuffix ".tgz" file >>= T.stripPrefix (unscopedName name <> "-") of
        Just version
            | not (T.null version) -> Tarball name (mkVersion Npm version) (Filename file)
        _ -> Unsupported

{- | npm's route grammar as data for the __capability manifest__: one 'RouteSpec' per
route, the documentation projection ('specOf') of the same 'npmPatterns' the classifier
routes on, plus the synthetic deny-by-default catch-all. Deriving the specs from the
patterns is what keeps the documented paths, methods, and parameters from drifting from
what the server matches: there is one grammar, projected two ways (parsed by 'classify',
rendered by the manifest).
-}
npmRouteSpecs :: NonEmpty RouteSpec
npmRouteSpecs = unsupportedSpec :| map specOf npmPatterns

{- | The synthetic spec for the deny-by-default catch-all. 'Unsupported' is not a
matched pattern (it is the /absence/ of a match), so it has no 'RoutePattern'; the
manifest documents it explicitly as the boundary, so a reader learns the limit from the
manifest rather than from an error reply.
-}
unsupportedSpec :: RouteSpec
unsupportedSpec = RouteSpec GET [Param unsupportedParam] Unsupported

unsupportedParam :: ParamSpec
unsupportedParam = ParamSpec "unsupportedPath" "Any path under this mount matched by none of the routes above."

examplePackage :: PackageName
examplePackage = mkPackageName Npm Nothing "lodash"

exampleVersion :: Version
exampleVersion = mkVersion Npm "1.0.0"

exampleFilename :: Filename
exampleFilename = Filename "lodash-1.0.0.tgz"
