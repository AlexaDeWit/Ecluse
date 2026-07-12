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
'Route'. The agnostic dispatcher carries a route classifier per mount; this module
is npm's, wired in at the composition root.

A @PUT \/{pkg}@ is the npm __publish__ request, so the method is part of the match:
a @PUT@ over a bare-package path is a 'Publish', while every read method (@GET@,
@HEAD@, …) over the same path is a 'Packument'. The read grammar below is otherwise
method-independent -- a @HEAD@ classifies like its @GET@, the dispatcher answering it
bodiless.

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
import Network.HTTP.Types.Method (StdMethod (GET, PUT), methodPut)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, unscopedName)
import Ecluse.Core.Server.Route (Classifier, Filename (Filename), Route (..), isSafeComponent)
import Ecluse.Core.Server.RouteSpec (ParamSpec (ParamSpec), PathSeg (Lit, Param), RouteSpec (RouteSpec))
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
classify method segments
    | method == methodPut = classifyPublish segments
    | otherwise = classifyRead segments

{- Classify a read request's path (any non-@PUT@ method): reserved meta-routes
first, then a package request. A @HEAD@ takes this same path as its @GET@ -- the
dispatcher answers it bodiless -- so the read grammar is method-independent. -}
classifyRead :: [Text] -> Route
classifyRead ("-" : meta) = classifyMeta meta
classifyRead segments = classifyPackage segments

{- Classify a @PUT@ as an npm publish. npm publishes a package with @PUT \/{pkg}@,
the version manifest and tarball carried in the body, so a publish is exactly a
__bare-package__ path (no trailing segments) -- both scoped encodings handled by
'takePackage'. A @PUT@ to anything else (a tarball slot, a meta-route, trailing
junk) is 'Unsupported' (deny by default); the version is /not/ read from the path
here -- it lives in the relayed document. -}
classifyPublish :: [Text] -> Route
classifyPublish segments =
    case takePackage segments of
        Just (name, []) -> Publish name
        _ -> Unsupported

{- Classify a reserved meta-route -- the segments __after__ the leading @"-"@.
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
bare package is a 'Packument', @\/-\/{file}.tgz@ a 'Tarball' when its basename
parses for the package, anything else 'Unsupported'.
-}
classifyPackage :: [Text] -> Route
classifyPackage segments =
    case takePackage segments of
        Nothing -> Unsupported
        Just (name, rest) -> dispatch name rest
  where
    dispatch name = \case
        [] -> Packument name
        ["-", file]
            | isSafeComponent file -> tarballRoute name file
        _ -> Unsupported

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

{- | npm's route grammar as data: one 'RouteSpec' per served 'Route', the
declarative projection of 'classify' the capability manifest renders.

Built by 'routeSpecFor' over one representative value per 'Route' constructor, so
the case is __total__: adding a 'Route' is a compile error here until it has a spec,
just as it is in 'classify'. Each spec's 'Ecluse.Core.Server.RouteSpec.rsExample' is
the request the correspondence test drives 'classify' with, asserting it yields the
spec's 'Ecluse.Core.Server.RouteSpec.rsRoute' -- so the description and the parser
cannot fall out of step.
-}
npmRouteSpecs :: NonEmpty RouteSpec
npmRouteSpecs = fmap routeSpecFor representativeRoutes

{- | One representative value per 'Route' constructor -- the iteration
'npmRouteSpecs' folds over. The payloads are inert ('routeSpecFor' reads the
constructor, not the payload); they reuse the example coordinates so the value read
is the one the spec documents.
-}
representativeRoutes :: NonEmpty Route
representativeRoutes =
    Packument examplePackage
        :| [ Tarball examplePackage exampleVersion exampleFilename
           , Publish examplePackage
           , Ping
           , Search
           , Unsupported
           ]

{- | Map an npm 'Route' to its declarative spec. __Total__ over the closed 'Route'
sum: the method, the path template, an example request, and the exact 'Route' the
example classifies to. The path grammar mirrors 'classify' (a packument @GET@ and a
publish @PUT@ share @\/{package}@; a tarball is @\/{package}\/-\/{filename}@; the
meta-routes are literal); the correspondence test is what forbids the two drifting.
-}
routeSpecFor :: Route -> RouteSpec
routeSpecFor = \case
    Packument{} ->
        RouteSpec GET [Param packageParam] ["lodash"] (Packument examplePackage)
    Tarball{} ->
        RouteSpec
            GET
            [Param packageParam, Lit "-", Param filenameParam]
            ["lodash", "-", "lodash-1.0.0.tgz"]
            (Tarball examplePackage exampleVersion exampleFilename)
    Publish{} ->
        RouteSpec PUT [Param packageParam] ["lodash"] (Publish examplePackage)
    Ping ->
        RouteSpec GET [Lit "-", Lit "ping"] ["-", "ping"] Ping
    Search ->
        RouteSpec GET [Lit "-", Lit "v1", Lit "search"] ["-", "v1", "search"] Search
    -- The deny-by-default catch-all documents @\/{unsupportedPath}@, but its example
    -- is any path the routes above do not claim (here an unknown meta-route), which
    -- is what actually classifies to 'Unsupported'.
    Unsupported ->
        RouteSpec GET [Param unsupportedParam] ["-", "whoami"] Unsupported

examplePackage :: PackageName
examplePackage = mkPackageName Npm Nothing "lodash"

exampleVersion :: Version
exampleVersion = mkVersion Npm "1.0.0"

exampleFilename :: Filename
exampleFilename = Filename "lodash-1.0.0.tgz"

packageParam :: ParamSpec
packageParam = ParamSpec "package" "The package name, URL-encoded; a scoped name is `@scope%2Fname`."

filenameParam :: ParamSpec
filenameParam = ParamSpec "filename" "The artifact's on-the-wire file name, e.g. `lodash-4.17.21.tgz`."

unsupportedParam :: ParamSpec
unsupportedParam = ParamSpec "unsupportedPath" "Any path under this mount matched by none of the routes above."
