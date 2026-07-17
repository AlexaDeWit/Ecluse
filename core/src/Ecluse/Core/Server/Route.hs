-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ExistentialQuantification #-}

{- | A route: one record saying everything there is to say about one URL the proxy
serves.

A 'Route' carries its method condition, its path template (literal segments and named
captures that parse themselves), what to /do/ when it matches, and its documentation.
An ecosystem's routing table is then simply __a list of these values__
("Ecluse.Core.Registry.Npm.Route" is npm's), and 'routerOf' folds that list into the
mount's router: first match wins, no match is the deny-by-default @404@.

There is no route /sum/. A classified-route type would have to be matched again to decide
what to do about it, and again to document it, and each of those matches is somewhere the
three can fall out of step. Here the pattern, the action, and the documentation are the
same value, so they cannot disagree, and the manifest renders 'Ecluse.Core.Server.RouteSpec'
projections of the very records the router runs.

== What stays a named function

The engine owns the __structure__: literal matching, capture arity, ordering, exact
consumption. It does not infer an ecosystem's __semantics__. A 'Capture' carries its own
segment parser and a 'Route' its own builder, so the security-critical leaf logic (the
component-safety gate, an ecosystem's scoped-name decoding, a version parse, the
cross-capture path-confusion check) stays in named, reviewed, separately-tested functions
that the record references, rather than being regenerated from a generic template.
-}
module Ecluse.Core.Server.Route (
    -- * A route
    Route (..),
    RouteName (..),
    PatternSeg (..),
    Capture (..),
    MethodMatch (..),

    -- * Routing a request
    routerOf,
    matchRoute,
) where

import Network.HTTP.Types.Method (Method, methodGet, methodHead, methodPut)

import Ecluse.Core.Server.Context (MountRouter, ResponseAction, RouteAction (RouteAction))
import Ecluse.Core.Server.Contract (RequestSpec, ResponseContract, bodilessContract)

{- | One route, whole: how it matches, what it does, and what it means.

Generic over the ecosystem's capture-value type @v@, which is the only thing about a
route that is not shared (npm's captures yield a parsed package or an artifact name;
another registry's would yield its own).
-}
data Route v = forall response. Route
    { routeName :: RouteName
    {- ^ This route's name, unique within its ecosystem (@"packument"@). It is the handle
    a test asserts on when it checks /which/ route a request took, and the manifest
    qualifies it by ecosystem to form OpenAPI's @operationId@ (which must be unique across
    the whole document, so only the manifest, which sees every mount at once, can
    guarantee it).
    -}
    , routeMethod :: MethodMatch
    -- ^ The method condition a request must satisfy to match.
    , routeSegs :: [PatternSeg v]
    -- ^ The mount-relative path template: literal segments and named captures, in order.
    , routeBuild :: Method -> [v] -> Maybe (ResponseAction response)
    {- ^ What serving this route amounts to, given the request method and the captured
    values (one per 'SegCap', in template order).

    'Nothing' __denies__: the route does not claim this request after all, and matching
    falls through to the next route (and, failing all of them, to the @404@). That is
    where a __cross-capture__ check lives, e.g. an artifact file name that must parse for
    the package captured earlier: a name addressing some other package's artifact is
    refused rather than fabricated into a coordinate.

    The 'Method' is passed because a @HEAD@ is a __bodiless variation__ of its @GET@
    rather than a distinct route: it matches the same pattern, and the builder selects the
    head-mode handler.
    -}
    , routeSummary :: Text
    -- ^ A one-line summary (the OpenAPI operation summary).
    , routeDescription :: Text
    -- ^ The fuller prose description of what the route does.
    , routeRequest :: Maybe RequestSpec
    -- ^ The request body a write route accepts; 'Nothing' for a read.
    , routeContract :: ResponseContract response
    {- ^ The response contract whose indexed value the builder's action can produce.
    Runtime dispatch renders that value to WAI while the manifest renders the same
    contract's response documents, so the action and documentation cannot be paired with
    different response sets.
    -}
    }

{- | A route's name within its ecosystem (@"packument"@, @"tarball"@). Not qualified: the
route already lives in its ecosystem's table, and the manifest adds the namespace when it
needs a globally unique identifier.
-}
newtype RouteName = RouteName {unRouteName :: Text}
    deriving stock (Eq, Ord, Show)

{- | One segment of a path template: a fixed segment matched verbatim, or a named capture
that consumes one or more leading segments and yields a value.
-}
data PatternSeg v
    = SegLit Text
    | SegCap (Capture v)

{- | A named path capture: how it parses (the security-critical leaf) and how it
documents. 'capConsume' may consume __more than one__ segment (an ecosystem whose
identifier spans a decoded @\'\/\'@ needs this) and returns the unconsumed tail, so
captures thread left to right; 'Nothing' fails the match, and the request falls through to
the next route or to the deny-by-default catch-all.
-}
data Capture v = Capture
    { capName :: Text
    -- ^ The capture name, as it appears in the template (@{package}@).
    , capDescription :: Text
    -- ^ A one-line, human-facing description for the documentation.
    , capConsume :: [Text] -> Maybe (v, [Text])
    -- ^ Consume the leading segments this capture claims, yielding its value and the tail.
    }

{- | The method condition on a route: the __read__ methods (@GET@ and @HEAD@), or the one
client __write__ (@PUT@).

Any other method matches no route and therefore denies (deny by default): the front door
answers only the methods it was taught, so a @DELETE@ or @POST@ over a package path is a
@404@ rather than being read as a package request. This also keeps the documented method
honest: the manifest says @GET@ for a read, and only a @GET@ (or its bodiless @HEAD@) is
served.

Kept as a small closed vocabulary rather than a bare predicate so the manifest can still
name the documented method.
-}
data MethodMatch
    = -- | The write method (@PUT@).
      MethodPut
    | -- | The read methods (@GET@ and @HEAD@).
      MethodRead
    deriving stock (Eq, Show)

-- | Whether a request method satisfies a route's 'MethodMatch'.
methodMatches :: MethodMatch -> Method -> Bool
methodMatches MethodPut m = m == methodPut
methodMatches MethodRead m = m == methodGet || m == methodHead

{- | Fold an ecosystem's route table into its mount's router: the first route that claims
the request decides what is done with it, and a request no route claims is the
deny-by-default @404@ in the mount's own error surface.

Deny-by-default is __structural__ here: 'routerOf' has no other way to answer. There is no
catch-all branch to forget. The @404@ 'Answer' a mount supplies for a path no route
claims is its deny-by-default surface (npm's @{"error": "not found"}@).
-}
routerOf :: RouteAction -> [Route v] -> MountRouter
routerOf notFound routes method segments =
    maybe (fallbackFor method notFound) snd (matchRoute routes method segments)
  where
    fallbackFor requested (RouteAction contract action)
        | requested == methodHead = RouteAction (bodilessContract contract) action
        | otherwise = RouteAction contract action

{- | The route that claims a request, and the action it names: the first whose method
condition holds, whose segments are consumed __exactly__, and whose builder accepts the
captures. 'Nothing' when none does.

Exported beside 'routerOf' because it is what makes a routing table testable with no
server: feed it a method and segments and assert /which/ route won (by its 'routeName'), or
that none did. The action itself is a closure and is exercised through the serve path.
-}
matchRoute :: [Route v] -> Method -> [Text] -> Maybe (Route v, RouteAction)
matchRoute routes method segments =
    listToMaybe (mapMaybe claim routes)
  where
    claim route@Route{routeMethod = matchedMethod, routeSegs = patternSegs, routeBuild = build, routeContract = contract}
        | methodMatches matchedMethod method = do
            captures <- consumeSegs patternSegs segments
            action <- build method captures
            pure (route, RouteAction (contractFor method contract) action)
        | otherwise = Nothing

    contractFor requested
        | requested == methodHead = bodilessContract
        | otherwise = id

{- Run a route's segments against a request's segments, collecting one value per capture
in template order. Requires __exact__ consumption: a leftover request segment, or a
template segment with nothing to match, fails. A 'SegCap' may consume more than one segment
(its 'capConsume' decides) and threads the remainder to the rest of the template. -}
consumeSegs :: [PatternSeg v] -> [Text] -> Maybe [v]
consumeSegs [] [] = Just []
consumeSegs (SegLit l : ps) (s : ss)
    | l == s = consumeSegs ps ss
consumeSegs (SegCap c : ps) ss = do
    (v, rest) <- capConsume c ss
    (v :) <$> consumeSegs ps rest
consumeSegs _ _ = Nothing
