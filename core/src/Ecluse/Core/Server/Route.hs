-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ExistentialQuantification #-}

{- | A route: one record saying everything there is to say about one URL the proxy
serves.

A 'Route' carries its method condition, its path template, what to /do/ when it matches,
and its documentation. The template is literal segments and named captures that parse
themselves. An ecosystem's routing table is then __a list of these values__
("Ecluse.Core.Registry.Npm.Route" is npm's). 'routerOf' folds that list into the mount's
router: first match wins, and no match is the deny-by-default @404@.

There is no route /sum/. A classified-route type must be matched again to decide what to
do about it, and again to document it. Each of those matches is a place the three can
fall out of step. Here the pattern, the action, and the documentation are the same value,
so they cannot disagree. The manifest renders 'Ecluse.Core.Server.RouteSpec' projections
of the same records the router runs.

== What stays a named function

The engine owns the __structure__: literal matching, capture arity, ordering, exact
consumption. It does not infer an ecosystem's __semantics__. A 'Capture' carries its own
segment parser and a 'Route' its own builder. The security-critical leaf logic therefore
stays in named, reviewed, separately-tested functions that the record references, rather
than being regenerated from a generic template. That leaf logic is the component-safety
gate, an ecosystem's scoped-name decoding, a version parse, and the cross-capture
path-confusion check.
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

    -- * Building a route table
    answering,
    safeSegment,
    isHead,
) where

import Network.HTTP.Types.Method (Method, methodDelete, methodGet, methodHead, methodPut)

import Ecluse.Core.Server.Context (
    MountRouter,
    ResponseAction (AnswerLocally),
    RouteAction (RouteAction),
 )
import Ecluse.Core.Server.Contract (RequestSpec, ResponseContract, bodilessContract)
import Ecluse.Core.Server.Path (isSafeComponent)

{- | One route: how it matches, what it does, and what it documents.

The type parameter @v@ is the ecosystem's capture value, the only part of a route that is
not shared across ecosystems.
-}
data Route v = forall response. Route
    { routeName :: RouteName
    {- ^ This route's name, unique within its ecosystem (@"packument"@). The manifest qualifies
    it by ecosystem to form OpenAPI's @operationId@, which must be unique across the document.
    -}
    , routeMethod :: MethodMatch
    -- ^ The method condition a request must satisfy to match.
    , routeSegs :: [PatternSeg v]
    -- ^ The mount-relative path template: literal segments and named captures, in order.
    , routeBuild :: Method -> [v] -> Maybe (ResponseAction response)
    {- ^ What serving this route amounts to, given the request method and the captured values
    (one per 'SegCap', in template order).

    'Nothing' denies: matching falls through to the next route, and to the @404@ when every
    route declines. A cross-capture check lives here. An artifact file name, for example, must
    parse for the package captured earlier.

    The builder receives the 'Method' because a @HEAD@ is a bodiless variation of its @GET@,
    not a distinct route.
    -}
    , routeSummary :: Text
    -- ^ A one-line summary (the OpenAPI operation summary).
    , routeDescription :: Text
    -- ^ The fuller prose description of what the route does.
    , routeRequest :: Maybe RequestSpec
    -- ^ The request body a write route accepts. 'Nothing' for a read.
    , routeContract :: ResponseContract response
    {- ^ The response contract the builder's action produces a value for. Runtime dispatch and
    the manifest both read it, so the served responses and the documented ones cannot drift.
    -}
    }

{- | A route's name within its ecosystem (@"packument"@, @"tarball"@). The manifest adds the
ecosystem namespace when it needs a globally unique identifier.
-}
newtype RouteName = RouteName {unRouteName :: Text}
    deriving stock (Eq, Ord, Show)

{- | One segment of a path template: a fixed segment matched verbatim, or a named
capture. A capture consumes one or more leading segments and yields a value.
-}
data PatternSeg v
    = SegLit Text
    | SegCap (Capture v)

{- | A named path capture and its parser. 'capConsume' may consume more than one segment,
which an ecosystem whose identifier spans a decoded @\'\/\'@ needs, and it returns the
unconsumed tail so captures thread left to right. 'Nothing' fails the match, so the request
falls through to the next route or to the deny-by-default catch-all.
-}
data Capture v = Capture
    { capName :: Text
    -- ^ The capture name, as it appears in the template (@{package}@).
    , capDescription :: Text
    -- ^ A one-line, human-facing description for the documentation.
    , capConsume :: [Text] -> Maybe (v, [Text])
    -- ^ Consume the leading segments this capture claims, yielding its value and the tail.
    }

{- | The method condition on a route: a closed vocabulary rather than a predicate, so the
manifest can name the documented method. A method outside it matches no route and denies.
-}
data MethodMatch
    = -- | The write method (@PUT@).
      MethodPut
    | -- | The removal method (@DELETE@).
      MethodDelete
    | -- | The read methods (@GET@ and @HEAD@).
      MethodRead
    deriving stock (Eq, Show)

-- | Whether a request method satisfies a route's 'MethodMatch'.
methodMatches :: MethodMatch -> Method -> Bool
methodMatches MethodPut m = m == methodPut
methodMatches MethodDelete m = m == methodDelete
methodMatches MethodRead m = m == methodGet || m == methodHead

{- | Fold an ecosystem's route table into its mount's router. The first route that claims the
request decides what happens to it.

A request no route claims gets the mount's @404@ 'Answer' (npm's @{"error": "not found"}@), so
deny-by-default is structural. 'routerOf' has no other way to answer, and there is no catch-all
branch to forget.
-}
routerOf :: RouteAction -> [Route v] -> MountRouter
routerOf notFound routes method segments =
    maybe (fallbackFor method notFound) snd (matchRoute routes method segments)
  where
    fallbackFor requested (RouteAction contract action)
        | isHead requested = RouteAction (bodilessContract contract) action
        | otherwise = RouteAction contract action

{- | The route that claims a request, and the action it names: the first route whose method
condition holds, whose segments are consumed exactly, and whose builder accepts the captures.
'Nothing' when none does. Exported beside 'routerOf' so a route table is testable with no
server.
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
        | isHead requested = bodilessContract
        | otherwise = id

{- Requires exact consumption: a leftover request segment, or a template segment with nothing
to match, fails. A 'SegCap' may consume more than one segment and threads the remainder to
the rest of the template. -}
consumeSegs :: [PatternSeg v] -> [Text] -> Maybe [v]
consumeSegs [] [] = Just []
consumeSegs (SegLit l : ps) (s : ss)
    | l == s = consumeSegs ps ss
consumeSegs (SegCap c : ps) ss = do
    (v, rest) <- capConsume c ss
    (v :) <$> consumeSegs ps rest
consumeSegs _ _ = Nothing

{- | A 'routeBuild' that answers with one fixed value whatever the method and captures. The
literal routes an ecosystem answers itself, rather than through the data plane, are built with it.
-}
answering :: response -> Method -> [v] -> Maybe (ResponseAction response)
answering answer _method _captures = Just (AnswerLocally answer)

{- | A 'capConsume' that claims one leading segment, and only when it is a safe path component.
A traversal, separator, or control character therefore fails the match before the value exists.
-}
safeSegment :: (Text -> v) -> [Text] -> Maybe (v, [Text])
safeSegment build = \case
    seg : rest | isSafeComponent seg -> Just (build seg, rest)
    _ -> Nothing

-- | Whether a request is the bodiless read. A @HEAD@ is a variation of its @GET@, not a route.
isHead :: Method -> Bool
isHead = (== methodHead)
