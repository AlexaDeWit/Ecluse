-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ExistentialQuantification #-}

{- | A route is one record for a URL the proxy serves.
It holds the method, template, action, and documentation. A route table folds into a mount
router where the first match wins and all other requests receive a @404@. The manifest renders
'Ecluse.Core.Server.RouteDescription' projections of the records the router runs.
-}
module Ecluse.Core.Server.Route (
    -- * A route
    Route (..),
    RouteName (..),
    PatternSeg (..),
    Capture (..),
    MethodMatch (..),
    MediaNegotiation (..),

    -- * Routing a request
    routerOf,
    matchRoute,

    -- * Rendering a route
    renderRoute,

    -- * Building a route table
    answering,
    refusing,
    safeSegment,
    isHead,
) where

import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Method (Method, methodDelete, methodGet, methodHead, methodPost, methodPut)

import Ecluse.Core.Server.Accept (acceptsAny)
import Ecluse.Core.Server.Context (
    MountRouter,
    ResponseAction (AnswerLocally, AnswerRefusal),
    RouteAction (RouteAction),
 )
import Ecluse.Core.Server.Contract (RequestSpec, ResponseContract, bodilessContract)
import Ecluse.Core.Server.Path (isSafeComponent)
import Ecluse.Core.Server.Response (HelpMessage)

{- | One route: how it matches, what it does, and what it documents. The type parameter @v@ is the
ecosystem's capture value, the only part of a route not shared across ecosystems.
-}
data Route v = forall response. Route
    { routeName :: RouteName
    -- ^ Unique within its ecosystem. The manifest qualifies it for OpenAPI's global operation ID.
    , routeMethod :: MethodMatch
    -- ^ The method condition a request must satisfy to match.
    , routeAccepts :: MediaNegotiation response
    -- ^ Served media types and the response when a request admits none.
    , routeSegs :: [PatternSeg v]
    -- ^ The mount-relative path template: literal segments and named captures, in order.
    , routeBuild :: Method -> [v] -> Maybe (ResponseAction response)
    {- ^ Builds an action from captured values in template order. 'Nothing' falls through to the
    next route, and a @HEAD@ uses the @GET@ builder because its response has no body.
    -}
    , routeSummary :: Text
    -- ^ A one-line summary (the OpenAPI operation summary).
    , routeDescription :: Text
    -- ^ The fuller prose description of what the route does.
    , routeRequest :: Maybe RequestSpec
    -- ^ The request body a write route accepts. 'Nothing' for a read.
    , routeContract :: ResponseContract response
    -- ^ Runtime dispatch and the manifest share this response contract.
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

{- | A named path capture and its parser. 'capConsume' may claim more than one segment, which an
ecosystem whose identifier spans a decoded @\'\/\'@ needs, and 'Nothing' fails the match.
-}
data Capture v = Capture
    { capName :: Text
    -- ^ The capture name, as it appears in the template (@{package}@).
    , capDescription :: Text
    -- ^ A one-line, human-facing description for the documentation.
    , capConsume :: [Text] -> Maybe (v, [Text])
    -- ^ Consume the leading segments this capture claims, yielding its value and the tail.
    , capRender :: v -> [Text]
    -- ^ 'capConsume' inverted, so a served URL is built from the record that must claim it.
    }

{- | What a route serves, and what it refuses a client that will not take it. The refusal renders
into the route's own contract, so the @406@ is documented from the record that serves it.
-}
data MediaNegotiation response
    = -- | The route negotiates nothing: every request is admitted whatever it says it accepts.
      AcceptsAnything
    | {- | The route serves these media types alone, and refuses a request whose @Accept@ admits
      none of them under the mount's configured help message.
      -}
      AcceptsOnly (NonEmpty ByteString) (Maybe HelpMessage -> response)

{- | The method condition on a route: a closed vocabulary rather than a predicate, so the
manifest can name the documented method. A method outside it matches no route and denies.
-}
data MethodMatch
    = -- | The write method (@PUT@).
      MethodPut
    | -- | The submission method (@POST@).
      MethodPost
    | -- | The removal method (@DELETE@).
      MethodDelete
    | -- | The read methods (@GET@ and @HEAD@).
      MethodRead
    deriving stock (Eq, Show)

-- | Whether a request method satisfies a route's 'MethodMatch'.
methodMatches :: MethodMatch -> Method -> Bool
methodMatches MethodPut m = m == methodPut
methodMatches MethodPost m = m == methodPost
methodMatches MethodDelete m = m == methodDelete
methodMatches MethodRead m = m == methodGet || m == methodHead

{- | Fold an ecosystem's route table into its mount's router. The first route that claims the
request decides it, and deny-by-default is structural because there is no other way to answer.
-}
routerOf :: RouteAction -> [Route v] -> MountRouter
routerOf notFound routes method headers segments =
    maybe (fallbackAction method notFound) snd (matchRoute routes method headers segments)

-- The catch-all, under the requested method's own contract.
fallbackAction :: Method -> RouteAction -> RouteAction
fallbackAction method (RouteAction contract action) =
    RouteAction (contractForMethod method contract) action

{- | The route that claims a request, and the action it names: the first route whose method
condition holds, whose segments are consumed exactly, and whose builder accepts the captures.
-}
matchRoute :: [Route v] -> Method -> RequestHeaders -> [Text] -> Maybe (Route v, RouteAction)
matchRoute routes method headers segments =
    listToMaybe (mapMaybe claim routes)
  where
    claim route@Route{routeMethod = matchedMethod, routeAccepts = negotiation, routeSegs = patternSegs, routeBuild = build, routeContract = contract}
        | methodMatches matchedMethod method = do
            captures <- consumeSegs patternSegs segments
            action <- negotiated headers negotiation (build method captures)
            pure (route, RouteAction (contractForMethod method contract) action)
        | otherwise = Nothing

-- A @HEAD@ answers the contract of its @GET@ with the body dropped.
contractForMethod :: Method -> ResponseContract response -> ResponseContract response
contractForMethod method
    | isHead method = bodilessContract
    | otherwise = id

{- A route the request will not take answers its own refusal, decided before the builder's action
is ever run. A route that negotiates nothing keeps whatever its builder decided. -}
negotiated ::
    RequestHeaders ->
    MediaNegotiation response ->
    Maybe (ResponseAction response) ->
    Maybe (ResponseAction response)
negotiated headers negotiation built = case negotiation of
    AcceptsAnything -> built
    AcceptsOnly served refusal
        | acceptsAny headers served -> built
        | otherwise -> AnswerRefusal refusal <$ built

{- Requires exact consumption: a leftover request segment, or a template segment with nothing to
match, fails. A 'SegCap' threads the remainder to the rest of the template. -}
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

{- | 'answering' for a refusal: the route decides it, and the site holding the mount's
dependencies renders it under the configured help message.
-}
refusing :: (Maybe HelpMessage -> response) -> Method -> [v] -> Maybe (ResponseAction response)
refusing render _method _captures = Just (AnswerRefusal render)

{- | A 'capConsume' that claims one leading segment, and only when it is a safe path component.
A traversal, separator, or control character therefore fails the match before the value exists.
-}
safeSegment :: (Text -> v) -> [Text] -> Maybe (v, [Text])
safeSegment build = \case
    seg : rest | isSafeComponent seg -> Just (build seg, rest)
    _ -> Nothing

{- | The mount-relative path a route serves one set of captures under. A rewritten artifact URL
is built through this, so the URL served and the route that must claim it are one record.
-}
renderRoute :: Route v -> [v] -> Maybe [Text]
renderRoute Route{routeSegs = patternSegs} = fillSegs patternSegs

-- The inverse of 'consumeSegs': one capture value per 'SegCap', in template order.
fillSegs :: [PatternSeg v] -> [v] -> Maybe [Text]
fillSegs [] [] = Just []
fillSegs (SegLit lit : ps) vs = (lit :) <$> fillSegs ps vs
fillSegs (SegCap capture : ps) (v : vs) = (capRender capture v <>) <$> fillSegs ps vs
fillSegs _ _ = Nothing

-- | Whether a request is the bodiless read. A @HEAD@ is a variation of its @GET@, not a route.
isHead :: Method -> Bool
isHead = (== methodHead)
