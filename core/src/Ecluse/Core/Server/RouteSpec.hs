-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The __documented__ view of a route: everything a capability manifest needs to
describe it, as plain data.

A 'RouteSpec' is the erased projection of a "Ecluse.Core.Server.Route".'Route' ('specOf'):
its name, its method, its path template, its prose, its request body, and the closed set of
'Ecluse.Core.Server.Contract.Outcome's it can emit. The capture-value type, the builder, and
the concrete answer are all dropped, so this type is monomorphic and every ecosystem's routes
describe themselves through it, whatever their own captures.

The manifest interprets these specs into an OpenAPI document, and that is the __only__
thing it does with them: it holds no per-route knowledge of its own, so it has nothing to
drift with. The specs are projections of the very records the router runs, and the outcomes
are the very ones the handler answers through, so the documented surface cannot lie about
the routed one.
-}
module Ecluse.Core.Server.RouteSpec (
    -- * The documented view
    RouteSpec (..),
    PathSeg (..),
    ParamSpec (..),

    -- * Projection from a route
    specOf,
) where

import Network.HTTP.Types.Method (StdMethod (GET, PUT))

import Ecluse.Core.Server.Contract (RequestSpec, SomeOutcome)
import Ecluse.Core.Server.Route (
    Capture (capDescription, capName),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (routeDescription, routeMethod, routeName, routeOutcomes, routeRequest, routeSegs, routeSummary),
    RouteName,
 )

{- | One route, described as data for a renderer: what it is called, the method it answers,
its path template, its prose, the body it accepts, and the responses it can emit. Derived
from a 'Route' by 'specOf'.

Not 'Eq'\/'Show': its outcomes and request carry @autodocodec@ codecs, which are functions,
so a 'RouteSpec' is compared and inspected through its projections, not by deriving.
-}
data RouteSpec = RouteSpec
    { rsName :: RouteName
    {- ^ The route's name within its ecosystem. The manifest qualifies it by ecosystem to
    form OpenAPI's @operationId@.
    -}
    , rsMethod :: StdMethod
    -- ^ The HTTP method this route is documented under.
    , rsPattern :: [PathSeg]
    {- ^ The mount-relative path template: literal segments and named parameters, in order
    (e.g. @{package} \/ - \/ {filename}@). The mount prefix is not included; the manifest
    renderer prepends it.
    -}
    , rsSummary :: Text
    -- ^ A one-line summary (the OpenAPI operation summary).
    , rsDescription :: Text
    -- ^ The fuller prose description of what the route does.
    , rsRequest :: Maybe RequestSpec
    -- ^ The request body this route accepts; 'Nothing' for a read.
    , rsOutcomes :: [SomeOutcome]
    -- ^ The closed set of responses this route can emit, each a status paired with a body shape.
    }

{- | One segment of a path template: a fixed segment matched verbatim, or a single
captured, named path parameter rendered as @{name}@.
-}
data PathSeg
    = Lit Text
    | Param ParamSpec
    deriving stock (Eq, Show)

-- | A path parameter: its template name and a human-facing description for the docs.
data ParamSpec = ParamSpec
    { psName :: Text
    -- ^ The parameter name, as it appears in the template (@{package}@).
    , psDescription :: Text
    -- ^ A one-line, human-facing description of the parameter.
    }
    deriving stock (Eq, Show)

{- | Project a 'Route' to its documented view: the method it is documented under (a read
route documents as @GET@, its bodiless @HEAD@ being a variation rather than a route of its
own; a write route as @PUT@), its path template (a literal segment verbatim, a capture as a
named parameter carrying its description), and its prose, request, and outcomes carried
across unchanged.

The capture-value type, the builder, and the concrete answer are dropped: the manifest
documents structure, not semantics.
-}
specOf :: Route v -> RouteSpec
specOf route =
    RouteSpec
        { rsName = routeName route
        , rsMethod = documentedMethod (routeMethod route)
        , rsPattern = map paramOf (routeSegs route)
        , rsSummary = routeSummary route
        , rsDescription = routeDescription route
        , rsRequest = routeRequest route
        , rsOutcomes = routeOutcomes route
        }
  where
    documentedMethod MethodPut = PUT
    documentedMethod MethodRead = GET

    paramOf (SegLit t) = Lit t
    paramOf (SegCap c) = Param (ParamSpec (capName c) (capDescription c))
