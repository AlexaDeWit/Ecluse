-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The documented operation view of a route, as plain OpenAPI-free data.

'specsOf' erases an "Ecluse.Core.Server.Route".'Route' into the operations the OpenAPI spec
needs. A write route contributes @PUT@, a submission route @POST@, and a removal route
@DELETE@. A read route contributes both @GET@ and its derived bodiless @HEAD@ operation. The
capture type, builder, and typed response value disappear. Each operation's 'ResponseDoc's
still come from the same 'Ecluse.Core.Server.Contract.ResponseContract' runtime dispatch uses.
-}
module Ecluse.Core.Server.RouteSpec (
    -- * The documented view
    RouteSpec (..),
    PathSeg (..),
    ParamSpec (..),

    -- * Projection from a route
    specsOf,

    -- * The synthetic catch-all
    catchAllSpecs,
) where

import Network.HTTP.Types.Method (StdMethod (DELETE, GET, HEAD, POST, PUT))

import Ecluse.Core.Server.Contract (RequestSpec, ResponseContract, ResponseDoc, bodilessContract, responseDocs)
import Ecluse.Core.Server.Route (
    Capture (capDescription, capName),
    MethodMatch (MethodDelete, MethodPost, MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route, routeContract, routeDescription, routeMethod, routeName, routeRequest, routeSegs, routeSummary),
    RouteName (RouteName, unRouteName),
 )

{- | One served HTTP operation, as the manifest documents it.

No 'Eq' or 'Show': request and response schemas may carry @autodocodec@ codecs, which are
functions.
-}
data RouteSpec = RouteSpec
    { rsName :: RouteName
    -- ^ The operation name within its ecosystem. A @HEAD@ projection adds a @.head@ suffix.
    , rsMethod :: StdMethod
    -- ^ The exact HTTP method this operation serves.
    , rsPattern :: [PathSeg]
    -- ^ The mount-relative path template.
    , rsSummary :: Text
    -- ^ A one-line summary.
    , rsDescription :: Text
    -- ^ The fuller operation description.
    , rsRequest :: Maybe RequestSpec
    -- ^ The accepted request body, when any.
    , rsOutcomes :: [ResponseDoc]
    -- ^ The responses projected from the operation's runtime contract.
    }

-- | One literal or captured path segment.
data PathSeg
    = Lit Text
    | Param ParamSpec
    deriving stock (Eq, Show)

-- | A named path parameter and its human-facing description.
data ParamSpec = ParamSpec
    { psName :: Text
    -- ^ The name as it appears in the template.
    , psDescription :: Text
    -- ^ A one-line description.
    }
    deriving stock (Eq, Show)

{- | Project a route to every exact method it serves. The @HEAD@ contract is the
'bodilessContract' interpretation of the same response value @GET@ uses. Its status set
therefore cannot drift, and neither its manifest nor its wire response can carry a body.
-}
specsOf :: Route v -> [RouteSpec]
specsOf
    Route
        { routeName = name
        , routeMethod = matchedMethod
        , routeSegs = segments
        , routeSummary = summary
        , routeDescription = description
        , routeRequest = request
        , routeContract = contract
        } = map operationSpec operations
      where
        operations = case matchedMethod of
            MethodRead -> [(GET, name, contract), (HEAD, headName name, bodilessContract contract)]
            MethodPut -> [(PUT, name, contract)]
            MethodPost -> [(POST, name, contract)]
            MethodDelete -> [(DELETE, name, contract)]

        operationSpec (method, operationName, operationContract) =
            RouteSpec
                { rsName = operationName
                , rsMethod = method
                , rsPattern = map paramOf segments
                , rsSummary = summary
                , rsDescription = description
                , rsRequest = request
                , rsOutcomes = responseDocs operationContract
                }

        paramOf (SegLit text) = Lit text
        paramOf (SegCap capture) = Param (ParamSpec (capName capture) (capDescription capture))

-- The @HEAD@ projection's operation name, derived so it cannot collide with its @GET@.
headName :: RouteName -> RouteName
headName name = RouteName (unRouteName name <> ".head")

{- | The @GET@ and @HEAD@ pair documenting a mount's deny-by-default catch-all. It is the
absence of a match rather than a route, so no 'Route' carries it and each table appends it.
-}
catchAllSpecs :: ResponseContract r -> ParamSpec -> NonEmpty RouteSpec
catchAllSpecs contract param = catchAllGet :| [catchAllHead]
  where
    catchAllGet =
        RouteSpec
            (RouteName "unsupported")
            GET
            [Param param]
            "Deny by default (unsupported path)"
            "Any request under this mount matched by none of the routes above is denied with `404` -- \
            \deny by default at the routing layer."
            Nothing
            (responseDocs contract)

    catchAllHead =
        RouteSpec
            (headName (RouteName "unsupported"))
            HEAD
            [Param param]
            "Deny by default (unsupported path)"
            "Any HEAD request under this mount matched by none of the routes above is denied with `404` \
            \and no response body."
            Nothing
            (responseDocs (bodilessContract contract))
