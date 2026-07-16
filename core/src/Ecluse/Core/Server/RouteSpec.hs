-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The documented operation view of a route, as plain OpenAPI-free data.

'specsOf' erases an "Ecluse.Core.Server.Route".'Route' into the operations the
capability manifest needs. A write route contributes @PUT@. A read route contributes
both @GET@ and its derived bodiless @HEAD@ operation. The capture type, builder, and
typed response value disappear, but each operation's 'ResponseDoc's are projected from
the same 'Ecluse.Core.Server.Contract.ResponseContract' runtime dispatch uses.
-}
module Ecluse.Core.Server.RouteSpec (
    -- * The documented view
    RouteSpec (..),
    PathSeg (..),
    ParamSpec (..),

    -- * Projection from a route
    specsOf,
) where

import Network.HTTP.Types.Method (StdMethod (GET, HEAD, PUT))

import Ecluse.Core.Server.Contract (RequestSpec, ResponseDoc, bodilessContract, responseDocs)
import Ecluse.Core.Server.Route (
    Capture (capDescription, capName),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route, routeContract, routeDescription, routeMethod, routeName, routeRequest, routeSegs, routeSummary),
    RouteName (RouteName, unRouteName),
 )

{- | One served HTTP operation: its name, exact method, path template, prose, request,
and response documents.

Not 'Eq'\/ 'Show': response and request schemas may carry @autodocodec@ codecs, which
are functions.
-}
data RouteSpec = RouteSpec
    { rsName :: RouteName
    -- ^ The operation name within its ecosystem; @HEAD@ projections carry a @.head@ suffix.
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
'bodilessContract' interpretation of the same response value used by @GET@, so its
status set cannot drift and neither its manifest nor wire response can carry a body.
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

        headName routeName' = RouteName (unRouteName routeName' <> ".head")

        paramOf (SegLit text) = Lit text
        paramOf (SegCap capture) = Param (ParamSpec (capName capture) (capDescription capture))
