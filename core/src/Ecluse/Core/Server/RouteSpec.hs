-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The documentation view of a route: the v-erased projection of a
"Ecluse.Core.Server.RoutePattern".'RoutePattern' the OpenAPI manifest renders.

A 'RouteSpec' states one route the way the __documentation__ needs it: the HTTP
method, the mount-relative path template (literal segments and named parameters), and
the shared 'Route' it denotes. 'specOf' derives it from the /same/ 'RoutePattern' the
front door routes on, dropping the capture-value type @v@ and the runtime parser, so
the documented paths, methods, and parameters are a projection of what the classifier
matches rather than a hand-kept parallel copy. The manifest can therefore not lie about
the routed surface: both derive from one pattern.

This module is __plain data__ with no OpenAPI (or any web) dependency, so the shared
grammar lives in the agnostic core and the heavy manifest tooling stays confined to
the build-time generator that renders it.
-}
module Ecluse.Core.Server.RouteSpec (
    -- * The documentation view
    RouteSpec (..),
    PathSeg (..),
    ParamSpec (..),

    -- * Projection from a route pattern
    specOf,
) where

import Network.HTTP.Types.Method (StdMethod (GET, PUT))

import Ecluse.Core.Server.Route (Route)
import Ecluse.Core.Server.RoutePattern (
    Capture (capDescription, capName),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    RoutePattern (rpMethod, rpRoute, rpSegs),
 )

{- | One route, described as data for a renderer: the method, the path template, and
the 'Route' it denotes. Derived from a 'RoutePattern' by 'specOf'.
-}
data RouteSpec = RouteSpec
    { rsMethod :: StdMethod
    -- ^ The HTTP method this route is documented under.
    , rsPattern :: [PathSeg]
    {- ^ The mount-relative path template: literal segments and named parameters, in
    order (e.g. @{package} \/ - \/ {filename}@). The mount prefix is not included; the
    manifest renderer prepends it.
    -}
    , rsRoute :: Route
    {- ^ The serve action this route denotes. It keys the manifest's per-route
    documentation, so a total case over the 'Route' sum keeps the described surface
    exhaustive.
    -}
    }
    deriving stock (Eq, Show)

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

{- | Project a 'RoutePattern' to its documentation view: the method it is documented
under (a read pattern matches any non-write method but documents as @GET@; the write
pattern as @PUT@), its path template (a literal segment verbatim, a capture as a named
parameter carrying its documentation), and the 'Route' it denotes. The capture-value
type and the runtime parser are dropped -- the manifest documents structure, not semantics.
-}
specOf :: RoutePattern v -> RouteSpec
specOf rp =
    RouteSpec
        { rsMethod = documentedMethod (rpMethod rp)
        , rsPattern = map paramOf (rpSegs rp)
        , rsRoute = rpRoute rp
        }
  where
    documentedMethod MethodPut = PUT
    documentedMethod MethodRead = GET

    paramOf (SegLit t) = Lit t
    paramOf (SegCap c) = Param (ParamSpec (capName c) (capDescription c))
