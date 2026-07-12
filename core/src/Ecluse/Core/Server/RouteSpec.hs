-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The declarative route grammar: a mount's serve surface as data.

A 'RouteSpec' states one route the way the __documentation__ needs it: the HTTP
method, the mount-relative path template (literal segments and named path
parameters), a concrete example request, and the shared 'Route' the route denotes.
It is the projection of the
'Ecluse.Core.Server.Route.Classifier' the server actually routes on: the classifier
is the authoritative /parser/ (a request to a 'Route'), and this is the same grammar
turned outward as a /description/ a renderer can walk.

The two live side by side on the mount's adapter
("Ecluse.Core.Registry.Adapter.Types.AdapterServe" carries both the classifier and
these specs), and are reconciled by an executable correspondence: each spec's
'rsExample', run through the classifier, must yield its 'rsRoute'. So the documented
surface (its paths, methods, and parameters) cannot silently drift from what the
server routes, and the OpenAPI manifest ("Ecluse.Manifest") is a pure renderer of
this data rather than a hand-kept parallel copy of the path grammar.

This module is __plain data__ with no OpenAPI (or any web) dependency, so the shared
grammar lives in the agnostic core and the heavy manifest tooling stays confined to
the build-time generator that renders it.
-}
module Ecluse.Core.Server.RouteSpec (
    -- * The grammar
    RouteSpec (..),
    PathSeg (..),
    ParamSpec (..),
) where

import Network.HTTP.Types.Method (StdMethod)

import Ecluse.Core.Server.Route (Route)

{- | One route, described as data: what a renderer needs to document it and what the
correspondence check needs to hold it against the live 'Ecluse.Core.Server.Route.Classifier'.
-}
data RouteSpec = RouteSpec
    { rsMethod :: StdMethod
    -- ^ The HTTP method this route answers.
    , rsPattern :: [PathSeg]
    {- ^ The mount-relative path template: literal segments and named parameters, in
    order (e.g. @{package} \/ - \/ {filename}@). The mount prefix is not included; the
    manifest renderer prepends it.
    -}
    , rsExample :: [Text]
    {- ^ A concrete, mount-relative, already-percent-decoded example request (the
    path segments a client would send, prefix stripped) that the live classifier
    maps to 'rsRoute'. The reconciling correspondence test drives the classifier with
    this. For the deny-by-default catch-all it is any path the other routes do not
    claim, so it need not match 'rsPattern' structurally.
    -}
    , rsRoute :: Route
    {- ^ The serve action this route denotes: the exact 'Route' the classifier yields
    for 'rsExample'. It also keys the manifest's per-route documentation, so a total
    case over the 'Route' sum keeps the described surface exhaustive.
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
