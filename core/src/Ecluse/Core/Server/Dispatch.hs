-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What serving a classified 'Route' amounts to: the one total interpretation of
the serve-action vocabulary.

"Ecluse.Core.Server.Route" names /what/ the proxy is willing to serve and an
ecosystem's 'Ecluse.Core.Server.Route.Classifier' decides /which/ action a request
names. This module is the third and last thing a 'Route' is matched on: it says how
each action is carried out. Every route falls into exactly one of two kinds
('RouteAction'):

* __Answered locally__ ('AnswerLocally'). A pure response the front door produces
  itself, with no upstream round-trip and no effects: @\/-\/ping@ is @200 {}@,
  @\/-\/v1\/search@ is a @501@ pointer (search is not an install path), and an
  unrecognised in-mount path is a @404@ (deny by default). Each is shaped by the
  matched mount's 'MountRenderer', so the body arrives in the ecosystem's own error
  surface.

* __Run through the data plane__ ('RunPipeline'). The package, artifact, and publish
  routes, whose fetch → gate → serve pipeline lives in "Ecluse.Core.Server.Pipeline".
  These run in the 'Handler' reader over the request context, and the web layer runs
  them under its request perimeter.

A @HEAD@ is a __bodiless variation__ of its @GET@, not a distinct action, so the
classifier does not distinguish it and the branch is taken here: 'routeAction' reads
the method and selects the head-mode handler for the two routes that have one. That
matters for more than tidiness on the artifact path, where running a @GET@ handler and
discarding the body would stream a whole artifact to nowhere (the reason @wai-extra@'s
@Autohead@ is deliberately not used; see "Ecluse.Runtime.Server").

__Why this lives in core, beside the 'Route' it interprets.__ The dispatch is
ecosystem-agnostic: a 'Route' is an action shared across registries, so npm, PyPI, and
RubyGems all reach the same handlers, and only the (method, path)→'Route' mapping is
per-ecosystem. Keeping the interpretation here means the web layer holds no route
knowledge at all: it asks for the action and either responds with it or runs it. The
whole serve pipeline and its 'Handler' monad are already core, so nothing about this
crosses the core/runtime boundary.
-}
module Ecluse.Core.Server.Dispatch (
    -- * The serve action
    RouteAction (..),
    routeAction,
) where

import Network.HTTP.Types (Method, methodHead, status200, status404, status501)
import Network.Wai (Request, Response, ResponseReceived)

import Ecluse.Core.Server.Context (Handler)
import Ecluse.Core.Server.Pipeline (headPackument, headTarball, servePackument, servePublish, serveTarball)
import Ecluse.Core.Server.Pipeline.Shared (jsonResponse, renderedResponse)
import Ecluse.Core.Server.Response (MountRenderer, renderError)
import Ecluse.Core.Server.Route (Route (..))

{- | How one classified 'Route' is served: locally, or through the data plane.

The split is the web layer's whole interest in a route. An 'AnswerLocally' route is a
pure function of the mount's renderer, so the dispatcher simply responds with it. A
'RunPipeline' route is an effectful handler awaiting the request and the respond
continuation, so the dispatcher discharges it to 'IO' under the request perimeter (the
guard that answers an escaped fault with a neutral, mount-shaped @500@).

Being a closed sum of exactly these two is what lets the front door route without
knowing any route's name.
-}
data RouteAction
    = -- | A pure response the proxy answers itself, shaped by the mount's renderer.
      AnswerLocally (MountRenderer -> Response)
    | {- | A data-plane handler, awaiting the request and the respond continuation. Run
      in 'Handler' over the per-request context, from which it reads its mount's serve
      dependencies (and answers the recognised-but-unwired stub itself when they are
      absent).
      -}
      RunPipeline (Request -> (Response -> IO ResponseReceived) -> Handler ResponseReceived)

{- | Interpret a classified 'Route' under the request's HTTP 'Method': the __one__
place a 'Route' is turned into the work of serving it.

Total over the sum, so a new serve action cannot be added without deciding here how it
is carried out. The method is read only to pick a route's bodiless @HEAD@ mode: a
@HEAD@ on the packument or artifact route runs the identical gating as its @GET@ and
emits the same status and headers with the body withheld, rather than building a body
that is then thrown away.
-}
routeAction :: Method -> Route -> RouteAction
routeAction method = \case
    Packument name
        | isHead -> RunPipeline (headPackument name)
        | otherwise -> RunPipeline (servePackument name)
    Tarball name version filename
        | isHead -> RunPipeline (headTarball name version filename)
        | otherwise -> RunPipeline (serveTarball name version filename)
    Publish name -> RunPipeline (servePublish name)
    Ping -> AnswerLocally (const pong)
    Search -> AnswerLocally searchUnsupported
    Unsupported -> AnswerLocally notFoundInMount
  where
    isHead :: Bool
    isHead = method == methodHead

{- @\/-\/ping@: answered locally with @200 {}@, since the client is only checking that
the proxy endpoint it talks to is up. No upstream round-trip, and no mount surface to
shape: an empty JSON object is what an npm client expects and what every other
ecosystem's probe can read.
-}
pong :: Response
pong = jsonResponse status200 [] "{}"

{- @\/-\/v1\/search@: a @501@ pointer in the mount's own error surface. Search is not
an install path, so the proxy does not proxy it; the message sends the client to the
public registry's website rather than leaving it to guess.
-}
searchUnsupported :: MountRenderer -> Response
searchUnsupported renderer =
    renderedResponse status501 [] (renderError renderer Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

{- An in-mount path no classifier recognised: a @404@ in the mount's error surface.
This is the routing layer's deny-by-default, mirroring the rules engine: the front
door serves nothing it was not explicitly taught to. Distinct from the neutral
@text\/plain@ @404@ above the mounts, where there is no ecosystem to render.
-}
notFoundInMount :: MountRenderer -> Response
notFoundInMount renderer =
    renderedResponse status404 [] (renderError renderer Nothing "not found")
