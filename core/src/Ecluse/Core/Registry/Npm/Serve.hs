-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's serve surface: how an npm request is served, and the denial body it is
refused in.

Two halves, both npm's own and both bound onto an npm mount by the composition root
(through 'Ecluse.Core.Registry.Adapter.Types.AdapterServe'):

* __'npmRouter'__, the whole routing decision. It classifies a mount-relative request
  through npm's route table ("Ecluse.Core.Registry.Npm.Route") and says what serving it
  amounts to, as the agnostic 'RouteAction' the web layer understands
  ("Ecluse.Core.Server.Context"). The __table is npm's__, because npm's URL grammar is;
  the __actions are shared__, because the data-plane handlers are ecosystem-neutral and
  reach their registry's client and projection as injected capabilities rather than
  imports. An ecosystem with a route npm lacks simply names a different action, and one
  with npm's routes reuses the same handlers.

* __'npmRenderer'__, the client-facing error body. The agnostic serve layer
  ("Ecluse.Core.Server.Response") decides the HTTP /status/ of a refusal; the /body/
  shape is npm's. npm clients read the human-facing reason from a JSON error object
  (preferring @message@, then @error@); Écluse emits the @error@ key, matching npm's own
  denial bodies, so the npm @{"error": …}@ shape never leaks into the
  ecosystem-neutral web layer.
-}
module Ecluse.Core.Registry.Npm.Serve (
    npmRouter,
    npmRenderer,
    npmDenialBody,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Network.HTTP.Types (Method, methodHead, status200, status501)
import Network.Wai (Response)

import Ecluse.Core.Registry.Npm.Route (classify)
import Ecluse.Core.Server.Context (
    MountRouter,
    RouteAction (AnswerLocally, RunPipeline),
 )
import Ecluse.Core.Server.Pipeline (headPackument, headTarball, servePackument, servePublish, serveTarball)
import Ecluse.Core.Server.Pipeline.Shared (jsonResponse, notFoundInMount, renderedResponse)
import Ecluse.Core.Server.Response (
    HelpMessage,
    MountRenderer (MountRenderer),
    RenderedBody (RenderedBody),
    appendHelp,
    renderError,
 )
import Ecluse.Core.Server.Route (Route (Packument, Ping, Publish, Search, Tarball, Unsupported))

{- | npm's router: classify a mount-relative request through npm's route table, then say
what serving it amounts to.

Total over npm's routes, so a route added to the table cannot be left unserved: it is a
compile error here until an action is chosen for it.

The 'Method' is read twice, and for different reasons. 'classify' reads it because the
same path names different /actions/ by method (@GET \/{pkg}@ reads a packument,
@PUT \/{pkg}@ publishes one, and nothing else is served at all). 'npmAction' reads it to
choose a route's __bodiless @HEAD@ mode__: a @HEAD@ classifies exactly like its @GET@,
because it is a rendering variation rather than a distinct action, and the head-mode
handler runs the identical gating while withholding the body. That matters for more than
tidiness on the artifact path, where running the @GET@ handler and discarding its body
would open the upstream and stream a whole artifact to nowhere (the reason @wai-extra@'s
@Autohead@ is deliberately not used; see "Ecluse.Runtime.Server").
-}
npmRouter :: MountRouter
npmRouter method segments = npmAction method (classify method segments)

-- The action each npm route names. The package, artifact, and publish routes run the
-- shared data-plane pipeline; the meta-routes and the deny-by-default miss are answered
-- locally, in npm's own error surface.
npmAction :: Method -> Route -> RouteAction
npmAction method = \case
    Packument name
        | isHead -> RunPipeline (headPackument name)
        | otherwise -> RunPipeline (servePackument name)
    Tarball name version filename
        | isHead -> RunPipeline (headTarball name version filename)
        | otherwise -> RunPipeline (serveTarball name version filename)
    Publish name -> RunPipeline (servePublish name)
    Ping -> AnswerLocally (const npmPong)
    Search -> AnswerLocally npmSearchUnsupported
    Unsupported -> AnswerLocally notFoundInMount
  where
    isHead :: Bool
    isHead = method == methodHead

{- @\/-\/ping@: answered locally with @200 {}@, since @npm ping@ is only checking that
the endpoint it talks to is up. No upstream round-trip, and no error surface to shape:
the empty JSON object is exactly what an npm client expects.
-}
npmPong :: Response
npmPong = jsonResponse status200 [] "{}"

{- @\/-\/v1\/search@: a @501@ pointer, in npm's error surface. Search is a discovery
convenience, not an install path, so the proxy does not proxy it; the message sends the
client to the public registry's website rather than leaving it to guess.
-}
npmSearchUnsupported :: MountRenderer -> Response
npmSearchUnsupported renderer =
    renderedResponse status501 [] (renderError renderer Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

{- | The npm mount renderer: every error body is npm's @{"error": …}@ JSON object,
tagged @application\/json@.
-}
npmRenderer :: MountRenderer
npmRenderer =
    MountRenderer (\help message -> RenderedBody "application/json" (npmDenialBody help message))

{- | Render an npm denial body -- the @{"error": …}@ object whose @error@ string is
the message with the operator help message, if any, appended. A blank or absent
help message is omitted rather than appended as empty text.

>>> npmDenialBody Nothing "denied because reasons"
"{\"error\":\"denied because reasons\"}"
-}
npmDenialBody :: Maybe HelpMessage -> Text -> LByteString
npmDenialBody help message =
    Aeson.encode (object ["error" .= appendHelp help message])
