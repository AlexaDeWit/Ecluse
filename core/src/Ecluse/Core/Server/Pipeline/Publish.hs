-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve path behind the first-party publish route: @PUT \/{pkg}@.

This module handles the publish flow: it validates edge authentication, applies
anti-shadowing scope guards to ensure the package name is permitted for publication,
enforces body-name agreement between the URL path and the publish document, and
relays the request to the upstream publication target with the publisher's credential.
-}
module Ecluse.Core.Server.Pipeline.Publish (
    PublishReplies (..),

    -- * The first-party publish handler
    servePublish,
) where

import Data.Aeson (Value (String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS

import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Types (ResponseHeaders, Status, mkStatus, status403, status405, status500, status502)
import Network.Wai (Request, ResponseReceived, consumeRequestBodyStrict)

import Ecluse.Core.Package (
    PackageName,
    Scope,
    pkgNamespace,
    renderPackageName,
 )
import Ecluse.Core.Registry (PublishRelayFault (RelayBoundExceeded, RelayTransport, RelayUrlUnformable), PublishRelayResponse (PublishRelayResponse))
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPublishDeps),
    PublishDeps (..),
    ServeRuntime (srPrivateManager),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (appendHelp)

{- | The route-owned ways the publish pipeline may answer. The configured target may
return any status, so npm supplies these constructors from an explicit OpenAPI @default@
contract whose media type remains @application/json@.
-}
data PublishReplies response = PublishReplies
    { publishRelayed :: Status -> ResponseHeaders -> LByteString -> response
    -- ^ Relay the publication target's status and bytes.
    , publishError :: Status -> ResponseHeaders -> Text -> response
    -- ^ Emit an ecosystem-shaped local error.
    }

servePublish ::
    PublishReplies response ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePublish replies name request respond = do
    asks (bindingPublishDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (publishDisabled replies))
        Just deps -> publishWithDeps replies deps name request respond

-- Serve a publish once the mount's publication target is known: the edge gate, the
-- anti-shadowing scope guard, then the body-name agreement check (all before any write),
-- then the relay to the publication target with the publisher's forwarded credential.
publishWithDeps ::
    PublishReplies response ->
    PublishDeps ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
publishWithDeps replies deps name request respond
    | not (edgeTokenMatches (pubInboundToken deps) clientToken) =
        liftIO (respond (publishError replies (mkStatus 401 "Unauthorized") [] "authentication required"))
    | not (inPublishScope (pubScopes deps) name) =
        liftIO (respond (outOfScope replies deps name))
    | otherwise = do
        rt <- asks ctxRuntime
        -- The body is bounded by the client→proxy request-size cap (the size-limit
        -- middleware), read here only after the scope guard has admitted the name, so a
        -- refused publish never even buffers its (potentially large, base64-tarball)
        -- body.
        body <- liftIO (consumeRequestBodyStrict request)
        -- The body-name agreement leg of the anti-shadowing guard (issue #391): the scope
        -- guard authorised the URL-path name, but the publish document carries its own
        -- declared identity, so a crafted body could otherwise write a name the guard never
        -- saw. Refuse -- before the relay -- any present declared name that disagrees with the
        -- URL-path name, so the identity authorised is provably the identity written.
        case bodyNameDisagreement (pubCanonicaliseName deps) name body of
            Just declared -> liftIO (respond (bodyNameMismatch replies deps name declared))
            -- @consumeRequestBodyStrict@ reads the whole body but returns it lazy; the
            -- publish builder ('relayPublishDocument') puts it on the wire as a strict
            -- @RequestBodyBS@, so materialise it strict here. The body is already bounded by
            -- the client→proxy request-size cap.
            Nothing -> do
                -- The relay reports its failures as the typed
                -- 'PublishRelayFault' value, so the render below is a total
                -- match -- nothing caught, and residue is the perimeter's.
                outcome <- liftIO (pubRelayPublish deps (pubLimits deps) (srPrivateManager rt) (pubTargetUrl deps) (clientToken <|> pubStaticToken deps) name (LBS.toStrict body))
                liftIO (respond (renderRelay replies deps outcome))
  where
    -- The publisher's bearer, scanned out of the headers once: the edge gate
    -- compares it and the relay forwards it (falling back to the static token).
    clientToken = forwardedToken request

{- Whether a package name falls within the configured publish-scope allow-list -- the
anti-shadowing guard. A __scoped__ name is admitted iff its scope is one of the
configured scopes; an __unscoped__ name is never in any scope, so it is refused (the
MVP allow-list is scope-based, e.g. @\@acme@). The scope equality is exact, so
@\@acme-evil@ does not match an @\@acme@ allow-list entry. -}
inPublishScope :: [Scope] -> PackageName -> Bool
inPublishScope scopes name = case pkgNamespace name of
    Just scope -> scope `elem` scopes
    Nothing -> False

{- Render the relay outcome: the publication target's own status and body forwarded to
the client on success (so the publisher sees the registry's real answer -- a success
shape, a @409@, a @403@ the registry's own authorisation produced); a @502@ when the
target's answer never arrived whole (a transport fault, or a response past the bound);
a @500@ when its URL is unformable (misconfiguration). -}
renderRelay ::
    PublishReplies response ->
    PublishDeps ->
    Either PublishRelayFault PublishRelayResponse ->
    response
renderRelay replies deps = \case
    Right (PublishRelayResponse code relayed) ->
        publishRelayed replies (mkStatus code "") [] relayed
    Left (RelayUrlUnformable _urlErr) ->
        publishError replies status500 [] (appendHelp (pubHelp deps) "the publication target URL is misconfigured")
    Left (RelayTransport _fault) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")
    Left (RelayBoundExceeded _limit) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")

-- A @405@ for a publish on a mount with no publication target configured: the
-- opt-in path is off, so a @PUT \/{pkg}@ is not an allowed method here. The @Allow@
-- header advertises the read methods the package route does serve.
publishDisabled :: PublishReplies response -> response
publishDisabled replies =
    publishError replies status405 [("Allow", "GET, HEAD")] "publishing is not enabled on this proxy (no publication target is configured)"

-- A @403@ for a publish whose name is outside the configured publish-scope
-- allow-list -- the anti-shadowing guard, refused before any upstream write.
outOfScope :: PublishReplies response -> PublishDeps -> PackageName -> response
outOfScope replies deps name =
    publishError replies status403 [] (appendHelp (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': its name is outside the configured publish-scope allow-list (the anti-shadowing guard against publishing a name that shadows a public package)"

-- A @403@ for a publish whose document body declares a package name -- its @_id@,
-- top-level @name@, or a @versions[].name@ -- that disagrees with the scope-guarded
-- URL-path name. The body-name agreement leg of the anti-shadowing guard (issue #391),
-- refused before any upstream write so the identity the guard authorises is the
-- identity written.
bodyNameMismatch :: PublishReplies response -> PublishDeps -> PackageName -> Text -> response
bodyNameMismatch replies deps name declared =
    publishError replies status403 [] (appendHelp (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': the document body declares the name '"
            <> declared
            <> "', which disagrees with the URL-path package name the scope guard authorised (the anti-shadowing guard against publishing a name the allow-list never saw)"

{- The first declared body name that disagrees with the URL-path name, or 'Nothing'
when the body declares no disagreeing name. The publish document carries its own
identity -- a top-level @_id@ and @name@, and a @name@ per entry in @versions@ -- so a
relay that keyed the write off the body could otherwise write a name the scope guard
never authorised. Each __present__ declared name is canonicalised the same way the
route builds its 'PackageName' ('projectName') and compared by 'PackageName' equality
(ecosystem-aware, so an encoding variant of the same name cannot disagree silently); a
present name that does not equal the URL-path name is a disagreement. Only the names
are read -- the base64 @_attachments@ are never decoded. An __absent__ name is not a
claim, so it is not a disagreement (a legitimate npm client always sends matching
names); a body that does not decode to a JSON object likewise declares no readable
name and raises none, leaving the relay to meet the target's own validation. -}
bodyNameDisagreement :: (Text -> Maybe PackageName) -> PackageName -> LByteString -> Maybe Text
bodyNameDisagreement canonicalise name body =
    case Aeson.decode body of
        Nothing -> Nothing
        Just document -> find disagrees (declaredNames document)
  where
    disagrees :: Text -> Bool
    disagrees declared = case canonicalise declared of
        Just declaredName -> declaredName /= name
        Nothing -> True

-- Every package-name string a publish document declares as its own identity: the
-- top-level @_id@ and @name@, and each @versions.<v>.name@. Only string-valued name
-- slots are read (a non-string slot is no name claim); the base64 @_attachments@ are
-- never touched.
declaredNames :: Value -> [Text]
declaredNames document =
    [ declared
    | slot <-
        [document ^? key "_id", document ^? key "name"]
            <> [ versionDoc ^? key "name"
               | versions <- maybeToList (document ^? key "versions" . _Object)
               , versionDoc <- KeyMap.elems versions
               ]
    , Just (String declared) <- [slot]
    ]
