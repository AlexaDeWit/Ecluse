-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve path behind the first-party publish route: @PUT \/{pkg}@.

The publish flow runs in order:

* validate edge authentication
* apply the anti-shadowing scope guard, so the package name is one this proxy may publish
* bound the request body at the per-request size cap
* check body-name agreement between the URL path and the publish document
* relay the request to the upstream publication target with the publisher's credential

A declared over-cap length fails closed up front, and a counted read bounds a chunked
body. Both answer @413@.
-}
module Ecluse.Core.Server.Pipeline.Publish (
    PublishReplies (..),

    -- * The first-party publish handler
    servePublish,
) where

import Data.ByteString.Lazy qualified as LBS

import Network.HTTP.Types (ResponseHeaders, Status, mkStatus, status403, status405, status413, status500, status502)
import Network.Wai (Request, RequestBodyLength (ChunkedBody, KnownLength), ResponseReceived, getRequestBodyChunk, requestBodyLength)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (
    PackageName,
    Scope,
    pkgNamespace,
    renderPackageName,
 )
import Ecluse.Core.Registry (PublishRelayFault (RelayBoundExceeded, RelayTransport, RelayUrlUnformable), PublishRelayResponse (PublishRelayResponse))
import Ecluse.Core.Security (Limits (maxBodyBytes), boundedRead)
import Ecluse.Core.Server.Admission.Bytes (withByteAdmission)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPublishDeps),
    PublishDeps (..),
    ServeRuntime (srMetrics, srPrivateManager),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (appendHelp)

{- | The route-owned ways the publish pipeline may answer. The configured target may
return any status, so npm supplies these constructors from an explicit OpenAPI @default@
contract whose media type stays @application/json@.
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
    mount <- asks ctxMount
    case bindingPublishDeps mount of
        Nothing -> liftIO (respond (publishDisabled replies))
        Just deps -> publishWithDeps replies deps (forwardedCredential mount request) name request respond

-- Serve a publish once the mount's publication target is known. The edge gate, the
-- anti-shadowing scope guard, and the body-name agreement check all run before any write.
-- The relay to the publication target then carries the publisher's forwarded credential.
-- The mount's ecosystem presentation recovered that credential, scanned out of the
-- headers once at the entry point.
publishWithDeps ::
    PublishReplies response ->
    PublishDeps ->
    Maybe Secret ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
publishWithDeps replies deps clientToken name request respond
    | not (edgeTokenMatches (pubInboundToken deps) clientToken) =
        liftIO (respond (publishError replies (mkStatus 401 "Unauthorized") [] "authentication required"))
    | not (inPublishScope (pubScopes deps) name) =
        liftIO (respond (outOfScope replies deps name))
    | overDeclaredCap =
        -- A declared Content-Length already over the cap fails closed before a byte is
        -- read (no reservation, no relay). A chunked body carries no length to judge up
        -- front, so the counted read below enforces its cap instead.
        liftIO (respond (publishTooLarge replies deps))
    | otherwise = do
        rt <- asks ctxRuntime
        -- The whole buffered-body residency (read, name check, relay) runs inside
        -- the aggregate byte-admission. That admission is acquired only after the
        -- edge gate and the scope guard admitted the request, so a refused publish
        -- reserves nothing. The weight is the declared Content-Length. A chunked
        -- body declares nothing and reserves the per-request cap pessimistically, so
        -- the reservation always covers the bounded read's ceiling. Exhaustion sheds
        -- with the read path's vocabulary: a brief in-process wait, then a 503 with
        -- the same Retry-After hint.
        outcome <- withByteAdmission (srMetrics rt) (pubBodyBudget deps) bodyWeight $ do
            -- Read the body chunk-by-chunk through 'boundedRead', bounded at the
            -- per-request cap and returning the breach as a __value__. A chunked body
            -- has no declared length, so this counted read is what caps it. The breach
            -- is a fail-closed 413, never a truncated body, never a throw across the
            -- perimeter. It runs only after the scope guard admitted the name, so a
            -- refused publish never even buffers its large base64-tarball body.
            liftIO (boundedRead requestBodyLimits (getRequestBodyChunk request)) >>= \case
                -- 'boundedRead' reports only 'BodyTooLarge', so any breach of the
                -- request cap is the 413.
                Left _ -> pure (publishTooLarge replies deps)
                -- The body-name agreement leg of the anti-shadowing guard. The scope
                -- guard authorised the URL-path name, but the publish document carries
                -- its own declared identity. A crafted body could otherwise write a name
                -- the guard never saw. Refuse, before the relay, any present declared
                -- name that disagrees with the URL-path name, so the identity authorised
                -- is provably the identity written.
                Right body -> case bodyNameDisagreement (pubDeclaredNames deps) (pubCanonicaliseName deps) name (LBS.fromStrict body) of
                    Just declared -> pure (bodyNameMismatch replies deps name declared)
                    -- The relay reports its failures as the typed 'PublishRelayFault'
                    -- value, so the render below is a total match. Nothing is caught
                    -- here, and residue is the perimeter's. 'boundedRead' returns the
                    -- body strict, and the publish builder puts it on the wire as a
                    -- strict 'RequestBodyBS'.
                    Nothing ->
                        renderRelay replies deps
                            <$> liftIO (pubRelayPublish deps (pubLimits deps) (srPrivateManager rt) (pubTargetUrl deps) (clientToken <|> pubStaticToken deps) name body)
        liftIO (respond (fromMaybe (bodyBudgetShed replies deps) outcome))
  where
    -- The per-request body cap as a 'boundedRead' bound. 'boundedRead' consults only
    -- 'maxBodyBytes', so the response budget's other 'Limits' fields do not matter here.
    -- This keeps the request cap named in one place ('pubMaxRequestBytes').
    requestBodyLimits = (pubLimits deps){maxBodyBytes = pubMaxRequestBytes deps}

    -- Whether the request declares a Content-Length already over the per-request cap.
    overDeclaredCap = case requestBodyLength request of
        KnownLength n -> n > fromIntegral (pubMaxRequestBytes deps)
        ChunkedBody -> False

    bodyWeight = case requestBodyLength request of
        KnownLength n -> fromIntegral n
        ChunkedBody -> pubMaxRequestBytes deps

{- Whether a package name falls within the configured publish-scope allow-list: the
anti-shadowing guard. A __scoped__ name is admitted if and only if its scope is one of the
configured scopes. An __unscoped__ name is never in any scope, so the guard refuses it
(the allow-list is scope-based, e.g. @\@acme@). The scope equality is exact, so
@\@acme-evil@ does not match an @\@acme@ allow-list entry. -}
inPublishScope :: [Scope] -> PackageName -> Bool
inPublishScope scopes name = case pkgNamespace name of
    Just scope -> scope `elem` scopes
    Nothing -> False

{- Render the relay outcome. On success the client gets the publication target's own
status and body. The publisher then sees the registry's real answer: a success shape, a
@409@, or a @403@ the registry's own authorisation produced. A @502@ says the target's
answer never arrived whole, from a transport fault or a response past the bound. A @500@
says its URL is unformable, a misconfiguration. -}
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

-- A @503@ for a publish shed at the aggregate body-byte budget: server capacity, not
-- client rate, so not a @429@. Same brief-wait-then-shed timing and @Retry-After@ hint
-- as the read path's admission.
bodyBudgetShed :: PublishReplies response -> PublishDeps -> response
bodyBudgetShed replies deps =
    publishError replies shedStatus [shedRetryAfter] (appendHelp (pubHelp deps) "the server is at its publish-body capacity; retry shortly")

-- A @413@ for a publish whose body exceeds the per-request size cap
-- ('pubMaxRequestBytes', the client→proxy request-body limit). The breach is a declared
-- Content-Length over the cap, or a chunked body whose counted read crossed it. It
-- renders through the route's own error contract, before any upstream write.
publishTooLarge :: PublishReplies response -> PublishDeps -> response
publishTooLarge replies deps =
    publishError replies status413 [] (appendHelp (pubHelp deps) "the publish body exceeds the maximum accepted request size")

-- A @405@ for a publish on a mount with no publication target configured. The opt-in
-- path is off, so a @PUT \/{pkg}@ is not an allowed method here. The @Allow@ header
-- advertises the read methods the package route does serve.
publishDisabled :: PublishReplies response -> response
publishDisabled replies =
    publishError replies status405 [("Allow", "GET, HEAD")] "publishing is not enabled on this proxy (no publication target is configured)"

-- A @403@ for a publish whose name is outside the configured publish-scope allow-list:
-- the anti-shadowing guard, refused before any upstream write.
outOfScope :: PublishReplies response -> PublishDeps -> PackageName -> response
outOfScope replies deps name =
    publishError replies status403 [] (appendHelp (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': its name is outside the configured publish-scope allow-list (the anti-shadowing guard against publishing a name that shadows a public package)"

-- A @403@ for a publish whose document body declares a package name that disagrees with
-- the scope-guarded URL-path name. The ecosystem's own injected extractor,
-- 'pubDeclaredNames', reads that name. This is the body-name agreement leg of the
-- anti-shadowing guard, refused before any upstream write, so the identity the guard
-- authorises is the identity written.
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

{- The first declared body name that disagrees with the URL-path name, or 'Nothing' when
the body declares no disagreeing name. The publish document carries its own identity. A
relay that keyed the write off the body could otherwise write a name the scope guard never
authorised.

The ecosystem's own 'pubDeclaredNames' extractor reads each present declared name from the
raw body. The publish-document schema is the adapter's knowledge, not this neutral
pipeline's. Each name is canonicalised the way the route builds its 'PackageName' and
compared by 'PackageName' equality. That comparison is ecosystem-aware, so an encoding
variant of the same name cannot disagree silently. A present name that does not equal the
URL-path name is a disagreement. An __absent__ name is not a claim, so it is not a
disagreement, and a legitimate client always sends matching names. A body the extractor
reads no name from raises none, which leaves the relay to meet the target's own
validation. -}
bodyNameDisagreement :: (LByteString -> [Text]) -> (Text -> Maybe PackageName) -> PackageName -> LByteString -> Maybe Text
bodyNameDisagreement declaredNames canonicalise name body =
    find disagrees (declaredNames body)
  where
    disagrees :: Text -> Bool
    disagrees declared = case canonicalise declared of
        Just declaredName -> declaredName /= name
        Nothing -> True
