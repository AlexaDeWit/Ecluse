-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The first-party publish route, guarded before any upstream write.
Credential selection follows the authenticated edge mode. Body limits and name agreement
bound the request before the adapter relays it to the publication target.
-}
module Ecluse.Core.Server.Pipeline.Publish (
    PublishReplies (..),

    -- * The first-party publish handler
    servePublish,
) where

import Data.ByteString.Lazy qualified as LBS

import Network.HTTP.Types (ResponseHeaders, Status, mkStatus, status401, status403, status405, status413, status500, status502)
import Network.Wai (Request, RequestBodyLength (ChunkedBody, KnownLength), ResponseReceived, getRequestBodyChunk, requestBodyLength)

import Ecluse.Core.Credential (ClientCredential, bareCredential)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable), PublishRelayResponse (PublishRelayResponse))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishDeclaredNames, publishRelay))
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Security (BodyLimit (PublishRequestBodyLimit), Limits (maxPublishRequestBytes), boundedRead)
import Ecluse.Core.Server.Admission.Bytes (withByteAdmission)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPublishDeps),
    PublishDeps (..),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (appendHelp)

-- | Route-owned replies, including the publication target's unrestricted status codes.
data PublishReplies response = PublishReplies
    { publishRelayed :: Status -> ResponseHeaders -> LByteString -> response
    -- ^ Relay the publication target's status and bytes.
    , publishError :: Status -> ResponseHeaders -> Text -> response
    -- ^ Emit an ecosystem-shaped local error.
    }

-- | Relay a configured first-party publish after authentication and body guards.
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

publishWithDeps ::
    PublishReplies response ->
    PublishDeps ->
    Maybe ClientCredential ->
    PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
publishWithDeps replies deps clientToken name request respond
    | not (edgeTokenMatches (pubInboundToken deps) clientToken) =
        liftIO (respond (publishError replies status401 [] unauthorisedMessage))
    | not (pubAllowed deps name) =
        liftIO (respond (outOfScope replies deps name))
    | overDeclaredCap =
        liftIO (respond (publishTooLarge replies deps))
    | otherwise = do
        rt <- asks ctxRuntime
        outcome <-
            withByteAdmission (srMetrics rt) (pubBodyBudget deps) bodyWeight $
                liftIO (readAndRelay replies deps (publicationTarget deps rt clientToken) name request)
        liftIO (respond (fromMaybe (bodyBudgetShed replies deps) outcome))
  where
    overDeclaredCap = case requestBodyLength request of
        KnownLength n -> n > fromIntegral (maxPublishRequestBytes (pubLimits deps))
        ChunkedBody -> False

    bodyWeight = case requestBodyLength request of
        KnownLength n -> fromIntegral n
        ChunkedBody -> maxPublishRequestBytes (pubLimits deps)

{- Read the bounded body, check the name it declares, then relay, in that order. This runs
inside the byte-admission bracket, so the reserved weight always covers the bytes buffered. -}
readAndRelay ::
    PublishReplies response ->
    PublishDeps ->
    OriginClient ->
    PackageName ->
    Request ->
    IO response
readAndRelay replies deps target name request =
    boundedRead (PublishRequestBodyLimit (maxPublishRequestBytes (pubLimits deps))) (getRequestBodyChunk request) >>= \case
        Left _ -> pure (publishTooLarge replies deps)
        Right (_, body) -> case bodyNameDisagreement (publishDeclaredNames (pubAdapter deps)) (pubProjectName deps) name (LBS.fromStrict body) of
            Just declared -> pure (bodyNameMismatch replies deps name declared)
            Nothing -> renderRelay replies deps <$> publishRelay (pubAdapter deps) target name body

publicationTarget :: PublishDeps -> ServeRuntime -> Maybe ClientCredential -> OriginClient
publicationTarget deps rt clientToken =
    originClient
        (pubLimits deps)
        (srPrivateManager rt)
        (pubTargetUrl deps)
        (publicationCredential deps clientToken)

-- A configured static token replaces the caller's own, but only on an authenticated edge, so
-- an open edge never mints it.
publicationCredential :: PublishDeps -> Maybe ClientCredential -> Maybe ClientCredential
publicationCredential deps clientToken = case (pubInboundToken deps, pubStaticToken deps) of
    (Just _, Just staticToken) -> Just (bareCredential staticToken)
    _ -> clientToken

renderRelay ::
    PublishReplies response ->
    PublishDeps ->
    Either FetchFault PublishRelayResponse ->
    response
renderRelay replies deps = \case
    Right (PublishRelayResponse code relayed) ->
        publishRelayed replies (mkStatus code "") [] relayed
    -- An unformable target URL is this proxy's own misconfiguration, so it answers 500. The
    -- other two are the target's failure to answer, so they answer 502.
    Left (FetchUrlUnformable _urlErr) ->
        publishError replies status500 [] (appendHelp (pubHelp deps) "the publication target URL is misconfigured")
    Left (FetchTransport _fault) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")
    Left (FetchBoundExceeded _limit) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")

-- A @503@ for a publish shed at the aggregate body-byte budget: server capacity, not client
-- rate, so not a @429@. Same wait-then-shed timing and @Retry-After@ hint as the read path.
bodyBudgetShed :: PublishReplies response -> PublishDeps -> response
bodyBudgetShed replies deps =
    publishError replies shedStatus [shedRetryAfter] (appendHelp (pubHelp deps) "the server is at its publish-body capacity; retry shortly")

publishTooLarge :: PublishReplies response -> PublishDeps -> response
publishTooLarge replies deps =
    publishError replies status413 [] (appendHelp (pubHelp deps) "the publish body exceeds the maximum accepted request size")

-- A @405@ for a publish on a mount with no publication target configured. The @Allow@
-- header advertises the read methods the package route does serve.
publishDisabled :: PublishReplies response -> response
publishDisabled replies =
    publishError replies status405 [("Allow", "GET, HEAD")] message
  where
    -- The remediation stays ecosystem-neutral, because this pipeline serves every
    -- ecosystem: it names no client's own commands or configuration keys.
    message :: Text
    message =
        "publishing is not enabled on this proxy (no publication target is configured); \
        \publish directly to the registry you intend to publish to"

outOfScope :: PublishReplies response -> PublishDeps -> PackageName -> response
outOfScope replies deps name =
    publishError replies status403 [] (appendHelp (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': its name is outside the first-party namespaces this deployment declares (the anti-shadowing guard against publishing a name that shadows a public package)"

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
            <> "', which disagrees with the requested package name the first-party guard authorised (the anti-shadowing guard against publishing a name the guard never saw)"

-- A body-driven target must not write a name the URL-path guard never authorised.
bodyNameDisagreement :: (LByteString -> [Text]) -> (Text -> Maybe PackageName) -> PackageName -> LByteString -> Maybe Text
bodyNameDisagreement declaredNames canonicalise name body =
    find disagrees (declaredNames body)
  where
    disagrees :: Text -> Bool
    disagrees declared = case canonicalise declared of
        Just declaredName -> declaredName /= name
        Nothing -> True
