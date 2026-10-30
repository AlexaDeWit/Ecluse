-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | First-party publication checks caller authority and body identity before target discovery or relay.
module Ecluse.Core.Server.Pipeline.Publish (
    PublishReplies (..),

    -- * The first-party publish handler
    servePublish,
) where

import Data.List (lookup)
import Network.HTTP.Types (ResponseHeaders, Status, hContentType, mkStatus, status400, status401, status403, status413, status500, status501, status502)
import Network.Wai (Request, RequestBodyLength (ChunkedBody, KnownLength), ResponseReceived, getRequestBodyChunk, requestBodyLength, requestHeaders)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchCooldown, FetchTransport, FetchUrlUnformable),
    ParseError (parseErrorMessage),
    PublishBodyFault (PublishMalformed, PublishUnsupported),
    PublishRelayResponse (PublishRelayResponse),
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishCodec, publishCredential, publishRelay, publishRequestExtras, publishUploadClaims), PublishRequestBody (..))
import Ecluse.Core.Registry.Exchange (accumulatingCharge, withinServeCap)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseRequest, ocChargeFullRead, ocClient), originClient)
import Ecluse.Core.Registry.Publish (PublishCodec (pcPrepareTarget, pcTargetReadPermille))
import Ecluse.Core.Registry.Request (credentialRecover, mkBaseRequest)
import Ecluse.Core.Security (BodyLimit (PublishRequestBodyLimit), Limits (maxPublishRequestBytes, progressFloor), boundedRead)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Bytes (withByteAdmission)
import Ecluse.Core.Server.Admission.Meter (charge, newChargeRegion)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps, bindingPublishDeps),
    PackumentDeps (pdHelp, pdMountBaseUrl),
    PublishDeps (..),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Response (HelpMessage, appendHelp)

-- | Route-owned replies, including the publication target's unrestricted status codes.
data PublishReplies response = PublishReplies
    { publishAllowedMethods :: ByteString
    -- ^ Methods advertised when this route has no configured publication target.
    , publishDisabledReply :: ResponseHeaders -> Maybe HelpMessage -> response
    -- ^ Preserve the route's disabled-publication response body.
    , publishRelayed :: Status -> ResponseHeaders -> LByteString -> response
    -- ^ Relay the publication target's status and bytes.
    , publishError :: Status -> ResponseHeaders -> Text -> response
    -- ^ Emit an ecosystem-shaped local error.
    }

-- | Relay a configured first-party publish after authentication and body guards.
servePublish ::
    PublishReplies response ->
    Maybe PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePublish replies name request respond = do
    mount <- asks ctxMount
    case bindingPublishDeps mount of
        Nothing -> liftIO (respond (publishDisabledReply replies [("Allow", publishAllowedMethods replies)] (pdHelp (bindingPackumentDeps mount))))
        Just deps -> publishWithDeps replies deps (pdMountBaseUrl (bindingPackumentDeps mount)) (credentialRecover (publishCredential (pubAdapter deps)) (requestHeaders request)) name request respond

publishWithDeps ::
    PublishReplies response ->
    PublishDeps ->
    Text ->
    Maybe ClientCredential ->
    Maybe PackageName ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
publishWithDeps replies deps registry clientToken name request respond
    | not (edgeTokenMatches (pubInboundToken deps) clientToken) =
        liftIO (respond (publishError replies status401 [] unauthorisedMessage))
    | Just pathName <- name
    , not (pubAllowed deps pathName) =
        liftIO (respond (outOfScope replies deps pathName))
    | Left fault <- extras =
        liftIO (respond (bodyRefusal replies deps fault))
    | overDeclaredCap =
        liftIO (respond (publishTooLarge replies deps))
    | otherwise = do
        rt <- asks ctxRuntime
        withMetadataAdmission
            rt
            (\headers -> liftIO (respond (publishError replies shedStatus headers shedMessage)))
            ( \ticket -> do
                workspace <- liftIO (newChargeRegion (charge ticket))
                withByteAdmission (srMetrics rt) (pubBodyBudget deps) bodyWeight $
                    liftIO (readAndRelay replies deps registry workspace (publicationTarget deps rt clientToken) name request)
            )
            (liftIO . respond . fromMaybe (bodyBudgetShed replies deps))
  where
    extras = publishRequestExtras (pubAdapter deps) (requestHeaders request)

    overDeclaredCap = case requestBodyLength request of
        KnownLength n -> n > fromIntegral (pubMaxRequestBytes deps)
        ChunkedBody -> False

    bodyWeight = case requestBodyLength request of
        KnownLength n -> fromIntegral n
        ChunkedBody -> pubMaxRequestBytes deps

-- The body reservation stays live through validation, discovery and relay.
readAndRelay ::
    PublishReplies response ->
    PublishDeps ->
    Text ->
    (Int -> IO ()) ->
    OriginClient ->
    Maybe PackageName ->
    Request ->
    IO response
readAndRelay replies deps registry workspace target name request =
    boundedRead (PublishRequestBodyLimit (pubMaxRequestBytes deps)) (getRequestBodyChunk request) >>= \case
        Left _ -> pure (publishTooLarge replies deps)
        Right (_, bytes) -> case publishRequestExtras (pubAdapter deps) (requestHeaders request) of
            Left fault -> pure (bodyRefusal replies deps fault)
            Right extras -> do
                let body = PublishRequestBody bytes (lookup hContentType (requestHeaders request)) extras
                claims <- publishUploadClaims (pubAdapter deps) ((pubLimits deps){maxPublishRequestBytes = pubMaxRequestBytes deps}) workspace registry body
                case claims of
                    Left fault -> pure (bodyRefusal replies deps fault)
                    Right declared -> case resolveName declared of
                        Left refusal -> pure refusal
                        Right agreed -> do
                            outcome <- withinServeCap (ecDeadlines (ocClient target)) (progressFloor (pubLimits deps)) MetadataFetch $ do
                                fullRead <- accumulatingCharge (workspace . scaleCharge (pcTargetReadPermille (publishCodec (pubAdapter deps))))
                                prepared <- pcPrepareTarget (publishCodec (pubAdapter deps)) target{ocChargeFullRead = fullRead} (pubTargetUrl deps)
                                case prepared of
                                    Left fault -> pure (Left fault)
                                    Right approved -> first MetadataFetch <$> publishRelay (pubAdapter deps) target{ocBaseRequest = mkBaseRequest approved} agreed body
                            pure (either (targetRefusal replies deps) (renderRelay replies deps . Right) outcome)
  where
    resolveName claims = case name of
        Just pathName -> case bodyNameDisagreement (pubProjectName deps) pathName claims of
            Just declared -> Left (bodyNameMismatch replies deps pathName declared)
            Nothing -> Right pathName
        Nothing -> case claims of
            [] -> Left invalidName
            declared : remaining -> case pubProjectName deps declared of
                Nothing -> Left invalidName
                Just agreed
                    | isJust (bodyNameDisagreement (pubProjectName deps) agreed remaining) -> Left invalidName
                    | not (pubAllowed deps agreed) -> Left (outOfScope replies deps agreed)
                    | otherwise -> Right agreed
    invalidName = publishError replies status400 [] (appendHelp (pubHelp deps) "the publish body must declare one valid agreed package name")

publicationTarget :: PublishDeps -> ServeRuntime -> Maybe ClientCredential -> OriginClient
publicationTarget deps rt clientToken =
    originClient
        ((pubLimits deps){maxPublishRequestBytes = pubMaxRequestBytes deps})
        (srPrivateClient rt)
        (pubReadUrl deps)
        (publicationCredential deps clientToken)

-- Static publication credentials require an authenticated edge.
publicationCredential :: PublishDeps -> Maybe ClientCredential -> Maybe ClientCredential
publicationCredential deps clientToken = case (pubInboundToken deps, pubStaticCredential deps) of
    (Just _, Just staticToken) -> Just staticToken
    _ -> clientToken

targetRefusal :: PublishReplies response -> PublishDeps -> MetadataError -> response
targetRefusal replies deps = \case
    MetadataFetch fault -> renderRelay replies deps (Left fault)
    MetadataBoundExceeded fault -> renderRelay replies deps (Left (FetchBoundExceeded fault))
    MetadataAuthorisationFailure status -> publishError replies (mkStatus status "") [] "the publication registry refused access"
    MetadataHttpFailure status -> publishError replies (mkStatus status "") [] "the publication registry discovery failed"
    MetadataAbsent -> publishError replies status502 [] "the publication registry has no discovery document"
    MetadataUndecodable -> publishError replies status502 [] "the publication registry discovery is malformed or names an unapproved API root"
    MetadataNameMismatch _ -> publishError replies status502 [] "the publication registry discovery identity disagrees"

renderRelay ::
    PublishReplies response ->
    PublishDeps ->
    Either FetchFault PublishRelayResponse ->
    response
renderRelay replies deps = \case
    Right (PublishRelayResponse code relayed) ->
        publishRelayed replies (mkStatus code "") [] relayed
    -- An invalid target is local configuration. Transport failures belong to the target.
    Left (FetchUrlUnformable _urlErr) ->
        publishError replies status500 [] (appendHelp (pubHelp deps) "the publication target URL is misconfigured")
    Left (FetchTransport _fault) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")
    Left (FetchCooldown _retry) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")
    Left (FetchBoundExceeded _limit) ->
        publishError replies status502 [] (appendHelp (pubHelp deps) "the publication target could not be reached")

bodyRefusal :: PublishReplies response -> PublishDeps -> PublishBodyFault -> response
bodyRefusal replies deps = \case
    PublishMalformed fault -> publishError replies status400 [] (appendHelp (pubHelp deps) (parseErrorMessage fault))
    PublishUnsupported reason -> publishError replies status501 [] (appendHelp (pubHelp deps) reason)

-- Body capacity uses the read path's 503 status and retry hint.
bodyBudgetShed :: PublishReplies response -> PublishDeps -> response
bodyBudgetShed replies deps =
    publishError replies shedStatus [shedRetryAfter] (appendHelp (pubHelp deps) "the server is at its publish-body capacity; retry shortly")

publishTooLarge :: PublishReplies response -> PublishDeps -> response
publishTooLarge replies deps =
    publishError replies status413 [] (appendHelp (pubHelp deps) "the publish body exceeds the maximum accepted request size")

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
bodyNameDisagreement :: (Text -> Maybe PackageName) -> PackageName -> [Text] -> Maybe Text
bodyNameDisagreement canonicalise name = find disagrees
  where
    disagrees declared = canonicalise declared /= Just name
