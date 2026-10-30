-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Discovery and per-gem Compact Index responses retain current caller authority.
module Ecluse.Core.Registry.RubyGems.Handlers.CompactIndex (serveDirectory, serveInfo) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (brRead, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)
import Network.Wai (Request, ResponseReceived)
import UnliftIO (evaluate)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (BodyOutcome (SuccessBody, UnreadStatus), FetchFault (FetchBoundExceeded, FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Exchange (formThen, runExchangeUsing)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue), readStepsWithEnd)
import Ecluse.Core.Registry.Metadata (MetadataError (..), metadataResponse)
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps)
import Ecluse.Core.Registry.Public (ReadControl (..))
import Ecluse.Core.Registry.Request (BaseRequest, sealRequest)
import Ecluse.Core.Registry.RubyGems.Directory
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Handlers.Budget (newNativeDocumentBudget, newNativeReadBudget, withNativeRequest)
import Ecluse.Core.Registry.RubyGems.Request (gemDirectoryRequestUnder)
import Ecluse.Core.Registry.RubyGems.Route.Response (RubyResponse, rubyReplies)
import Ecluse.Core.Registry.RubyGems.Serving (serialiseRubyGemsDocument)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (..))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, awaitingFlight, charge, chargePart, newChargePart, newChargeRegion, servingPart, stepDown)
import Ecluse.Core.Server.Admission.Types (FlightKey (FlightKey))
import Ecluse.Core.Server.Cache (resolveDirectory)
import Ecluse.Core.Server.Context
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), answerDigestedMetadataBytes, mapPackumentReplies, withObservedAdmittedDocument)
import Ecluse.Core.Server.Pipeline.Shared (forwardedCredential, withListingAdmission)
import Ecluse.Core.Server.Response (mkRefusal)
import Ecluse.Core.Telemetry.Metrics (CacheStore (FullStore))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (Listing))

-- | Share admission and a whole-request deadline across discovery and response assembly.
serveDirectory :: Request -> (RubyResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveDirectory request respond =
    withNativeRequest
        withListingAdmission
        request
        (liftIO . respond)
        (const (directoryFailure (MetadataHttpFailure 503)))
        ( \ticket deps -> do
            runtime <- asks ctxRuntime
            mount <- asks ctxMount
            privateRead <- liftIO (newChargePart ticket)
            publicRead <- liftIO (newChargePart ticket)
            let credential = forwardedCredential mount request
                limits = pdLimits deps
                publicKey = registryUrlText (pdPublicBaseUrl deps) <> "\0" <> show (maxMetadataBytes limits, maxVersionCount limits, maxArtifactCount limits)
                flight = FlightKey FullStore (toShort ("rubygems-directory:" <> encodeUtf8 publicKey))
                -- Once the body exists nothing reaches the released reads' views, so their charges go back before it is sent.
                answerBody released body = do
                    bytes <- evaluate body
                    liftIO (traverse_ (`stepDown` 0) released)
                    answerCompact deps ticket request bytes
            private <- case pdPrivateBase deps of
                Nothing -> pure (Right Nothing)
                Just base -> fmap Just <$> liftIO (fetchDirectory (chargePart privateRead) limits Uncontrolled (srPrivateClient runtime) base credential)
            case private of
                Left fault -> pure (directoryFailure fault)
                Right privateView -> do
                    public <-
                        if pdReadSourceSelection deps == PrivateOnly
                            then pure (Right (GemDirectory ""))
                            else
                                liftIO $
                                    awaitingFlight ticket flight $
                                        resolveDirectory
                                            (srMetadataCache runtime)
                                            publicKey
                                            (fetchDirectory (chargePart (servingPart flight publicRead)) limits (Controlled (srPublicControl runtime) Listing) (srPublicClient runtime) (pdPublicBase deps) Nothing)
                    case public of
                        Left fault -> case privateView of
                            Nothing -> pure (directoryFailure fault)
                            Just own -> answerBody [publicRead] ("---\n" <> directoryBytes own)
                        Right shared -> do
                            let privateBytes = maybe 0 (BS.length . directoryBytes) privateView
                            if BS.length (directoryBytes shared) + privateBytes + 4 > maxMetadataBytes limits
                                then pure (directoryFailure (MetadataBoundExceeded (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits)))))
                                else do
                                    liftIO (charge ticket (2 * (directoryWeight shared + privateBytes) + maybe 0 (directoryMergeWorkspace shared) privateView + 4096))
                                    let merged = maybe shared (mergeDirectories shared) privateView
                                    answerBody [privateRead, publicRead] ("---\n" <> directoryBytes merged)
        )
        (liftIO . respond)

-- | Per-gem admission preserves exact winning installation rows and archive checksums.
serveInfo :: PackageName -> Request -> (RubyResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveInfo name request respond =
    withNativeRequest
        withListingAdmission
        request
        (liftIO . respond)
        (const (directoryFailure (MetadataHttpFailure 503)))
        ( \ticket deps -> do
            observe <- liftIO (newNativeReadBudget (maxMetadataBytes (pdLimits deps)))
            check <- liftIO (newNativeDocumentBudget (maxMetadataBytes (pdLimits deps)) (Left (directoryFailure MetadataUndecodable)))
            selected <-
                withObservedAdmittedDocument
                    (mapPackumentReplies Left rubyReplies)
                    ticket
                    observe
                    check
                    Nothing
                    name
                    request
                    (Right Nothing)
                    ( \cached _ _ -> pure $ case snd rubyGemsCached cached of
                        Nothing -> Left (directoryFailure MetadataUndecodable)
                        Just document -> Right (Just (cached, document))
                    )
            case selected of
                Left refusal -> pure refusal
                Right Nothing -> answerCompact deps ticket request "---\n"
                Right (Just (cached, document)) -> do
                    let size = BS.length (gemHeader document) + sum [BS.length (grSource row) + 1 | row <- gemRows document]
                    liftIO (charge ticket (2 * size + 4096))
                    case serialiseRubyGemsDocument cached of
                        Left _ -> pure (directoryFailure MetadataUndecodable)
                        Right encoded -> answerCompact deps ticket request (LBS.toStrict encoded)
        )
        (liftIO . respond)

fetchDirectory :: (Int -> IO ()) -> Limits -> ReadControl -> ExchangeClient -> BaseRequest -> Maybe ClientCredential -> IO (Either MetadataError GemDirectory)
fetchDirectory pay limits control client base credential = do
    workspace <- newChargeRegion pay
    result <-
        metadataResponse
            <$> formThen
                FetchUrlUnformable
                ( \upstream -> runExchangeUsing control client (progressFloor limits) upstream $ \response ->
                    if statusCode (responseStatus response) == 200
                        then fmap (SuccessBody 200) <$> readStepsWithEnd id finishRecordSteps (MetadataBodyLimit (maxMetadataBytes limits)) (directorySteps workspace limits) (brRead (responseBody response))
                        else pure (Right (UnreadStatus (statusCode (responseStatus response))))
                )
                (sealRequest <$> gemDirectoryRequestUnder base credential)
    pure (result >>= first (const MetadataUndecodable) . streamValue)

answerCompact :: PackumentDeps -> MemoryTicket -> Request -> ByteString -> Handler RubyResponse
answerCompact deps ticket request bytes
    | BS.length bytes > maxMetadataBytes (pdLimits deps) = pure (directoryFailure (MetadataBoundExceeded (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes (pdLimits deps))))))
    | otherwise = answerDigestedMetadataBytes replies deps ticket request digest bytes
  where
    -- One SHA-256 serves the strong ETag and Repr-Digest, which Bundler checks against the decoded native file.
    digest = hash bytes :: Digest SHA256
    reprDigest = "sha-256=\"" <> (convertToBase Base64 digest :: ByteString) <> "\""
    replies =
        rubyReplies
            { packumentOk = \headers -> packumentOk rubyReplies (("Repr-Digest", reprDigest) : headers)
            , packumentNotModified = \headers -> packumentNotModified rubyReplies (("Repr-Digest", reprDigest) : headers)
            }

directoryFailure :: MetadataError -> RubyResponse
directoryFailure fault = case fault of
    MetadataAuthorisationFailure code -> packumentPrivateRefused rubyReplies code [] refusal
    MetadataUndecodable -> packumentBadGateway rubyReplies [] refusal
    MetadataBoundExceeded _ -> packumentBadGateway rubyReplies [] refusal
    MetadataFetch (FetchBoundExceeded _) -> packumentBadGateway rubyReplies [] refusal
    MetadataHttpFailure 400 -> packumentBadGateway rubyReplies [] (mkRefusal Nothing "the native directory rejected the read with HTTP 400. CodeArtifact directories support at most 5,000 gem names")
    MetadataAbsent -> packumentBadGateway rubyReplies [] refusal
    MetadataHttpFailure code | code >= 200 && code < 400 -> packumentBadGateway rubyReplies [] refusal
    _ -> packumentUnavailable rubyReplies [] refusal
  where
    refusal = mkRefusal Nothing "the RubyGems Compact Index response is unavailable"
