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
import UnliftIO (evaluate, withRunInIO)

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
import Ecluse.Core.Registry.Request (BaseRequest, baseRequestUrl, sealRequest)
import Ecluse.Core.Registry.RubyGems.Directory
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Handlers.Budget (newNativeDocumentBudget, newNativeReadBudget, withNativeRequest)
import Ecluse.Core.Registry.RubyGems.Request (gemDirectoryRequestUnder)
import Ecluse.Core.Registry.RubyGems.Route.Response (RubyResponse, rubyReplies)
import Ecluse.Core.Registry.RubyGems.Serving (serialiseRubyGemsDocument)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (..))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Meter (ChargePart, MemoryTicket, awaitingFlight, charge, chargePart, newChargePart, newChargeRegion, servingPart, stepDown)
import Ecluse.Core.Server.Cache (resolveDirectory)
import Ecluse.Core.Server.Cache.Types (directoryKey)
import Ecluse.Core.Server.Context
import Ecluse.Core.Server.Metadata (recordedFetch)
import Ecluse.Core.Server.Pipeline.Diagnostics (logDirectoryInvalidEntries)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), answerDigestedMetadataBytes, mapPackumentReplies, withObservedAdmittedDocument)
import Ecluse.Core.Server.Pipeline.Origin (publicSource)
import Ecluse.Core.Server.Pipeline.Shared (flightOf, forwardedCredential, withListingAdmission)
import Ecluse.Core.Server.Response (mkRefusal)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (Listing))

-- | Share admission and a whole-request deadline across discovery and response assembly.
serveDirectory :: Request -> (RubyResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveDirectory request respond =
    withNativeRequest
        withListingAdmission
        request
        (liftIO . respond)
        (const (directoryFailure (MetadataHttpFailure 503)))
        (answerDirectory request)
        (liftIO . respond)

-- A private fault answers before any public read. A public fault keeps the private rows unless it breached a bound.
answerDirectory :: Request -> MemoryTicket -> PackumentDeps -> Handler RubyResponse
answerDirectory request ticket deps = do
    privateRead <- liftIO (newChargePart ticket)
    publicRead <- liftIO (newChargePart ticket)
    private <- readPrivateDirectory request deps privateRead
    case private of
        Left fault -> pure (directoryFailure fault)
        Right privateView -> do
            public <- readPublicDirectory ticket deps publicRead
            case (public, privateView) of
                (Left fault, Just own) | not (boundBreach fault) && not (BS.null (directoryBytes own)) -> answerBody [publicRead] ("---\n" <> directoryBytes own)
                (Left fault, _) -> pure (directoryFailure fault)
                (Right shared, _) -> do
                    let privateBytes = maybe 0 (BS.length . directoryBytes) privateView
                    if BS.length (directoryBytes shared) + privateBytes + 4 > maxMetadataBytes limits
                        then pure (directoryFailure (MetadataBoundExceeded (BodyTooLarge (MetadataBodyLimit (maxMetadataBytes limits)))))
                        else do
                            liftIO (charge ticket (2 * (directoryWeight shared + privateBytes) + maybe 0 (directoryMergeWorkspace shared) privateView + 4096))
                            let merged = maybe shared (mergeDirectories shared) privateView
                            answerBody [privateRead, publicRead] ("---\n" <> directoryBytes merged)
  where
    limits = pdLimits deps
    -- Once the body exists nothing reaches the released reads' views, so their charges go back before it is sent.
    answerBody released body = do
        bytes <- evaluate body
        liftIO (traverse_ (`stepDown` 0) released)
        answerCompact deps ticket request bytes

-- The caller's credential reads private discovery on every request, outside shared retention.
readPrivateDirectory :: Request -> PackumentDeps -> ChargePart -> Handler (Either MetadataError (Maybe GemDirectory))
readPrivateDirectory request deps privateRead = case pdPrivateBase deps of
    Nothing -> pure (Right Nothing)
    Just base -> do
        runtime <- asks ctxRuntime
        mount <- asks ctxMount
        let source = DirectorySource (srPrivateClient runtime) Uncontrolled base (forwardedCredential mount request)
        withRunInIO $ \runInIO -> fmap Just <$> fetchDirectory runInIO (chargePart privateRead) (pdLimits deps) source

-- Only the flight leader fetches, records the fetch and logs dropped rows. Followers and retained snapshots do neither.
readPublicDirectory :: MemoryTicket -> PackumentDeps -> ChargePart -> Handler (Either MetadataError GemDirectory)
readPublicDirectory ticket deps publicRead
    | pdReadSourceSelection deps == PrivateOnly = pure (Right (GemDirectory ""))
    | otherwise = do
        runtime <- asks ctxRuntime
        let source = DirectorySource (srPublicClient runtime) (Controlled (srPublicControl runtime) Listing) (pdPublicBase deps) Nothing
        withRunInIO $ \runInIO ->
            awaitingFlight ticket flight $
                resolveDirectory (srMetrics runtime) (srMetadataCache runtime) key $
                    recordedFetch (srMetrics runtime) Metric.Public $
                        fetchDirectory runInIO (chargePart (servingPart flight publicRead)) limits source
  where
    limits = pdLimits deps
    key = directoryKey (publicSource deps) (maxMetadataBytes limits)
    flight = flightOf key

-- A public directory past its byte bound or name ceiling refuses the request, as an exceeded npm input bound does.
boundBreach :: MetadataError -> Bool
boundBreach = \case
    MetadataBoundExceeded _ -> True
    MetadataFetch (FetchBoundExceeded _) -> True
    _ -> False

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

-- One directory source: its transport, read control, base and the caller's credential, if any.
data DirectorySource = DirectorySource ExchangeClient ReadControl BaseRequest (Maybe ClientCredential)

-- Read one directory and log the rows it dropped under the source's authority.
fetchDirectory :: (Handler () -> IO ()) -> (Int -> IO ()) -> Limits -> DirectorySource -> IO (Either MetadataError GemDirectory)
fetchDirectory runInIO pay limits (DirectorySource client control base credential) = do
    workspace <- newChargeRegion pay
    result <-
        metadataResponse
            <$> formThen
                FetchUrlUnformable
                ( \upstream -> runExchangeUsing control client (progressFloor limits) upstream $ \response ->
                    if statusCode (responseStatus response) == 200
                        then fmap (SuccessBody 200) <$> readStepsWithEnd id finishRecordSteps (MetadataBodyLimit (maxMetadataBytes limits)) (directorySteps workspace) (brRead (responseBody response))
                        else pure (Right (UnreadStatus (statusCode (responseStatus response))))
                )
                (sealRequest <$> gemDirectoryRequestUnder base credential)
    let parsed = result >>= first (const MetadataUndecodable) . streamValue
    for_ parsed $ \directory -> unless (null (readDropped directory)) (runInIO (logDirectoryInvalidEntries (registryUrlText (baseRequestUrl base)) (readDropped directory)))
    pure (readDirectory <$> parsed)

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
