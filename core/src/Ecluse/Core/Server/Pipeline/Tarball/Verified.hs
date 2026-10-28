-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Complete bounded artifact bytes verify before any successful response commits.
module Ecluse.Core.Server.Pipeline.Tarball.Verified (
    VerifiedArtifactBody,
    VerifiedArtifactFault (..),
    VerifiedArtifactStage (..),
    VerifiedRelayResult (..),
    readVerifiedArtifactWith,
    relayVerifiedArtifact,
    relayVerifiedArtifactObserved,
    unconditionalArtifactRequest,
    verifiedHeadRequest,
    verifiedHeadResponse,
) where

import Crypto.Hash qualified as Crypto
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Internal qualified as BSI
import Data.Text qualified as T
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Network.HTTP.Client (Request, brRead, responseBody, responseHeaders, responseStatus)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, status200, status304, status401, status403, status500, status502, status503, statusCode)
import Network.Wai (ResponseReceived)
import UnliftIO.Exception (evaluate, throwIO, try)

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault (tfCause))
import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256))
import Ecluse.Core.Package.Hash (canonicalHashValue)
import Ecluse.Core.Package.Integrity (IntegrityResult (..), assertedAlg, authoritativeDigest, verifyIntegrity)
import Ecluse.Core.Registry (FetchFault (..))
import Ecluse.Core.Registry.Exchange (runExchangeUsingObserved, withinServeCap)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Public (ReadControl)
import Ecluse.Core.Security (BodyLimit (VerifiedArtifactBodyLimit), LimitError (BodyTooLarge), Limits (progressFloor))
import Ecluse.Core.Server.Admission.Artifact (verifiedArtifactMaxBytes, withVerifiedArtifactAdmission)
import Ecluse.Core.Server.Admission.Meter (charge, newChargeRegion)
import Ecluse.Core.Server.Conditional (Conditional (..), ETag, etagHeader, evaluateETag, mkStrongETag)
import Ecluse.Core.Server.Context (PackumentDeps (..), ServeRuntime (..))
import Ecluse.Core.Server.MemoryModel (verifiedArtifactCharge)
import Ecluse.Core.Server.Pipeline.Shared (retryAfterHeaders, shedMessage, shedRetryAfter, withListingAdmission)
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Pipeline.Tarball.Verified.Internal (VerifiedArtifactBody (..))
import Ecluse.Core.Server.Response (mkRefusal)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Core.Text (readDecimalText)

-- | Fetched but inadmissible bytes remain distinct from transport failures.
data VerifiedArtifactFault = VerifiedArtifactBound LimitError | VerifiedArtifactInvalid Text
    deriving stock (Eq, Show)

-- | Phase observations execute inside the same deadline as reading and verification.
data VerifiedArtifactStage = VerifyingArtifact | HashingArtifactValidator | ArtifactHashCompleted | ArtifactDigestVerified
    deriving stock (Eq, Show)

-- | Only a delivered verified GET may trigger mirror enqueueing.
data VerifiedRelayResult a = VerifiedDelivered a | VerifiedRefused Metric.Decision a | VerifiedUnanswered Status

newtype VerifiedReadFailure = VerifiedReadFailure VerifiedArtifactFault
    deriving stock (Show)

instance Exception VerifiedReadFailure

{- | Unknown lengths reserve their full cap. Known lengths require exact EOF.
One bounded destination avoids retaining an allocation per upstream chunk.
-}
readVerifiedArtifactWith :: (VerifiedArtifactStage -> IO ()) -> Int -> (Int -> IO ()) -> Artifact -> IO ByteString -> IO (Either VerifiedArtifactFault VerifiedArtifactBody)
readVerifiedArtifactWith observe cap pay artifact next = case checkedLimit cap artifact of
    Left fault -> pure (Left fault)
    Right limit -> case nonEmpty (artHashes artifact) of
        Nothing -> pure (Left (VerifiedArtifactInvalid "the selected artifact has no digest"))
        Just hashes
            | not (any ((== Just SHA256) . assertedAlg) hashes) -> pure (Left (VerifiedArtifactInvalid "the selected artifact has no advertised SHA-256"))
            | otherwise -> do
                pay (verifiedArtifactCharge limit)
                collected <- try (BSI.createAndTrim limit (\destination -> fill limit destination 0))
                case collected of
                    Left (VerifiedReadFailure fault) -> pure (Left fault)
                    Right bytes
                        | maybe False (/= BS.length bytes) (artSize artifact) -> pure (Left (VerifiedArtifactInvalid "the artifact length differs from its admitted size"))
                        | otherwise -> do
                            observe VerifyingArtifact
                            verdict <- evaluate (verifyIntegrity hashes bytes)
                            observe ArtifactHashCompleted
                            case verdict of
                                IntegrityMismatch detail -> pure (Left (VerifiedArtifactInvalid detail))
                                IntegrityVerified -> do
                                    tag <- case artifactValidator artifact of
                                        Just selected -> evaluate selected
                                        Nothing -> do
                                            observe HashingArtifactValidator
                                            completed <- evaluate (mkStrongETag (Crypto.hashWith Crypto.SHA256 bytes))
                                            observe ArtifactHashCompleted
                                            pure completed
                                    body <- evaluate (VerifiedArtifactBody bytes tag)
                                    observe ArtifactDigestVerified
                                    pure (Right body)
  where
    fill limit destination !seen = do
        chunk <- next
        pay (verifiedArtifactCharge (max limit (BS.length chunk)))
        if BS.null chunk
            then pure seen
            else
                if BS.length chunk > limit - seen
                    then throwIO (VerifiedReadFailure (VerifiedArtifactBound (BodyTooLarge (VerifiedArtifactBodyLimit limit))))
                    else do
                        BS.useAsCStringLen chunk (\(source, count) -> copyBytes (destination `plusPtr` seen) (castPtr source) count)
                        fill limit destination (seen + BS.length chunk)

checkedLimit :: Int -> Artifact -> Either VerifiedArtifactFault Int
checkedLimit cap artifact
    | cap < 0 || maybe False (< 0) (artSize artifact) = Left (VerifiedArtifactInvalid "negative verified artifact limit or size")
    | Just size <- artSize artifact, size > cap = Left (VerifiedArtifactBound (BodyTooLarge (VerifiedArtifactBodyLimit cap)))
    | otherwise = Right (fromMaybe cap (artSize artifact))

-- | Byte and memory reservations outlive verification and client delivery. Upstream closes before delivery.
relayVerifiedArtifact :: ArtifactRequest response -> ReadControl -> ExchangeClient -> Request -> Artifact -> IO (VerifiedRelayResult ResponseReceived)
relayVerifiedArtifact = relayVerifiedArtifactObserved (\_ _ -> pass)

-- | Public relay diagnostics retain actual origin status even when the reply becomes a local refusal.
relayVerifiedArtifactObserved :: (Status -> ResponseHeaders -> IO ()) -> ArtifactRequest response -> ReadControl -> ExchangeClient -> Request -> Artifact -> IO (VerifiedRelayResult ResponseReceived)
relayVerifiedArtifactObserved observe ctx control client request artifact = case pdVerifiedArtifacts deps of
    Nothing -> refuse Metric.Unavailable status500 [] "verified artifact admission is unavailable"
    Just budget -> case checkedLimit (verifiedArtifactMaxBytes budget) artifact of
        Left fault -> invalid fault
        Right weight -> do
            outcome <-
                withVerifiedArtifactAdmission metrics budget weight $
                    withListingAdmission
                        runtime
                        (\headers -> refuse Metric.Unavailable status503 headers shedMessage)
                        ( \ticket -> do
                            pay <- newChargeRegion (charge ticket)
                            withinServeCap (ecDeadlines client) (progressFloor (pdLimits deps)) id $
                                runExchangeUsingObserved observe control client (progressFloor (pdLimits deps)) request (fetch (verifiedArtifactMaxBytes budget) pay)
                        )
                        deliver
            case outcome of
                Nothing -> refuse Metric.Unavailable status503 [shedRetryAfter] shedMessage
                Just received -> pure received
  where
    deps = arDeps ctx
    runtime = arRuntime ctx
    metrics = srMetrics runtime
    replies = arReplies ctx
    respond = arRespond ctx
    refuse decision status headers message = VerifiedRefused decision <$> respond (tarballError replies status headers (mkRefusal (pdHelp deps) message))
    invalid fault = refuse Metric.Unavailable status502 [] (show fault)
    fetch cap pay response
        | responseStatus response /= status200 = pure (Right (Left (Left (responseStatus response))))
        | otherwise = case responseArtifact cap artifact (responseHeaders response) of
            Left fault -> pure (Right (Left (Right fault)))
            Right selected -> do
                consumed <- newIORef 0
                let next = do
                        chunk <- brRead (responseBody response)
                        modifyIORef' consumed (+ BS.length chunk)
                        mpArtifactReadBytes metrics (BS.length chunk)
                        pure chunk
                    stage ArtifactDigestVerified = mpArtifactVerified metrics
                    stage ArtifactHashCompleted = readIORef consumed >>= mpArtifactHashBytes metrics
                    stage _ = pure ()
                body <- readVerifiedArtifactWith stage cap pay selected next
                pure (Right (either (Left . Right) (Right . (responseHeaders response,)) body))
    deliver = \case
        Left (FetchCooldown retry) -> refuse Metric.Unavailable status503 (retryAfterHeaders (Just retry)) "the artifact source is cooling down"
        Left (FetchTransport fault) | tfCause fault == TransportProtocol -> refuse Metric.Unavailable status502 [] "the artifact source returned invalid framing"
        Left (FetchBoundExceeded fault) -> invalid (VerifiedArtifactBound fault)
        Left _ -> refuse Metric.Unavailable status503 [] "the artifact source is unavailable"
        Right (Left (Right fault)) -> invalid fault
        Right (Left (Left status))
            | status == status401 || status == status403 -> VerifiedRefused Metric.Deny <$> respond (tarballPrivateRefused replies (statusCode status) [] (mkRefusal Nothing "the artifact source refused access"))
            | statusCode status >= 200 && statusCode status < 400 -> refuse Metric.Unavailable status502 [] "the artifact source did not provide a complete representation"
            | otherwise -> pure (VerifiedUnanswered status)
        Right (Right (headers, VerifiedArtifactBody bytes tag)) -> do
            let represented = etagHeader tag : ("Content-Length", show (BS.length bytes)) : filter (\(name, _) -> name `notElem` ["ETag", "Last-Modified", "Content-Length", "Transfer-Encoding", "Connection", "Content-Encoding", "Content-Range", "Accept-Ranges"]) headers
            case evaluateETag (arValidators ctx) tag of
                NotModified _ -> VerifiedRefused Metric.Admit <$> respond (tarballEmpty replies status304 (filter ((/= "Content-Length") . fst) represented))
                Modified _ -> VerifiedDelivered <$> respond (tarballStream replies status200 represented (\write flush -> write (byteString bytes) >> flush))

responseArtifact :: Int -> Artifact -> ResponseHeaders -> Either VerifiedArtifactFault Artifact
responseArtifact cap artifact headers = do
    unless (all (\(_, value) -> T.toLower (T.strip (decodeUtf8 value)) == "identity") (filter ((== "Content-Encoding") . fst) headers)) (bad "the artifact response is not identity-coded")
    when (any ((== "Content-Range") . fst) headers) (bad "the artifact response carries a partial representation")
    case [value | (name, value) <- headers, name == "Content-Length"] of
        [] -> pure artifact
        [raw] -> case readDecimalText (T.strip (decodeUtf8 raw)) :: Maybe Integer of
            Just size | size >= 0 && size <= toInteger cap -> do
                when (maybe False ((/= size) . toInteger) (artSize artifact)) (bad "the source length differs from admitted metadata")
                pure artifact{artSize = Just (fromInteger size)}
            _ -> bad "the artifact response length exceeds its bound or is malformed"
        _ -> bad "the artifact response repeats its length"
  where
    bad = Left . VerifiedArtifactInvalid

-- | A full verified body cannot be replaced by a conditional or partial origin response.
unconditionalArtifactRequest :: Request -> Request
unconditionalArtifactRequest request =
    request
        { HTTP.method = "GET"
        , HTTP.decompress = const False
        , HTTP.requestHeaders = ("Accept-Encoding", "identity") : filter (\(name, _) -> name `notElem` ["If-None-Match", "If-Modified-Since", "Range", "If-Range", "Accept-Encoding"]) (HTTP.requestHeaders request)
        }

artifactValidator :: Artifact -> Maybe ETag
artifactValidator artifact = do
    hashes <- nonEmpty (artHashes artifact)
    guard (assertedAlg (authoritativeDigest hashes) == Just SHA256)
    digest <- case filter ((== Just SHA256) . assertedAlg) (toList hashes) of
        [selected] -> canonicalHashValue selected
        _ -> Nothing
    bytes <- rightToMaybe (convertFromBase Base16 (encodeUtf8 digest :: ByteString) :: Either String ByteString)
    mkStrongETag <$> (Crypto.digestFromByteString bytes :: Maybe (Crypto.Digest Crypto.SHA256))

-- | Authorise and check availability upstream before evaluating a local HEAD validator.
verifiedHeadRequest :: Request -> Request
verifiedHeadRequest request = (unconditionalArtifactRequest request){HTTP.method = "HEAD"}

-- | HEAD advertises the admitted representation without claiming a body hash observation.
verifiedHeadResponse :: Int -> Artifact -> RequestHeaders -> Status -> ResponseHeaders -> (Status, ResponseHeaders)
verifiedHeadResponse cap artifact validators status headers
    | status /= status200 = if statusCode status >= 200 && statusCode status < 400 then (status502, []) else (status, headers)
    | Left _ <- responseArtifact cap artifact headers = (status502, [])
    | Just tag <- artifactValidator artifact =
        let represented = etagHeader tag : filter (\(name, _) -> name `notElem` ["ETag", "Last-Modified", "Transfer-Encoding", "Connection", "Content-Encoding", "Content-Range", "Accept-Ranges"]) headers
         in case evaluateETag validators tag of
                NotModified _ -> (status304, filter ((/= "Content-Length") . fst) represented)
                Modified _ -> (status200, represented)
    | otherwise = (status502, [])
