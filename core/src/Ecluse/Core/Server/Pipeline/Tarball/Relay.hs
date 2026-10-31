-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Request shaping and response classification for artifact relays.
Response judgements precede the client response commit.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Relay (
    -- * Serve mode
    ArtifactServe (..),

    -- * Shaping the upstream artifact request
    withMethod,
    withValidators,

    -- * Relaying the upstream response
    relayUpstreamResult,
    relayPublicResult,
    acceptArtifact,
    acceptPrivateArtifact,
    privateArtifactResponder,
    relayUnjudged,
    relayJudged,

    -- * Judging the public relay
    RelayVerdict (..),
    relayVerdict,
    observeRelayAnomaly,
) where

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, hContentType, methodHead, statusCode, statusIsSuccessful)

import Data.ByteString qualified as BS
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (isAuthorisationFailure)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Public (PublicControl, ReadControl (Controlled))
import Ecluse.Core.Security (ProgressFloor)
import Ecluse.Core.Server.Conditional (isNotModified)
import Ecluse.Core.Server.Pipeline.Internal (pipelineModule)
import Ecluse.Core.Server.Stream (RelayResponder (..), UpstreamBody (NoBody, StreamBody), UpstreamResult, withUpstreamResult, withUpstreamResultUsing)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpPublicRelayAnomaly))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind)
import Ecluse.Core.Version (Version, renderVersion)
import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)

-- | The artifact method and whether a public admit enqueues a mirror job.
data ArtifactServe
    = -- | A @GET@: stream the artifact body through, enqueuing a mirror job on a public admit.
      ServeFull
    | -- | Probe with @HEAD@ without streaming bytes or enqueuing a mirror job.
      ServeHead

-- | Tag an upstream artifact request with the serve mode's method. 'ServeFull' keeps the @GET@.
withMethod :: ArtifactServe -> HTTP.Request -> HTTP.Request
withMethod = \case
    ServeFull -> id
    ServeHead -> \req -> req{HTTP.method = methodHead}

-- | Forward conditional validators so upstream can answer @304@.
withValidators :: RequestHeaders -> HTTP.Request -> HTTP.Request
withValidators validators req =
    req{HTTP.requestHeaders = validators <> HTTP.requestHeaders req}

-- | Relay in the serve mode, preserving an upstream rejection or transport failure.
relayUpstreamResult ::
    ArtifactServe ->
    ExchangeClient ->
    ProgressFloor ->
    HTTP.Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders, verdict)) ->
    RelayResponder response ->
    IO (UpstreamResult (verdict, response))
relayUpstreamResult mode client progress request =
    withUpstreamResult client progress request (upstreamBody mode)

-- | Relay a public artifact or companion after shared authority admission.
relayPublicResult :: PublicControl -> PublicReadKind -> ArtifactServe -> ExchangeClient -> ProgressFloor -> HTTP.Request -> (Status -> Bool) -> (Status -> ResponseHeaders -> IO (Status, ResponseHeaders, verdict)) -> RelayResponder response -> IO (UpstreamResult (verdict, response))
relayPublicResult control kind mode client progress request = withUpstreamResultUsing (Controlled control kind) client progress request (upstreamBody mode)

upstreamBody :: ArtifactServe -> UpstreamBody
upstreamBody = \case
    ServeFull -> StreamBody
    ServeHead -> NoBody

-- | A successful artifact response, including a matching conditional validator.
acceptArtifact :: Status -> Bool
acceptArtifact s = statusIsSuccessful s || isNotModified s

-- | Private authorisation failures commit a local refusal instead of allowing public fallback.
acceptPrivateArtifact :: Status -> Bool
acceptPrivateArtifact status = acceptArtifact status || isAuthorisationFailure (statusCode status)

-- | Replace private access refusals without invoking either upstream responder.
privateArtifactResponder :: (Status -> IO result) -> (response -> result) -> RelayResponder response -> RelayResponder result
privateArtifactResponder refuse admitted responder =
    RelayResponder
        (\status headers body -> answer status (relayStreamResponse responder status headers body))
        (\status headers -> answer status (relayEmptyResponse responder status headers))
  where
    answer status response
        | isAuthorisationFailure (statusCode status) = refuse status
        | otherwise = admitted <$> response

-- | Forward private headers without the public leg's anomaly scan.
relayUnjudged :: Status -> ResponseHeaders -> IO (Status, ResponseHeaders, ())
relayUnjudged status headers = pure (status, forwardedHeaders headers, ())

-- | Judge public headers before committing the response.
relayJudged :: Status -> ResponseHeaders -> IO (Status, ResponseHeaders, RelayVerdict)
relayJudged status headers = pure (status, forwardedHeaders headers, relayVerdict status headers)

forwardedHeaders :: ResponseHeaders -> ResponseHeaders
forwardedHeaders = filter (not . isHopByHop . fst)
  where
    isHopByHop name = name == "Transfer-Encoding" || name == "Connection"

-- | What the public leg relayed, judged at relay time from the status and headers alone.
data RelayVerdict
    = -- | A success whose headers look like the admitted artifact. A relayed @304@ counts.
      RelayedArtifact
    | -- | A success that does not look like an artifact. Carries a bounded reason.
      RelayedOddShape Text
    | -- | A non-success passed through verbatim. Carries the status.
      RelayedNonSuccess Status
    deriving stock (Eq, Show)

-- | Judge one public relay from its status and headers alone.
relayVerdict :: Status -> ResponseHeaders -> RelayVerdict
relayVerdict status headers
    | isNotModified status = RelayedArtifact
    | not (statusIsSuccessful status) = RelayedNonSuccess status
    | Just contentType <- snd <$> find ((== hContentType) . fst) headers
    , textualContentType contentType =
        RelayedOddShape ("a success carrying a non-artifact content type: " <> decodeUtf8 contentType)
    | otherwise = RelayedArtifact
  where
    textualContentType raw =
        "text/" `BS.isPrefixOf` raw || "application/json" `BS.isPrefixOf` raw

-- | Count bounded anomaly classes and log their unbounded detail.
observeRelayAnomaly :: forall m. (KatipContext m) => MetricsPort -> PackageName -> Version -> RelayVerdict -> m ()
observeRelayAnomaly metrics name version = \case
    RelayedArtifact -> pass
    RelayedOddShape reason -> record Metric.RelayOddShape ("the public upstream answered a success that does not look like the admitted artifact: " <> reason)
    RelayedNonSuccess status -> record Metric.RelayNonSuccess ("the public upstream answered a non-success: HTTP " <> show (statusCode status))
  where
    record :: Metric.RelayAnomaly -> Text -> m ()
    record cls message = do
        liftIO (mpPublicRelayAnomaly metrics cls)
        katipAddContext payload (logFM WarningS (ls message))
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "version" (renderVersion version)
