-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pure HTTP relay-mechanics behind the tarball pipeline:

* the serve-mode plumbing that shapes an upstream artifact request
* the dispatch that relays its response
* the verdict that judges a public relay from its status and headers alone

These are the artifact path's transport mechanics, factored out of the
'Ecluse.Core.Server.Pipeline.Tarball' handler orchestration. They operate on 'Status',
'ResponseHeaders', and 'Network.HTTP.Client.Request' values and on the metrics and log
ports. They touch neither the 'Ecluse.Core.Server.Context.Handler' reader nor the mount's
'Ecluse.Core.Server.Context.PackumentDeps'. The handler half composes them one way,
adapting its route-owned replies onto the 'RelayResponder' this layer drives.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Relay (
    -- * Serve mode
    ArtifactServe (..),

    -- * Shaping the upstream artifact request
    withMethod,
    withValidators,

    -- * Relaying the upstream response
    relayUpstreamWhen,
    acceptArtifact,
    relayArtifact,

    -- * Judging the public relay
    RelayVerdict (..),
    relayVerdict,
    observeRelayAnomaly,
) where

import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, hContentType, methodHead, statusCode, statusIsSuccessful)

import Data.ByteString qualified as BS
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Server.Conditional (isNotModified)
import Ecluse.Core.Server.Stream (RelayResponder, probeUpstreamWhen, streamUpstreamWhen)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpPublicRelayAnomaly))
import Ecluse.Core.Version (Version, renderVersion)
import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)

-- The artifact serve mode. Threading it through the artifact path keeps GET and HEAD on the
-- same gating and the same upstream-request construction.
data ArtifactServe
    = -- A GET: stream the artifact body through, enqueuing a mirror job on a public
      -- admit (the demand-driven back-fill).
      ServeFull
    | -- A HEAD: probe the upstream as a HEAD and relay the headers with no body,
      -- enqueuing nothing, because it serves no bytes and so has nothing to mirror.
      ServeHead

{- Tag an upstream artifact request with the serve mode's method. 'ServeFull' keeps the request's
default @GET@. -}
withMethod :: ArtifactServe -> HTTP.Request -> HTTP.Request
withMethod = \case
    ServeFull -> id
    ServeHead -> \req -> req{HTTP.method = methodHead}

{- Relay the client's conditional validators ('forwardValidators') onto an upstream artifact
request, so upstream can answer a @304 Not Modified@ instead of resending an unchanged body. -}
withValidators :: RequestHeaders -> HTTP.Request -> HTTP.Request
withValidators validators req =
    req{HTTP.requestHeaders = validators <> HTTP.requestHeaders req}

{- Relay an upstream artifact response in the serve mode. Both modes keep the same recoverable-miss
and committed split, so a HEAD falls through a private miss to the public origin as a GET does. -}
relayUpstreamWhen ::
    ArtifactServe ->
    Manager ->
    HTTP.Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
relayUpstreamWhen = \case
    ServeFull -> streamUpstreamWhen
    ServeHead -> probeUpstreamWhen

{- The private relay accepts a @2xx@ and a @304 Not Modified@, because a @304@ says the client's
relayed validators matched. Any other status is a clean private miss the caller falls through. -}
acceptArtifact :: Status -> Bool
acceptArtifact s = statusIsSuccessful s || isNotModified s

{- Forward the upstream status and headers, dropping only the hop-by-hop framing headers, whose
values describe the upstream hop, not the artifact. The content headers and the @ETag@ pass through
unchanged, so the client verifies @dist.integrity@ over exactly the relayed bytes. -}
relayArtifact :: Status -> ResponseHeaders -> (Status, ResponseHeaders)
relayArtifact status headers =
    (status, filter (not . isHopByHop . fst) headers)
  where
    isHopByHop name = name == "Transfer-Encoding" || name == "Connection"

{- | What the public leg relayed, judged at relay time from the status and headers alone.
Header-only by design: nothing here hashes, buffers, or inspects a body. Only a 'RelayedArtifact'
enqueues the demand-driven mirror job, because a relayed upstream miss would give the worker a job
it could only drop.
-}
data RelayVerdict
    = {- | A success whose headers look like the admitted artifact (a relayed
      @304@ counts: the validators matched, nothing odd).
      -}
      RelayedArtifact
    | -- | A success that does not look like an artifact. Carries a bounded reason.
      RelayedOddShape Text
    | -- | A non-success passed through verbatim. Carries the status.
      RelayedNonSuccess Status
    deriving stock (Eq, Show)

{- | Judge one public relay from its status and headers alone. An absent or binary content type
counts as the artifact, because this is a header-only tripwire and integrity verification owns
the bytes. Do not compare the admitted metadata's declared size against @Content-Length@: npm's
@dist.unpackedSize@ is the unpacked-tree size, so the comparison would flag every healthy relay.
-}
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

{- Observe one public-relay verdict. An anomaly counts on the bounded @ecluse.serve.relay.anomalies@
metric. The unbounded detail stays on the log line, never on a label. -}
observeRelayAnomaly :: forall m. (KatipContext m) => MetricsPort -> PackageName -> Version -> RelayVerdict -> m ()
observeRelayAnomaly metrics name version = \case
    RelayedArtifact -> pass
    RelayedOddShape reason -> record Metric.RelayOddShape ("the public upstream answered a success that does not look like the admitted artifact: " <> reason)
    RelayedNonSuccess status -> record Metric.RelayNonSuccess ("the public upstream answered a non-success, relayed verbatim: HTTP " <> show (statusCode status))
  where
    record :: Metric.RelayAnomaly -> Text -> m ()
    record cls message = do
        liftIO (mpPublicRelayAnomaly metrics cls)
        katipAddContext payload (logFM WarningS (ls message))
    payload =
        sl "module" ("Ecluse.Core.Server.Pipeline.Tarball.Relay" :: Text)
            <> sl "package" (renderPackageName name)
            <> sl "version" (renderVersion version)
