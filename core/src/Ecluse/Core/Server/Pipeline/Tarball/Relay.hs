-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pure HTTP relay-mechanics behind the tarball pipeline: the serve-mode plumbing
that shapes an upstream artifact request, the dispatch that relays its response, and the
verdict that judges a public relay from its status and headers alone.

These are the artifact path's transport mechanics, factored out of the
'Ecluse.Core.Server.Pipeline.Tarball' handler orchestration. They operate on
'Status', 'ResponseHeaders', and 'Network.HTTP.Client.Request' values and the metrics
and log ports, and touch neither the 'Ecluse.Core.Server.Context.Handler' reader nor the
mount's 'Ecluse.Core.Server.Context.PackumentDeps'. The handler half composes them
one-way, adapting its route-owned replies onto the 'RelayResponder' this layer drives.
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

-- The artifact serve mode: a full GET that streams the body through, or a HEAD that
-- probes the upstream bodiless and relays only the headers. Threaded through the
-- artifact path so the gating and upstream-request construction are shared verbatim
-- between the two, differing only in the upstream method, whether a body is pumped,
-- and whether an admit enqueues a mirror job.
data ArtifactServe
    = -- A GET: stream the artifact body through, enqueuing a mirror job on a public
      -- admit (the demand-driven back-fill).
      ServeFull
    | -- A HEAD: probe the upstream as a HEAD and relay the headers with no body,
      -- enqueuing nothing (no bytes are served, so there is nothing to mirror).
      ServeHead

{- Tag an upstream artifact request with the serve mode's method: a 'ServeFull' fetch
keeps the request's default @GET@, a 'ServeHead' probe is marked @HEAD@ so the upstream
sees a bodiless request and the proxy never pumps the body. -}
withMethod :: ArtifactServe -> HTTP.Request -> HTTP.Request
withMethod = \case
    ServeFull -> id
    ServeHead -> \req -> req{HTTP.method = methodHead}

{- Relay the client's conditional validators (the @If-None-Match@ \/ @If-Modified-Since@
'forwardValidators' filtered) onto an upstream artifact request, so upstream can answer
a @304 Not Modified@ for a pass-through body we serve unchanged. An empty validator set
(the client sent none) leaves the request unconditional. -}
withValidators :: RequestHeaders -> HTTP.Request -> HTTP.Request
withValidators validators req =
    req{HTTP.requestHeaders = validators <> HTTP.requestHeaders req}

{- Relay an upstream artifact response in the serve mode: 'ServeFull' streams the body
through with bounded memory ('streamUpstreamWhen'); 'ServeHead' probes bodiless,
relaying the status and headers with no body ('probeUpstreamWhen'). Both keep the same
recoverable-miss / committed split, so a HEAD falls through a private miss to the public
origin exactly as a GET does. -}
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

{- The upstream artifact statuses the private relay accepts back to the client: a
@2xx@ success (the streamed artifact) or a @304 Not Modified@ (the pass-through
conditional-GET relay -- the client's relayed validators matched upstream's, so the
unchanged artifact is answered as a bodiless @304@ by 'streamUpstreamWhen' rather than
re-downloaded). Any other status is a clean private miss the caller falls through on.
(The public relay accepts every status -- it relays whatever the public origin returns
verbatim -- so it needs no predicate of its own.) -}
acceptArtifact :: Status -> Bool
acceptArtifact s = statusIsSuccessful s || isNotModified s

{- The relay for an artifact stream: forward the upstream status and headers,
dropping only the hop-by-hop framing headers (@Transfer-Encoding@, @Connection@)
whose values describe the upstream hop, not the artifact. The body is opaque binary
streamed verbatim, so the content headers (type, length, encoding) and the
upstream's @ETag@ pass through unchanged -- the client verifies the artifact's own
@dist.integrity@ over exactly these bytes. -}
relayArtifact :: Status -> ResponseHeaders -> (Status, ResponseHeaders)
relayArtifact status headers =
    (status, filter (not . isHopByHop . fst) headers)
  where
    isHopByHop name = name == "Transfer-Encoding" || name == "Connection"

{- | What the public leg relayed, judged at relay time from the status and headers
alone -- the body always relays verbatim, and client-side plus worker
@dist.integrity@ verification stay the guarantors of the bytes. Header-only by
design: nothing here hashes, buffers, or inspects a body, and the private leg
computes no verdict at all.

The verdict's consumer side: a non-'RelayedArtifact' is logged and counted
(@ecluse.serve.relay.anomalies@), and only a 'RelayedArtifact' enqueues the
demand-driven mirror job -- a relayed upstream miss used to enqueue a doomed job
that the worker could only drop after a metadata round trip.
-}
data RelayVerdict
    = {- | A success whose headers look like the admitted artifact (a relayed
      @304@ counts: the validators matched, nothing odd).
      -}
      RelayedArtifact
    | -- | A success that does not look like an artifact (carried, bounded reason).
      RelayedOddShape Text
    | -- | A non-success passed through verbatim (carried).
      RelayedNonSuccess Status
    deriving stock (Eq, Show)

{- | Judge one public relay from its status and headers. A @304@ is a clean
pass-through (the relayed validators matched); any other non-2xx is the relayed
non-success; a 2xx whose @Content-Type@ is textual (@text\/*@, or JSON where a
tarball was admitted) is the odd shape -- an upstream answering a success that is
visibly not the artifact. An absent or binary content type is taken as the
artifact: this is a header-only tripwire, not a validator (integrity
verification owns the bytes).

The admitted metadata's declared size is deliberately __not__ compared against
@Content-Length@: for npm the declared size is the unpacked-tree size
(@dist.unpackedSize@), which never equals the transfer length, so the comparison
would flag every healthy relay.
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

{- Observe one public-relay verdict on its consumer side: a clean artifact relay
is silent; an anomaly is counted on the bounded @ecluse.serve.relay.anomalies@
metric and logged WARNING with the package coordinates (the unbounded detail
stays on the log line, never a label). The verdict never changes what the client
received -- the body already relayed verbatim. -}
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
