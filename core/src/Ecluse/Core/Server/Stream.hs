-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded-memory artifact streaming: the constant-memory serve path.

The proxy serves an artifact by __streaming it through__ from upstream, never buffering
it whole. A multi-hundred-megabyte tarball must not become a local memory spike. The
trap is resource lifetime. A WAI streaming body __runs after the handler returns__:
Warp serialises it while writing to the socket. An upstream connection released when the
handler returns lexically is therefore already gone by the time the body streams. That
is a use-after-free.

Raw WAI avoids it by construction. 'Network.Wai.Application' is continuation-passing.
The relay opens the upstream connection explicitly with @responseOpen@ before it commits
the response. It closes the connection with @responseClose@, run through @finally@
around the whole streamed relay. The open-to-@finally@ handoff is masked, so an async
exception cannot strike between @responseOpen@ returning and @finally@ arming
@responseClose@, and strand the connection. That exception is the request timeout's
kill, or Warp tearing the handler down on client disconnect. The connection then lives
for exactly the duration of the streamed body, and closes on every path, even under
cancellation, only after Warp returns @ResponseReceived@.

'pumpBody' pulls one chunk from upstream and writes it through the sink's bounded output
buffer before pulling the next. The write blocks on the socket send whenever the buffer
spills, so the proxy reads from upstream only as fast as the client drains. That gives
__constant memory regardless of artifact size__, with backpressure for free. Only the
first chunk is explicitly flushed, for a prompt first byte. The rest coalesce in the
output buffer, so the relay pays fewer socket sends than upstream chunks. No
@ResourceT@, no conduit on the hot path (see @docs\/architecture\/web-layer.md@ →
"Streaming and resource lifetime").

This is the serve path, and it __streams, never buffers__. The mirror worker's
whole-artifact fetch ('Ecluse.Core.Worker.Fetch.fetchArtifactBytes') is bounded and
buffered. That is the separate mirroring concern, not this.
-}
module Ecluse.Core.Server.Stream (
    -- * A typed relay responder
    RelayResponder (..),

    -- * Streaming a response through
    streamUpstreamWhen,

    -- * Probing without a body (HEAD)
    probeUpstreamWhen,

    -- * The pump
    pumpBody,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Network.HTTP.Client (BodyReader, Manager, Request, brRead, responseClose, responseHeaders, responseOpen, responseStatus)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (ResponseHeaders, Status)
import Network.Wai (StreamingBody)
import UnliftIO.Exception (finally, mask, tryAny)

import Ecluse.Core.Server.Conditional (isNotModified)

{- | The two ways an upstream relay can answer, over the caller's route-scoped response value.
WAI construction stays out of this module, so a pipeline can hold the upstream connection's
callback lifetime without holding an unrestricted WAI responder.
-}
data RelayResponder response = RelayResponder
    { relayStreamResponse :: Status -> ResponseHeaders -> StreamingBody -> IO response
    -- ^ Commit a status, headers, and bounded-memory streaming body.
    , relayEmptyResponse :: Status -> ResponseHeaders -> IO response
    -- ^ Commit the same response without a body (a @304@ or @HEAD@).
    }

{- | Stream an upstream response through only when its status passes @accept@, keeping a
recoverable miss distinct from an unrecoverable mid-stream failure.

A miss (the open failed, or @accept@ rejected the status) commits no response, closes the
connection, and returns 'Nothing', so the caller may fall through to another upstream. Once
a passing status commits the response, a body failure propagates instead: collapsing it into
a miss would call @respond@ a second time over a half-sent response.

A @304@ commits bodiless through 'relayEmptyResponse' and upstream's body reader is never
read, because a @304@ carries no body (RFC 9110 §15.4.5). @relay@ runs once, pre-commit, on
the accepted status and headers. The connection is released on every path.
-}
streamUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
streamUpstreamWhen manager request accept relay respond =
    -- The open-to-'finally' handoff runs masked. An async exception between 'responseOpen'
    -- returning the connection and 'finally' arming 'responseClose' would strand it. That
    -- exception is the request timeout's kill, or Warp tearing the handler down on client
    -- disconnect. 'restore' keeps the open and the pump interruptible.
    mask $ \restore ->
        tryAny (restore (responseOpen request manager)) >>= \case
            Left _ -> pure Nothing
            Right upstream -> restore (stream upstream) `finally` responseClose upstream
  where
    stream upstream
        | not (accept upstreamStatus) = pure Nothing
        | otherwise = do
            (status, headers) <- relay upstreamStatus (responseHeaders upstream)
            if isNotModified upstreamStatus
                then
                    -- A 304 carries no body, so answer bodiless without reading upstream's.
                    Just <$> relayEmptyResponse respond status headers
                else Just <$> relayStreamResponse respond status headers pump
      where
        upstreamStatus = responseStatus upstream
        pump = pumpBody (brRead (HTTP.responseBody upstream))

{- | Probe an upstream without pumping a body: the bodiless relay a @HEAD@ takes. A client
therefore cannot force the proxy to open an upstream artifact connection and stream a whole
artifact to nowhere, which is the amplification a @HEAD@ must never trigger.

The caller must already have set @HEAD@ on @request@, so upstream replies with headers and no
body. The hit and miss split matches 'streamUpstreamWhen', except that a passing status
commits a bodiless 'relayEmptyResponse' and upstream's body reader is never read. @relay@
picks the client-facing status and headers with the same filtering as the streamed path, so a
@HEAD@ relays an artifact's content headers exactly as its @GET@ would. The connection is
released on every path.
-}
probeUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
probeUpstreamWhen manager request accept relay respond =
    -- Masked open-to-'finally' handoff, as in 'streamUpstreamWhen'. An async exception between
    -- 'responseOpen' returning and 'finally' arming 'responseClose' would strand the connection.
    -- 'restore' keeps the open and the probe interruptible.
    mask $ \restore ->
        tryAny (restore (responseOpen request manager)) >>= \case
            Left _ -> pure Nothing
            Right upstream -> restore (probe upstream) `finally` responseClose upstream
  where
    probe upstream
        | not (accept upstreamStatus) = pure Nothing
        | otherwise = do
            (status, headers) <- relay upstreamStatus (responseHeaders upstream)
            -- A HEAD reply carries no body, so the upstream body reader is never read.
            Just <$> relayEmptyResponse respond status headers
      where
        upstreamStatus = responseStatus upstream

{- | Pump a chunked body from a reader to a WAI stream sink in constant memory.

At most one chunk plus the sink's fixed output buffer is resident, and @write@ blocks when
that buffer spills, so the pump pulls upstream only as fast as the client consumes. An empty
chunk is the @http-client@ 'BodyReader' end-of-body terminator, and the pump never writes it.

Only the first chunk is flushed, so the status, headers, and opening bytes reach the client
promptly. Later chunks are deliberately not flushed: at relay byte rates a per-chunk flush
degenerates into one socket send per upstream read. Warp's stream-close contract flushes the
tail. Taking plain reader and sink actions keeps the pump testable in process, with no socket.
-}
pumpBody :: BodyReader -> (Builder -> IO ()) -> IO () -> IO ()
pumpBody readChunk write flush = do
    opening <- readChunk
    unless (BS.null opening) $ do
        write (byteString opening)
        flush
        rest
  where
    rest :: IO ()
    rest = do
        chunk <- readChunk
        unless (BS.null chunk) $ do
            write (byteString chunk)
            rest
