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

{- | The two ways an upstream relay can answer, parameterised by the route-scoped
response value the caller sends. Keeping WAI construction out of this module lets a
pipeline retain the upstream connection's callback lifetime without receiving an
unrestricted WAI responder.
-}
data RelayResponder response = RelayResponder
    { relayStreamResponse :: Status -> ResponseHeaders -> StreamingBody -> IO response
    -- ^ Commit a status, headers, and bounded-memory streaming body.
    , relayEmptyResponse :: Status -> ResponseHeaders -> IO response
    -- ^ Commit the same response without a body (a @304@ or @HEAD@).
    }

{- | Stream an upstream response through __only when__ its status passes the
@accept@ predicate, keeping a recoverable miss distinct from an unrecoverable
mid-stream failure.

This is the conditional relay the serve path's __private-origin fetch__ needs. It opens
the upstream, learns its status, streams the body on a hit, and on a miss falls through
to another upstream. It does that without buffering and without leaking the connection.
The two outcomes are deliberately kept apart:

* A __recoverable miss__: the connection could not be opened, or the status fails
  @accept@. This function commits no response, closes the connection, and returns
  'Nothing', so the caller may fall through to another upstream.
* A __committed stream__: the status passed, so the response is begun on the wire. From
  that point a failure pumping the body is __unrecoverable__. It is __not__ collapsed
  into a miss, because that would call @respond@ a second time over a half-sent
  response. It propagates instead, tearing the connection down as it unwinds, so the
  caller fails internally rather than responding again.

A passing 'isNotModified' (@304 Not Modified@) status is the __pass-through
conditional-GET relay__. It is committed like any accepted status, but answered
__bodiless__ (through 'relayEmptyResponse') rather than pumped, since a @304@ carries no
body (RFC 9110 §15.4.5). The upstream body reader is never read. A client validator
relayed upstream that matches therefore comes straight back as a @304@, and the artifact
is never re-downloaded.

Only the connection open is caught here. Once @respond@ is reached exceptions fly. The
connection is released on every path. A rejected status closes it before returning, and
a streamed or failed body closes it as the stream unwinds.

The @accept@ predicate sees only the status, which is the hit\/miss decision a serve
fetch makes. The @relay@ chooses the client-facing status and headers for a passing
response. @relay@ runs in 'IO' once, pre-commit, on the accepted status and headers. A
caller can therefore observe what it is about to relay: the public leg's relay verdict.
This function itself knows nothing about verdicts.
-}
streamUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
streamUpstreamWhen manager request accept relay respond =
    -- The connection open is the recoverable phase: a failure here is a clean miss the
    -- caller may fall through on. Once a 2xx hands off to 'respond' the response is
    -- committed, so a body failure there propagates rather than being caught into a
    -- 'Nothing'. The connection closes on every path as the stream unwinds.
    --
    -- The open-to-'finally' handoff runs masked. An async exception must not strike
    -- between 'responseOpen' returning the connection and 'finally' arming
    -- 'responseClose' over it, which would strand the connection. That exception is the
    -- request timeout's kill, or Warp tearing the handler down on client disconnect.
    -- 'restore' keeps the open and the pump interruptible. Only the decision-and-attach
    -- handoff is pinned.
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
                    -- A 304 carries no body, so relay it bodiless rather than pumping.
                    -- The upstream body reader is never read. This is the pass-through
                    -- conditional-GET not-modified relay.
                    Just <$> relayEmptyResponse respond status headers
                else Just <$> relayStreamResponse respond status headers pump
      where
        upstreamStatus = responseStatus upstream
        pump = pumpBody (brRead (HTTP.responseBody upstream))

{- | Probe an upstream __without pumping a body__: the bodiless relay a @HEAD@ takes. A
client therefore cannot force the proxy to open the upstream artifact connection and
stream a whole artifact to nowhere. That is the GET-pump amplification a HEAD must never
trigger.

The @request@ must already carry the @HEAD@ method, which the caller sets. The upstream
then sees a bodiless request too, and replies with headers and no body. This mirrors
'streamUpstreamWhen''s hit\/miss split, but the committed phase answers with
'relayEmptyResponse' rather than the streaming pump:

* A __recoverable miss__: the connection could not be opened, or the status fails
  @accept@. This function commits no response, closes the connection, and returns
  'Nothing', so the caller may fall through to another upstream.
* A __committed reply__: the status passed, so a bodiless response is sent with the
  relayed status and headers. The upstream body reader is never read.

The @relay@ chooses the client-facing status and headers from upstream's, applying the
same header-filtering as the streamed path. A @HEAD@ therefore relays an artifact's
content headers exactly as the matching @GET@ would, only without the bytes. Those
headers are @Content-Type@, @Content-Length@, @ETag@, and the like. The connection is
released on every path. Nothing is pumped, so there is no mid-stream phase to guard.
-}
probeUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
probeUpstreamWhen manager request accept relay respond =
    -- Masked open-to-'finally' handoff, as in 'streamUpstreamWhen'. An async exception
    -- must not strike between 'responseOpen' returning and 'finally' arming
    -- 'responseClose', which would strand the connection. 'restore' keeps the open, and
    -- the bodiless probe, interruptible. Only the decision-and-attach handoff is pinned.
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

{- | Pump a chunked body from a reader to a WAI stream sink with constant memory.

Each pull reads one chunk and writes it before the pump pulls the next. At most one
chunk plus the sink's fixed output buffer is ever resident. An empty chunk is the
@http-client@ 'BodyReader' end-of-body terminator. The pump stops on it and never writes
it. The @write@ action fills the sink's bounded output buffer, and blocks on the socket
send whenever it spills. The loop therefore pulls from upstream only as fast as the
client consumes. That is backpressure, and bounded memory independent of body size.

Only the __first__ chunk is explicitly flushed. The response's status, headers, and
opening bytes therefore reach the client promptly (time to first byte), even when
upstream trickles. Later chunks are deliberately __not__ flushed per chunk. At relay
byte rates a per-chunk flush degenerates into a socket send per upstream read. Letting
the sink coalesce writes into its buffer raises the streaming ceiling. The sink
flushes whatever remains when the stream ends (Warp's stream-close contract), so the
tail is never stranded.

Taking the reader and sink as plain actions, not a @http-client@ response or a WAI
@Response@, keeps the pump testable in process. A test drives its memory and
backpressure behaviour against an instrumented source and sink, with no socket.
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
