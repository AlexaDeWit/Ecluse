-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded-memory artifact streaming -- the constant-memory serve path.

The proxy serves an artifact by __streaming it through__ from upstream, never
buffering it whole: a multi-hundred-megabyte tarball must not become a local
memory spike. The trap is resource lifetime. A WAI streaming body __runs after the
handler returns__ (Warp serialises it while writing to the socket), so an upstream
connection released when the handler returns lexically is already gone by the time
the body streams -- a use-after-free.

Raw WAI avoids it by construction: 'Network.Wai.Application' is
continuation-passing, so the upstream connection is opened explicitly with
@responseOpen@ before the response is committed and closed with @responseClose@ run
through @finally@ around the whole streamed relay. The open-to-@finally@ handoff is
masked, so an async exception (the request timeout's kill, or Warp tearing the
handler down on client disconnect) cannot strike between @responseOpen@ returning and
@finally@ arming @responseClose@ and strand the connection. The connection then lives
for exactly the duration of the streamed body and is closed on every path, even under
cancellation, only once Warp has returned @ResponseReceived@. 'pumpBody' pulls one chunk from upstream, writes it through
the sink's bounded output buffer -- blocking on the socket send whenever it
spills -- before pulling the next, so the proxy reads from upstream only as fast
as the client drains, giving __constant memory regardless of artifact size__ with
backpressure for free. Only the first chunk is explicitly flushed (prompt first
byte); the rest coalesce in the output buffer, so the relay pays fewer socket
sends than upstream chunks. No @ResourceT@, no conduit on the hot path (see
@docs\/architecture\/web-layer.md@ → "Streaming and resource lifetime").

This is the serve path; it __streams, never buffers__. The mirror worker's
whole-artifact fetch ('Ecluse.Core.Worker.Fetch.fetchArtifactBytes'), bounded and
buffered, is the separate mirroring concern, not this.
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

This is the conditional relay the serve path's __private-origin fetch__ needs: open the
upstream, learn its status, stream the body on a hit, and on a miss fall through to
another upstream -- without buffering and without leaking the connection. The two
outcomes are deliberately kept apart:

* __Recoverable miss__ -- the connection could not be opened, or the status fails
  @accept@. No response has been committed, so the connection is closed and
  'Nothing' is returned and the caller may fall through to another upstream.
* __Committed stream__ -- the status passed, so the response is begun on the wire.
  From that point a failure pumping the body is __unrecoverable__: it is __not__
  collapsed into a miss (that would call @respond@ a second time over a half-sent
  response), but propagates -- the connection torn down as it unwinds -- so the
  caller fails internally rather than responding again.

A passing 'isNotModified' (@304 Not Modified@) status is the __pass-through
conditional-GET relay__: it is committed like any accepted status, but answered
__bodiless__ (through 'relayEmptyResponse') rather than pumped, since a @304@
carries no body (RFC 9110 §15.4.5) -- the upstream body reader is never read. This is
how a client validator relayed upstream that matches comes straight back as a @304@,
the artifact never re-downloaded.

Only the connection open is caught here; once @respond@ is reached exceptions fly.
The connection is released on every path: a rejected status closes it before
returning, a streamed (or failed) body closes it as the stream unwinds.

The @accept@ predicate sees only the status (the hit\/miss decision a serve fetch
makes); a passing response is relayed with the @relay@ choosing the client-facing
status and headers. @relay@ runs in 'IO' --
once, pre-commit, on the accepted status and headers -- so a caller can observe
what it is about to relay (the public leg's relay verdict) without this
function knowing about verdicts.
-}
streamUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
streamUpstreamWhen manager request accept relay respond =
    -- The connection open is the recoverable phase: a failure here is a clean miss
    -- the caller may fall through on. Once a 2xx hands off to 'respond' the response
    -- is committed, so a body failure there is left to propagate (not caught into a
    -- 'Nothing'); the connection is closed on every path as the stream unwinds.
    --
    -- The open-to-'finally' handoff runs masked so an async exception (the request
    -- timeout's kill, or Warp tearing the handler down on client disconnect) cannot
    -- strike between 'responseOpen' returning the connection and 'finally' arming
    -- 'responseClose' over it, which would strand the connection. 'restore' keeps the
    -- open and the pump interruptible; only the decision-and-attach handoff is pinned.
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
                    -- A 304 carries no body: relay it bodiless rather than pumping (the
                    -- upstream body reader is never read), the pass-through conditional-GET
                    -- not-modified relay.
                    Just <$> relayEmptyResponse respond status headers
                else Just <$> relayStreamResponse respond status headers pump
      where
        upstreamStatus = responseStatus upstream
        pump = pumpBody (brRead (HTTP.responseBody upstream))

{- | Probe an upstream __without pumping a body__ -- the bodiless relay a @HEAD@
takes, so a client cannot force the proxy to open the upstream artifact connection
and stream a whole artifact to nowhere (the GET-pump amplification a HEAD must never
trigger).

The @request@ must already carry the @HEAD@ method (the caller sets it), so the
upstream sees a bodiless request too and replies with headers and no body. This
mirrors 'streamUpstreamWhen''s hit\/miss split, but the committed phase answers with
'relayEmptyResponse' rather than the streaming pump:

* __Recoverable miss__ -- the connection could not be opened, or the status fails
  @accept@; no response is committed, the connection is closed, and 'Nothing' is
  returned so the caller may fall through to another upstream.
* __Committed reply__ -- the status passed, so a bodiless response is sent with the
  relayed status and headers. The upstream body reader is never read.

The @relay@ chooses the client-facing status and headers from upstream's (the same
header-filtering the streamed path applies), so a @HEAD@ relays an artifact's content
headers -- @Content-Type@, @Content-Length@, @ETag@, and the like -- exactly as the
matching @GET@ would, only without the bytes. The connection is released on every
path; nothing is pumped, so there is no mid-stream phase to guard.
-}
probeUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders)) ->
    RelayResponder response ->
    IO (Maybe response)
probeUpstreamWhen manager request accept relay respond =
    -- Masked open-to-'finally' handoff, as in 'streamUpstreamWhen': an async exception
    -- must not strike between 'responseOpen' returning and 'finally' arming
    -- 'responseClose', which would strand the connection. 'restore' keeps the open (and
    -- the bodiless probe) interruptible; only the decision-and-attach handoff is pinned.
    mask $ \restore ->
        tryAny (restore (responseOpen request manager)) >>= \case
            Left _ -> pure Nothing
            Right upstream -> restore (probe upstream) `finally` responseClose upstream
  where
    probe upstream
        | not (accept upstreamStatus) = pure Nothing
        | otherwise = do
            (status, headers) <- relay upstreamStatus (responseHeaders upstream)
            -- A HEAD reply carries no body; the upstream body reader is never read.
            Just <$> relayEmptyResponse respond status headers
      where
        upstreamStatus = responseStatus upstream

{- | Pump a chunked body from a reader to a WAI stream sink with constant memory.

Each pull reads one chunk and writes it before the next is pulled, so at most one
chunk (plus the sink's fixed output buffer) is ever resident. An empty chunk is
the @http-client@ 'BodyReader' end-of-body terminator -- the pump stops on it and
never writes it. Because @write@ fills the sink's bounded output buffer and blocks
on the socket send whenever it spills, the loop pulls from upstream only as fast
as the client consumes: backpressure, and bounded memory independent of body size.

Only the __first__ chunk is explicitly flushed, so the response's status, headers,
and opening bytes reach the client promptly (time to first byte) even when
upstream trickles. Later chunks are deliberately __not__ flushed per chunk: at
relay byte rates a per-chunk flush degenerates into a socket send per upstream
read, and letting the sink coalesce writes into its buffer raises the streaming
ceiling. The sink flushes whatever remains when the stream ends (Warp's
stream-close contract), so the tail is never stranded.

Taking the reader and sink as plain actions (not a @http-client@ response or a WAI
@Response@) keeps the pump's memory and backpressure behaviour testable in process
against an instrumented source and sink, with no socket.
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
