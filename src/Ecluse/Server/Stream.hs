{- | Bounded-memory artifact streaming — the constant-memory serve path.

The proxy serves an artifact by __streaming it through__ from upstream, never
buffering it whole: a multi-hundred-megabyte tarball must not become a local
memory spike. The trap is resource lifetime. A WAI streaming body __runs after the
handler returns__ (Warp serialises it while writing to the socket), so an upstream
connection released when the handler returns lexically is already gone by the time
the body streams — a use-after-free.

Raw WAI avoids it by construction: 'Network.Wai.Application' is
continuation-passing, so the upstream connection is acquired with @withResponse@
__bracketed around the @respond@ call itself__. The connection then lives for
exactly the duration of the streamed body and is closed only when Warp returns
@ResponseReceived@. 'pumpBody' pulls one chunk from upstream, writes it, and
blocks on the socket send buffer before pulling the next — so the proxy reads from
upstream only as fast as the client drains, giving __constant memory regardless of
artifact size__ with backpressure for free. No @ResourceT@, no conduit on the hot
path (see @docs\/architecture\/web-layer.md@ → "Streaming and resource lifetime").

This is the serve path; it __streams, never buffers__. The whole-artifact-in-memory
'Ecluse.Registry.fetchArtifact' is the separate mirroring concern, not this.
-}
module Ecluse.Server.Stream (
    -- * Streaming a response through
    streamUpstream,
    streamUpstreamWhen,

    -- * The pump
    pumpBody,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Network.HTTP.Client (BodyReader, Manager, Request, brRead, responseHeaders, responseStatus, withResponse)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (ResponseHeaders, Status)
import Network.Wai (Response, ResponseReceived, responseStream)

{- | Stream an upstream response through to the client with constant memory.

The upstream connection is opened with @withResponse@ __bracketed around the
@respond@ call__, so it lives exactly as long as the streamed body and is released
only after Warp returns 'ResponseReceived' — the WAI streaming-lifetime contract.
The body is pumped chunk-by-chunk via 'pumpBody', whose @write@ blocks on the
socket, so upstream is read only as fast as the client drains (backpressure).

The @relay@ argument chooses the client-facing status and headers from upstream's,
so the caller controls what is forwarded (relaying an artifact's status and
content headers unchanged, passing a @304@ straight back, or filtering hop-by-hop
headers) without this helper hard-coding a policy.
-}
streamUpstream ::
    Manager ->
    Request ->
    (Status -> ResponseHeaders -> (Status, ResponseHeaders)) ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
streamUpstream manager request relay respond =
    withResponse request manager $ \upstream ->
        let (status, headers) = relay (responseStatus upstream) (responseHeaders upstream)
         in respond $
                responseStream status headers $ \write flush ->
                    pumpBody (brRead (HTTP.responseBody upstream)) write flush

{- | Stream an upstream response through __only when__ its status passes the
@accept@ predicate, otherwise abandon it without responding.

This is the conditional relay the serve path's __private leg__ needs: it must open
the upstream to learn the status, stream the body on a hit, and on a miss fall
through to another upstream — all without buffering and without leaking the
connection. The upstream is opened with @withResponse@, so the connection is held
only inside the bracket: on a 'Just' it lives exactly as long as the streamed body
(the WAI lifetime contract, as in 'streamUpstream'); on a 'Nothing' the predicate
rejected the status and @withResponse@ closes the connection as the bracket exits,
the body never read. The caller then tries the fall-through leg.

The @accept@ predicate sees only the status (the hit\/miss decision a serve leg
makes); a passing response is relayed exactly as 'streamUpstream' would, the
@relay@ choosing the client-facing status and headers.
-}
streamUpstreamWhen ::
    Manager ->
    Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> (Status, ResponseHeaders)) ->
    (Response -> IO ResponseReceived) ->
    IO (Maybe ResponseReceived)
streamUpstreamWhen manager request accept relay respond =
    withResponse request manager $ \upstream ->
        let upstreamStatus = responseStatus upstream
         in if not (accept upstreamStatus)
                then pure Nothing
                else
                    let (status, headers) = relay upstreamStatus (responseHeaders upstream)
                     in Just
                            <$> respond
                                ( responseStream status headers $ \write flush ->
                                    pumpBody (brRead (HTTP.responseBody upstream)) write flush
                                )

{- | Pump a chunked body from a reader to a WAI stream sink with constant memory.

Each pull reads one chunk; a non-empty chunk is written and flushed before the
next is pulled, so at most one chunk is ever resident. An empty chunk is the
@http-client@ 'BodyReader' end-of-body terminator — the pump stops on it and never
writes it. Because @write@ blocks on the socket send buffer, the loop pulls from
upstream only as fast as the client consumes: backpressure, and bounded memory
independent of body size.

Taking the reader and sink as plain actions (not a @http-client@ response or a WAI
@Response@) keeps the pump's memory and backpressure behaviour testable in process
against an instrumented source and sink, with no socket.
-}
pumpBody :: BodyReader -> (Builder -> IO ()) -> IO () -> IO ()
pumpBody readChunk write flush = loop
  where
    loop :: IO ()
    loop = do
        chunk <- readChunk
        unless (BS.null chunk) $ do
            write (byteString chunk)
            flush
            loop
