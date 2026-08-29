-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded-memory artifact streaming: the constant-memory serve path.

The proxy serves an artifact by __streaming it through__ from upstream, never buffering it
whole, so a multi-hundred-megabyte tarball never becomes a local memory spike. The mirror
worker's whole-artifact fetch ('Ecluse.Core.Worker.Fetch.fetchArtifactBytes') is the
separate, buffered mirroring concern.

== Resource lifetime

A WAI streaming body __runs after the handler returns__, so an upstream connection released
lexically is already gone by the time the body streams. Raw WAI avoids that: the relay opens
the connection with @responseOpen@ before it commits the response, and closes it through
@finally@ around the whole relay. The open-to-@finally@ handoff is masked, so the request
timeout's kill or Warp's teardown on client disconnect cannot strand the connection between
@responseOpen@ returning and @finally@ arming @responseClose@.

== Backpressure

'pumpBody' writes each chunk through the sink's bounded output buffer before pulling the
next, and the write blocks once that buffer spills. The proxy therefore reads upstream only
as fast as the client drains, in __constant memory whatever the artifact's size__. Only the
first chunk is flushed, for a prompt first byte: at relay byte rates a per-chunk flush
degenerates into one socket send per upstream read (see @docs\/architecture\/web-layer.md@ →
"Streaming and resource lifetime").
-}
module Ecluse.Core.Server.Stream (
    -- * A typed relay responder
    RelayResponder (..),

    -- * Relaying an upstream response through
    UpstreamBody (..),
    withUpstreamWhen,

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

{- | The two ways an upstream relay can answer, over the caller's route-scoped response value. WAI
construction stays out of this module, so no pipeline holds an unrestricted WAI responder.
-}
data RelayResponder response = RelayResponder
    { relayStreamResponse :: Status -> ResponseHeaders -> StreamingBody -> IO response
    -- ^ Commit a status, headers, and bounded-memory streaming body.
    , relayEmptyResponse :: Status -> ResponseHeaders -> IO response
    -- ^ Commit the same response without a body (a @304@ or @HEAD@).
    }

-- | Whether a relay pumps the upstream body through, or answers bodiless.
data UpstreamBody
    = {- | Stream the body through. A @304@ still answers bodiless, because it carries
      no body (RFC 9110 §15.4.5) and upstream's reader is never read.
      -}
      StreamBody
    | {- | Answer bodiless and never read upstream's body reader. A @HEAD@ takes this,
      so a client cannot make the proxy stream a whole artifact to nowhere.
      -}
      NoBody
    deriving stock (Eq, Show)

{- | Relay an upstream response when its status passes @accept@. 'Nothing' is a recoverable
miss that commits nothing, and a failure after the commit propagates rather than re-answering.
-}
withUpstreamWhen ::
    Manager ->
    Request ->
    UpstreamBody ->
    -- | Whether upstream's status is a hit. A rejected status is a clean miss.
    (Status -> Bool) ->
    {- | Run once, pre-commit, on the accepted status and headers: the client-facing status
    and headers, plus the caller's own verdict on the relay.
    -}
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders, verdict)) ->
    RelayResponder response ->
    IO (Maybe (verdict, response))
withUpstreamWhen manager request body accept relay respond =
    -- Masked from 'responseOpen' to 'finally' arming 'responseClose', so an async exception
    -- between the two cannot strand the connection. 'restore' keeps the relay interruptible.
    mask $ \restore ->
        tryAny (restore (responseOpen request manager)) >>= \case
            Left _ -> pure Nothing
            Right upstream -> restore (answer upstream) `finally` responseClose upstream
  where
    answer upstream
        | not (accept upstreamStatus) = pure Nothing
        | otherwise = do
            (status, headers, verdict) <- relay upstreamStatus (responseHeaders upstream)
            received <-
                if bodiless
                    then relayEmptyResponse respond status headers
                    else relayStreamResponse respond status headers pump
            pure (Just (verdict, received))
      where
        upstreamStatus = responseStatus upstream
        bodiless = body == NoBody || isNotModified upstreamStatus
        pump = pumpBody (brRead (HTTP.responseBody upstream))

{- | Pump a chunked body from a reader to a WAI stream sink in constant memory. An empty chunk
is @http-client@'s 'BodyReader' end-of-body terminator, and the pump never writes it.
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
