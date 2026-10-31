-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Artifact relays with bounded memory and upstream backpressure.
The connection stays open until the responder finishes. The progress floor
counts upstream waits, excluding time spent waiting for the client.
-}
module Ecluse.Core.Server.Stream (
    -- * A typed relay responder
    RelayResponder (..),

    -- * Relaying an upstream response through
    UpstreamBody (..),
    UpstreamResult (..),
    withUpstreamResult,
    withUpstreamResultUsing,
    UpstreamBodyFailed (..),

    -- * The pump
    pumpBody,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Network.HTTP.Client (BodyReader, Request, brRead, responseClose, responseHeaders, responseStatus)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (ResponseHeaders, Status)
import Network.Wai (StreamingBody)
import UnliftIO.Exception (finally, mask, throwIO, try, tryAny)

import Ecluse.Core.Fault (RetryAfter, TransportFault, renderTransportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Fault.RetryAfter (responseRetryAfter)
import Ecluse.Core.Registry (FetchFault (FetchCooldown))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Progress (meteredReader, watchedRaising)
import Ecluse.Core.Registry.Public (ReadControl (Uncontrolled), withReadControl)
import Ecluse.Core.Registry.ResponseWait (openResponse)
import Ecluse.Core.Security (ProgressFloor)
import Ecluse.Core.Server.Conditional (isNotModified)

-- | Route-scoped responders that keep WAI response construction at the caller.
data RelayResponder response = RelayResponder
    { relayStreamResponse :: Status -> ResponseHeaders -> StreamingBody -> IO response
    -- ^ Commit a status, headers, and bounded-memory streaming body.
    , relayEmptyResponse :: Status -> ResponseHeaders -> IO response
    -- ^ Commit the same response without a body (a @304@ or @HEAD@).
    }

-- | Whether a relay pumps the upstream body through, or answers bodiless.
data UpstreamBody
    = -- | Stream the body, except for a bodiless @304@.
      StreamBody
    | -- | Never read the upstream body, including for @HEAD@.
      NoBody
    deriving stock (Eq, Show)

-- | A committed relay, or the reason no response was committed.
data UpstreamResult a
    = -- | The responder finished, including any body streaming.
      UpstreamRelayed a
    | -- | The upstream answered a status the caller did not accept, with any delay its @Retry-After@ asked for.
      UpstreamRejected Status (Maybe RetryAfter)
    | -- | The upstream connection failed before a response was available.
      UpstreamTransportFailure
    | -- | Public admission refused the exchange before opening a connection.
      UpstreamCooling RetryAfter
    deriving stock (Eq, Show)

-- | A body failure after response commitment requires closing the client connection. The fault never includes the upstream request.
newtype UpstreamBodyFailed = UpstreamBodyFailed TransportFault
    deriving stock (Eq, Show)

instance Exception UpstreamBodyFailed where
    displayException (UpstreamBodyFailed fault) = toString ("the upstream body failed after the response was committed: " <> renderTransportFault fault)

-- | Relay accepted responses, preserving pre-commit misses and propagating failures after the commit.
withUpstreamResult ::
    ExchangeClient ->
    ProgressFloor ->
    Request ->
    UpstreamBody ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> IO (Status, ResponseHeaders, verdict)) ->
    RelayResponder response ->
    IO (UpstreamResult (verdict, response))
withUpstreamResult = withUpstreamResultUsing Uncontrolled

-- | Apply public admission and observation while retaining response ownership through the responder.
withUpstreamResultUsing :: ReadControl -> ExchangeClient -> ProgressFloor -> Request -> UpstreamBody -> (Status -> Bool) -> (Status -> ResponseHeaders -> IO (Status, ResponseHeaders, verdict)) -> RelayResponder response -> IO (UpstreamResult (verdict, response))
withUpstreamResultUsing control client progress request body accept relay respond = do
    opened <- newIORef False
    result <- tryAny $ withReadControl control request $ \observed recordHeaders ->
        mask $ \restore -> do
            upstream <- openResponse client observed
            writeIORef opened True
            restore (recordHeaders (responseStatus upstream) (responseHeaders upstream) >> answer upstream) `finally` responseClose upstream
    case result of
        Left fault -> readIORef opened >>= \hasResponse -> if hasResponse then throwIO fault else pure UpstreamTransportFailure
        Right (Left (FetchCooldown retry)) -> pure (UpstreamCooling retry)
        Right (Left _) -> pure UpstreamTransportFailure
        Right (Right received) -> pure received
  where
    answer upstream
        | not (accept upstreamStatus) = UpstreamRejected upstreamStatus <$> responseRetryAfter (responseHeaders upstream)
        | otherwise = do
            (status, headers, verdict) <- relay upstreamStatus (responseHeaders upstream)
            received <-
                if bodiless
                    then relayEmptyResponse respond status headers
                    else relayStreamResponse respond status headers pump
            pure (UpstreamRelayed (verdict, received))
      where
        upstreamStatus = responseStatus upstream
        bodiless = body == NoBody || isNotModified upstreamStatus
        -- A body read fails with the upstream request inside, so the fault is typed before it leaves.
        pump write flush =
            try (watchedRaising (ecDeadlines client) progress (\watch -> pumpBody (meteredReader watch (brRead (HTTP.responseBody upstream))) write flush))
                >>= either (throwIO . UpstreamBodyFailed . classifyTransport) pure

-- | Pump chunks with one opening flush. An empty chunk marks the end of the body.
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
