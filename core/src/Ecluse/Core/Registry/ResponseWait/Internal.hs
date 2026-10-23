-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The wait "Ecluse.Core.Registry.ResponseWait" keeps for each exchange, its pure transitions, and
the opener under any length of wait. Importing this module opts out of the public surface's
stability promises. It exists so a spec can hold the transitions and a short wait to account.
-}
module Ecluse.Core.Registry.ResponseWait.Internal (
    ResponseWait (..),
    connectedAt,
    uploadedAt,
    waitFault,
    openResponseAfter,
) where

import Control.Exception (asyncExceptionFromException, asyncExceptionToException, handleJust)
import Data.ByteString qualified as BS
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (
    BodyReader,
    GivesPopper,
    HttpException (HttpExceptionRequest),
    HttpExceptionContent (ConnectionTimeout, ResponseTimeout),
    Request (checkResponse, requestBody, responseTimeout),
    RequestBody (..),
    Response,
    responseOpen,
    responseTimeoutNone,
 )
import Network.HTTP.Client.Internal (throwHttp)
import Text.Show (showsPrec)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Deadline (withEndableDeadline)
import Ecluse.Core.Fault.Http (showsUntagged)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (..))

{- | Where an exchange stands, each with the monotonic nanoseconds that bound it: the connection's
deadline, the wait an upload leaves for the headers, or the headers' deadline.
-}
data ResponseWait
    = Connecting {-# UNPACK #-} !Word64
    | Uploading {-# UNPACK #-} !Word64
    | Answering {-# UNPACK #-} !Word64
    deriving stock (Eq, Show)

{- | The wait the headers have left once a connection is in hand, given the whole wait. A second
connection is http-client retrying a reused one, which starts its wait afresh.
-}
connectedAt :: Word64 -> Word64 -> ResponseWait -> Either HttpExceptionContent Word64
connectedAt whole now = \case
    Connecting deadline
        | now >= deadline -> Left ConnectionTimeout
        | otherwise -> Right (deadline - now)
    Uploading _ -> Right whole
    Answering _ -> Right whole

-- | Start the headers' deadline once the request body is written.
uploadedAt :: Word64 -> ResponseWait -> ResponseWait
uploadedAt now = \case
    Uploading left -> Answering (now + left)
    other -> other

-- | The timeout an exchange takes at a monotonic time, if its wait ran out.
waitFault :: Word64 -> ResponseWait -> Maybe HttpExceptionContent
waitFault now = \case
    Connecting deadline | now >= deadline -> Just ConnectionTimeout
    Answering deadline | now >= deadline -> Just ResponseTimeout
    _ -> Nothing

{- | 'responseOpen' with a wait of this many nanoseconds in place of http-client's response timeout.
The wait ends once the headers are in, before the caller sees the response.
-}
openResponseAfter :: Word64 -> ExchangeClient -> Request -> IO (Response BodyReader)
openResponseAfter whole ExchangeClient{ecManager, ecDeadlines} request = do
    started <- getMonotonicTimeNSec
    wait <- newIORef $! Connecting (started + whole)
    scope <- newIORef ()
    let expired now = fmap (toException . WaitExpired scope) . waitFault now <$> readIORef wait
        ours (WaitExpired raised content) = if raised == scope then Just content else Nothing
        timed answered =
            request
                { requestBody = RequestBodyIO (sending whole wait (requestBody request))
                , responseTimeout = responseTimeoutNone
                , checkResponse = \sent response -> answered >> checkResponse request sent response
                }
    handleJust ours (throwIO . HttpExceptionRequest request) $
        withEndableDeadline ecDeadlines expired (\answered -> responseOpen (timed answered) ecManager)

{- http-client builds the body once it holds a connection, just before writing the request. Only a
streamed body shows its end, and a metered upload is always one. -}
sending :: Word64 -> IORef ResponseWait -> RequestBody -> IO RequestBody
sending whole wait body = do
    now <- getMonotonicTimeNSec
    -- Thrown as http-client throws its own, which tags it with the request and closes the connection.
    left <- either throwHttp pure . connectedAt whole now =<< readIORef wait
    built <- resolved body
    case built of
        RequestBodyStream size gives -> RequestBodyStream size (endingWith uploaded gives) <$ settle (Uploading left)
        RequestBodyStreamChunked gives -> RequestBodyStreamChunked (endingWith uploaded gives) <$ settle (Uploading left)
        other -> other <$ settle (Answering (now + left))
  where
    -- The sweeper reads the wait from another thread, so it only ever holds an evaluated state.
    settle next = writeIORef wait $! next
    uploaded = getMonotonicTimeNSec >>= \now -> modifyIORef' wait (uploadedAt now)

resolved :: RequestBody -> IO RequestBody
resolved = \case
    RequestBodyIO build -> build >>= resolved
    other -> pure other

-- The connection pulls until a slice comes back empty.
endingWith :: IO () -> GivesPopper () -> GivesPopper ()
endingWith ended gives needsPopper = gives $ \popper -> needsPopper (popper >>= \slice -> slice <$ when (BS.null slice) ended)

{- Raised asynchronously, as http-client's own timer is, so a TLS handshake that wraps synchronous
faults passes it on. 'openResponseAfter' turns it back into the client's fault. -}
data WaitExpired = WaitExpired (IORef ()) HttpExceptionContent

-- Shown as http-client shows its own timeout before it tags it with the request.
instance Show WaitExpired where
    showsPrec precedence (WaitExpired _ content) = showsUntagged precedence content

instance Exception WaitExpired where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException
