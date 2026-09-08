-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared HTTP fault classification for registry, queue, and advisory adapters.
Client exceptions become "Ecluse.Core.Fault" values at the adapter boundary.
-}
module Ecluse.Core.Fault.Http (
    classifyTransport,
    isRetryableStatusCode,
) where

import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (
        ConnectionClosed,
        ConnectionFailure,
        ConnectionTimeout,
        InternalException,
        NoResponseDataReceived,
        ResponseTimeout
    ),
 )
import Network.TLS qualified as TLS

import Ecluse.Core.Fault (
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault,
    transportFault,
 )
import Ecluse.Core.Text (displayExceptionT)

-- | Classify a client exception, recognising TLS failures by type rather than rendered text.
classifyTransport :: HttpException -> TransportFault
classifyTransport err = transportFault (causeOf err) (displayExceptionT err)
  where
    causeOf = \case
        HttpExceptionRequest _ content -> case content of
            ConnectionTimeout -> TransportTimeout
            ResponseTimeout -> TransportTimeout
            ConnectionFailure _ -> TransportUnreachable
            ConnectionClosed -> TransportUnreachable
            -- The peer hung up before the first response byte, so the request never
            -- reached a protocol exchange.
            NoResponseDataReceived -> TransportUnreachable
            InternalException inner
                | Just (_ :: TLS.TLSException) <- fromException inner -> TransportTls
                | otherwise -> TransportProtocol
            _ -> TransportProtocol
        InvalidUrlException _ _ -> TransportProtocol

-- | Whether an HTTP status signals a temporary failure: server errors, timeout, or throttling.
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode code = code >= 500 || code == 408 || code == 429
