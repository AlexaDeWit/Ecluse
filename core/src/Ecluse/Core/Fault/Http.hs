-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared HTTP fault classification for registry, queue, and advisory adapters.
Client exceptions become "Ecluse.Core.Fault" values at the adapter boundary. Neither a value nor
its rendering carries the request or the response: a header, a query a redirect reached, or a
cookie can carry a credential.
-}
module Ecluse.Core.Fault.Http (
    classifyTransport,
    httpFault,
    isRetryableStatusCode,
    displayFaultT,
) where

import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (..),
    Request (host, port),
    responseStatus,
 )
import Network.HTTP.Types.Status (statusCode)
import Network.TLS qualified as TLS

import Ecluse.Core.Fault (
    HttpFault (HttpStatus, HttpTransport),
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault,
    renderHttpFault,
    transportFault,
 )
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)
import Ecluse.Core.Text (displayExceptionT)

-- | Classify a client exception, recognising TLS failures by type rather than rendered text. The detail is the dialled @host:port@ and the client's own reason.
classifyTransport :: HttpException -> TransportFault
classifyTransport err = transportFault (causeOf err) (dialledAuthority err <> ": " <> clientReason err)
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

-- | The answered status, or else the transport fault 'classifyTransport' assigns.
httpFault :: HttpException -> HttpFault
httpFault = \case
    HttpExceptionRequest _ (StatusCodeException response _) -> HttpStatus (statusCode (responseStatus response))
    other -> HttpTransport (classifyTransport other)

-- | Whether an HTTP status signals a temporary failure: server errors, timeout, or throttling.
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode code = code >= 500 || code == 408 || code == 429

-- | 'displayExceptionT' for a log line, except that an 'HttpException' renders through 'renderHttpFault' at the @host:port@ it dialled.
displayFaultT :: SomeException -> Text
displayFaultT err = maybe (displayExceptionT err) renderDialled (fromException err)
  where
    renderDialled http = renderHttpFault (dialledAuthority http) (httpFault http)

-- A status carries the request a redirect reached, and a transport fault the request first sent.
dialledAuthority :: HttpException -> Text
dialledAuthority = \case
    HttpExceptionRequest request _ -> dialledAuthorityLabel (decodeUtf8 (host request) <> ":" <> show (port request))
    InvalidUrlException url _ -> dialledAuthorityLabel (toText url)

clientReason :: HttpException -> Text
clientReason = \case
    HttpExceptionRequest _ content -> contentReason content
    InvalidUrlException _ reason -> "InvalidUrlException: " <> toText reason

-- Every constructor by name, so a new one fails the build until judged. Byte strings (headers, status
-- lines, proxy values) are never rendered, and an inner exception renders without GHC's context.
contentReason :: HttpExceptionContent -> Text
contentReason = \case
    StatusCodeException response _ -> "StatusCodeException: HTTP " <> show (statusCode (responseStatus response))
    TooManyRedirects _ -> "TooManyRedirects"
    OverlongHeaders -> "OverlongHeaders"
    TooManyHeaderFields -> "TooManyHeaderFields"
    ResponseTimeout -> "ResponseTimeout"
    ConnectionTimeout -> "ConnectionTimeout"
    ConnectionFailure (SomeException inner) -> "ConnectionFailure: " <> displayExceptionT inner
    InvalidStatusLine _ -> "InvalidStatusLine"
    InvalidHeader _ -> "InvalidHeader"
    InvalidRequestHeader _ -> "InvalidRequestHeader"
    InternalException (SomeException inner) -> "InternalException: " <> displayExceptionT inner
    ProxyConnectException{} -> "ProxyConnectException"
    NoResponseDataReceived -> "NoResponseDataReceived"
    TlsNotSupported -> "TlsNotSupported"
    WrongRequestBodyStreamSize{} -> "WrongRequestBodyStreamSize"
    ResponseBodyTooShort{} -> "ResponseBodyTooShort"
    InvalidChunkHeaders -> "InvalidChunkHeaders"
    IncompleteHeaders -> "IncompleteHeaders"
    InvalidDestinationHost _ -> "InvalidDestinationHost"
    HttpZlibException _ -> "HttpZlibException"
    InvalidProxyEnvironmentVariable name _ -> "InvalidProxyEnvironmentVariable: " <> name
    ConnectionClosed -> "ConnectionClosed"
    InvalidProxySettings _ -> "InvalidProxySettings"
