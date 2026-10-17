-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared HTTP fault classification for registry, queue, and advisory adapters.
Client exceptions become "Ecluse.Core.Fault" values at the adapter boundary. Rendered text names
the dialled @host:port@ and never the request or the response: a header, a query a redirect
reached, or a cookie can carry a credential.
-}
module Ecluse.Core.Fault.Http (
    -- * Classifying a client exception
    classifyTransport,
    isRetryableStatusCode,
    HttpFault (..),
    httpFault,

    -- * Rendering for a log line
    renderHttpFault,
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
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault,
    renderTransportCause,
    transportFault,
 )
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)
import Ecluse.Core.Text (displayExceptionT)

-- | Classify a client exception, recognising TLS failures by type rather than rendered text.
classifyTransport :: HttpException -> TransportFault
classifyTransport err = transportFault (transportCauseOf err) (dialledAuthority err <> ": " <> clientDetail err)

-- | Whether an HTTP status signals a temporary failure: server errors, timeout, or throttling.
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode code = code >= 500 || code == 408 || code == 429

-- | What an HTTP client failure says once its request and its response are dropped.
data HttpFault
    = -- | The peer answered with this non-2xx status.
      HttpStatus Int
    | -- | The transport delivered no usable answer.
      HttpTransport TransportCause
    deriving stock (Eq, Show)

-- | The answered status, or else the transport cause 'classifyTransport' assigns.
httpFault :: HttpException -> HttpFault
httpFault = \case
    HttpExceptionRequest _ (StatusCodeException response _) -> HttpStatus (statusCode (responseStatus response))
    other -> HttpTransport (transportCauseOf other)

-- | A fault for a log line, after the @host:port@ that names its source.
renderHttpFault :: Text -> HttpFault -> Text
renderHttpFault authority = \case
    HttpStatus code -> authority <> ": HTTP " <> show code
    HttpTransport cause -> authority <> ": " <> renderTransportCause cause

-- | 'displayExceptionT' for a log line, except that an 'HttpException' renders through 'renderHttpFault'.
displayFaultT :: SomeException -> Text
displayFaultT err = maybe (displayExceptionT err) renderHttpException (fromException err)

renderHttpException :: HttpException -> Text
renderHttpException err = renderHttpFault (dialledAuthority err) (httpFault err)

transportCauseOf :: HttpException -> TransportCause
transportCauseOf = \case
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

-- The request's host and port, which a redirect may have moved off the configured feed.
dialledAuthority :: HttpException -> Text
dialledAuthority = \case
    HttpExceptionRequest request _ -> dialledAuthorityLabel (decodeUtf8 (host request) <> ":" <> show (port request))
    InvalidUrlException url _ -> dialledAuthorityLabel (toText url)

clientDetail :: HttpException -> Text
clientDetail = \case
    HttpExceptionRequest _ content -> contentDetail content
    InvalidUrlException _ reason -> "InvalidUrlException: " <> toText reason

-- Every constructor by name, so a new one fails the build until it is judged. A byte string here
-- is a header, a status line, or a proxy value, and is never rendered.
contentDetail :: HttpExceptionContent -> Text
contentDetail = \case
    StatusCodeException response _ -> "StatusCodeException: HTTP " <> show (statusCode (responseStatus response))
    TooManyRedirects _ -> "TooManyRedirects"
    OverlongHeaders -> "OverlongHeaders"
    TooManyHeaderFields -> "TooManyHeaderFields"
    ResponseTimeout -> "ResponseTimeout"
    ConnectionTimeout -> "ConnectionTimeout"
    ConnectionFailure inner -> "ConnectionFailure: " <> displayExceptionT inner
    InvalidStatusLine _ -> "InvalidStatusLine"
    InvalidHeader _ -> "InvalidHeader"
    InvalidRequestHeader _ -> "InvalidRequestHeader"
    InternalException inner -> "InternalException: " <> displayExceptionT inner
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
