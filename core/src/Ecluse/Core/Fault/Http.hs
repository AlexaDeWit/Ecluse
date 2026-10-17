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
    hostPortLabel,
) where

import Data.List (lookup)
import Data.Text qualified as T
import GHC.IO.Exception (IOErrorType (ResourceVanished, TimeExpired))
import Network.Connection (HostCannotConnect, HostNotResolved)
import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (..),
    Request (host, port),
    responseStatus,
 )
import Network.HTTP.Client.Internal (unHttpExceptionContentWrapper)
import Network.HTTP.Types.Status (statusCode)
import Network.TLS qualified as TLS
import System.IO.Error (ioeGetErrorType)

import Ecluse.Core.Fault (
    HttpFault (HttpStatus, HttpTransport),
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault,
    renderHttpFault,
    renderTransportFault,
    transportFault,
 )
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)
import Ecluse.Core.Text (displayExceptionT)

-- | Classify a client exception by type, reading text only for the socket error tls keeps as text. The detail is the dialled @host:port@ and the client's own reason.
classifyTransport :: HttpException -> TransportFault
classifyTransport err = transportFault (causeOf err) (dialledAuthority err <> ": " <> clientReason err)
  where
    causeOf = \case
        HttpExceptionRequest _ content -> contentCause content
        InvalidUrlException _ _ -> TransportProtocol

contentCause :: HttpExceptionContent -> TransportCause
contentCause = \case
    ConnectionTimeout -> TransportTimeout
    ResponseTimeout -> TransportTimeout
    ConnectionFailure _ -> TransportUnreachable
    ConnectionClosed -> TransportUnreachable
    -- The peer hung up before the first response byte, so the request never
    -- reached a protocol exchange.
    NoResponseDataReceived -> TransportUnreachable
    InternalException inner -> internalCause inner
    _ -> TransportProtocol

-- The TLS manager wraps its connector's refused connection or empty address list here, where plain HTTP raises ConnectionFailure.
-- A failed socket call classes by its error type on either scheme, inside the TLS handshake or outside it.
internalCause :: SomeException -> TransportCause
internalCause inner
    | Just (TLS.HandshakeFailed reason) <- fromException inner, Just cause <- handshakeSocketCause reason = cause
    | Just (_ :: TLS.TLSException) <- fromException inner = TransportTls
    | Just (_ :: HostCannotConnect) <- fromException inner = TransportUnreachable
    | Just (_ :: HostNotResolved) <- fromException inner = TransportUnreachable
    | Just ioe <- fromException inner, Just cause <- lookup (ioeGetErrorType ioe) socketCauses = cause
    | otherwise = TransportProtocol

-- A peer that closes or resets is unreachable, as NoResponseDataReceived is. A timed-out socket call is the kernel giving up on an unanswered peer.
socketCauses :: [(IOErrorType, TransportCause)]
socketCauses = [(ResourceVanished, TransportUnreachable), (TimeExpired, TransportTimeout)]

-- tls keeps a socket's IOException only as its shown text, "location: type (reason)", so the error type is
-- recognised as base renders it between those delimiters.
handshakeSocketCause :: TLS.TLSError -> Maybe TransportCause
handshakeSocketCause = \case
    TLS.Error_EOF -> Just TransportUnreachable
    TLS.Error_Misc shown -> listToMaybe [cause | (errorType, cause) <- socketCauses, (": " <> show errorType <> " (") `T.isInfixOf` toText shown]
    _ -> Nothing

-- | The answered status, or else the transport fault 'classifyTransport' assigns.
httpFault :: HttpException -> HttpFault
httpFault = \case
    HttpExceptionRequest _ (StatusCodeException response _) -> HttpStatus (statusCode (responseStatus response))
    other -> HttpTransport (classifyTransport other)

-- | Whether an HTTP status signals a temporary failure: server errors, timeout, or throttling.
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode code = code >= 500 || code == 408 || code == 429

-- | An exception for a log line, without GHC's context. A client exception renders as its classified fault, naming the @host:port@ only when it had a request.
displayFaultT :: SomeException -> Text
displayFaultT err
    | Just http <- fromException err = renderHttpFault (dialledAuthority http) (httpFault http)
    -- http-client raises a fault outside a request, such as a malformed proxy variable when it builds a manager, without an HttpException around it.
    | Just content <- unHttpExceptionContentWrapper <$> fromException err = renderTransportFault (transportFault (contentCause content) (contentReason content))
    | otherwise = displayWithoutContext err

-- | A dialled host and port as a log line names them, so an IPv6 host keeps its brackets.
hostPortLabel :: Text -> Int -> Text
hostPortLabel dialledHost dialledPort = dialledAuthorityLabel (dialledHost <> ":" <> show dialledPort)

-- A status carries the request a redirect reached, and a transport fault the request first sent.
dialledAuthority :: HttpException -> Text
dialledAuthority = \case
    HttpExceptionRequest request _ -> hostPortLabel (decodeUtf8 (host request)) (port request)
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
    ConnectionFailure inner -> "ConnectionFailure: " <> displayWithoutContext inner
    InvalidStatusLine _ -> "InvalidStatusLine"
    InvalidHeader _ -> "InvalidHeader"
    InvalidRequestHeader _ -> "InvalidRequestHeader"
    InternalException inner -> "InternalException: " <> displayWithoutContext inner
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

-- GHC 9.10 appends the exception context, a backtrace, to a SomeException's display. The exception inside renders without it.
displayWithoutContext :: SomeException -> Text
displayWithoutContext (SomeException inner) = displayExceptionT inner
