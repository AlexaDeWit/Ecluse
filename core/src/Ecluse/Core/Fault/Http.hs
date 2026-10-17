-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared HTTP fault classification for registry, queue, and advisory adapters.
Client exceptions become "Ecluse.Core.Fault" values at the adapter boundary. Neither a value nor
its rendering carries the request or the response: a header, a query a redirect reached, or a
cookie can carry a credential. Every manager the roles build starts from 'typedTlsManagerSettings',
so a failed body read raises a client exception too.
-}
module Ecluse.Core.Fault.Http (
    classifyTransport,
    httpFault,
    isRetryableStatusCode,
    displayFaultT,
    hostPortLabel,

    -- * Managers
    typedBodyReads,
    typedTlsManagerSettings,
) where

import Data.List (lookup)
import Data.Text qualified as T
import GHC.IO.Exception (IOErrorType (ResourceVanished, TimeExpired))
import Network.Connection (HostCannotConnect, HostNotResolved)
import Network.HTTP.Client (
    BodyReader,
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (..),
    ManagerSettings (managerModifyResponse),
    Request (host, port),
    Response,
    getOriginalRequest,
    responseStatus,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.TLS qualified as TLS
import System.IO.Error (IOError, ioeGetErrorType)
import UnliftIO.Exception (catchJust, throwIO)

import Ecluse.Core.Fault (
    HttpFault (HttpStatus, HttpTransport),
    TransportCause (TransportProtocol, TransportTimeout, TransportTls, TransportUnreachable),
    TransportFault,
    renderHttpFault,
    transportFault,
 )
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)
import Ecluse.Core.Text (displayExceptionT)

-- | Classify a client exception by type, reading text only for the socket error tls keeps as text. The detail is the dialled @host:port@ and the client's own reason.
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
            InternalException inner -> internalCause inner
            _ -> TransportProtocol
        InvalidUrlException _ _ -> TransportProtocol

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

-- | 'displayExceptionT' for a log line, except that an 'HttpException' renders through 'renderHttpFault' at the @host:port@ it dialled.
displayFaultT :: SomeException -> Text
displayFaultT err = maybe (displayExceptionT err) renderDialled (fromException err)
  where
    renderDialled http = renderHttpFault (dialledAuthority http) (httpFault http)

-- | A dialled host and port as a log line names them, so an IPv6 host keeps its brackets.
hostPortLabel :: Text -> Int -> Text
hostPortLabel dialledHost dialledPort = dialledAuthorityLabel (dialledHost <> ":" <> show dialledPort)

-- | Raise a socket or TLS failure in a response body read as the 'HttpException' the client raises for one before the body.
typedBodyReads :: ManagerSettings -> ManagerSettings
typedBodyReads settings = settings{managerModifyResponse = fmap typedReads . managerModifyResponse settings}

-- | 'tlsManagerSettings' under 'typedBodyReads': every manager the roles build starts from this value.
typedTlsManagerSettings :: ManagerSettings
typedTlsManagerSettings = typedBodyReads tlsManagerSettings

-- http-client wraps an exception only until the response arrives, so a failure in the body reaches the reader bare.
typedReads :: Response BodyReader -> Response BodyReader
typedReads response = rewrap <$> response
  where
    rewrap bodyRead = catchJust bareFault bodyRead (throwIO . HttpExceptionRequest (getOriginalRequest response) . InternalException)

bareFault :: SomeException -> Maybe SomeException
bareFault err
    | isJust (fromException @IOError err) || isJust (fromException @TLS.TLSException err) = Just err
    | otherwise = Nothing

-- A status and a socket or TLS failure in a body read carry the request a redirect reached. Any other fault carries the request first sent.
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
