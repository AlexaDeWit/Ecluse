{- | The @http-client@ edge of the transport-fault vocabulary: fold the library's
exception type into "Ecluse.Core.Fault" at an adapter boundary.

Every HTTP-speaking adapter faces the same 'Network.HTTP.Client.HttpException' --
the npm registry client directly, and the AWS adapters through @amazonka@'s
transport-error channel -- so the classification lives once, here, rather than
per adapter. The module sits beside "Ecluse.Core.Fault" as a leaf: it imports
the client library, never any capability module, so the queue, the registry, and
the advisory sync can all reach it without crossing one another.
-}
module Ecluse.Core.Fault.Http (
    classifyTransport,
) where

import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (
        ConnectionClosed,
        ConnectionFailure,
        ConnectionTimeout,
        InternalException,
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

{- | Classify an @http-client@ exception into the core transport vocabulary
("Ecluse.Core.Fault"), at the one edge where the library's exception type is in
scope. Coarse by design: the 'TransportCause' is what a consumer or an operator
branches on, and the rendered exception rides along as the bounded detail. A TLS
refusal is recognised by the typed @tls@ exception @http-client@ wraps in its
internal-exception channel, never by matching rendered text.
-}
classifyTransport :: HttpException -> TransportFault
classifyTransport err = transportFault (causeOf err) (displayExceptionT err)
  where
    causeOf = \case
        HttpExceptionRequest _ content -> case content of
            ConnectionTimeout -> TransportTimeout
            ResponseTimeout -> TransportTimeout
            ConnectionFailure _ -> TransportUnreachable
            ConnectionClosed -> TransportUnreachable
            InternalException inner
                | Just (_ :: TLS.TLSException) <- fromException inner -> TransportTls
                | otherwise -> TransportProtocol
            _ -> TransportProtocol
        InvalidUrlException _ _ -> TransportProtocol
