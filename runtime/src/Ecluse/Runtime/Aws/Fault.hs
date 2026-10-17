-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ edge of the transport-fault vocabulary: fold the AWS error sum into
"Ecluse.Core.Fault" at an adapter boundary. A service-level refusal (a throttle, an access
denial, a serialisation surprise) is 'TransportProtocol' with the rendered error as detail: the
wire worked, the service said no.
-}
module Ecluse.Runtime.Aws.Fault (
    -- * Faults as values
    classifyAwsTransport,
    sendClassified,
    serviceStatus,

    -- * Faults raised past the adapter
    AwsRequestFailed (..),
    AwsFault (..),
    sendRaising,
    retypeAwsFailure,
) where

import Amazonka qualified as AWS
import Control.Monad.Trans.Resource (runResourceT)
import Lens.Micro ((^.))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import UnliftIO.Exception (handle, throwIO)

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault (tfCause), renderTransportCause, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Security.Authority (dialledAuthorityLabel)
import Ecluse.Core.Text (displayExceptionT)

-- | Classify an @amazonka@ error into the core transport vocabulary.
classifyAwsTransport :: AWS.Error -> TransportFault
classifyAwsTransport = \case
    AWS.TransportError httpErr -> classifyTransport httpErr
    err -> transportFault TransportProtocol (displayExceptionT err)

-- | Send a request with the AWS error folded into the caller's own fault type, a value rather than an exception.
sendClassified ::
    (AWS.AWSRequest a) =>
    (AWS.Error -> e) ->
    AWS.Env ->
    a ->
    IO (Either e (AWS.AWSResponse a))
sendClassified classify env = fmap (first classify) . runResourceT . AWS.sendEither env

-- | The HTTP status a service answered with, or 'Nothing' when the request never got an answer.
serviceStatus :: AWS.Error -> Maybe Int
serviceStatus = \case
    AWS.ServiceError err -> Just (statusCode (err ^. AWS.serviceError_status))
    _ -> Nothing

-- | A failed @amazonka@ request that renders without the request: http-client redacts only @Authorization@, so a session-token header would print.
data AwsRequestFailed = AwsRequestFailed
    { arfParty :: Text
    -- ^ The @host:port@ the request went to, or the answering service when no endpoint is in reach.
    , arfFault :: AwsFault
    }
    deriving stock (Eq, Show)

instance Exception AwsRequestFailed where
    displayException failed = toString ("AWS request failed at " <> arfParty failed <> ": " <> renderAwsFault (arfFault failed))

-- | Why a request failed, without the request. The service's message is left out too, as a signature error's message can quote the signed headers.
data AwsFault
    = -- | The service answered with this HTTP status and error code.
      AwsStatus Int Text
    | -- | No usable answer came back, for the cause 'classifyAwsTransport' assigns.
      AwsTransport TransportCause
    deriving stock (Eq, Show)

-- | Send a request, raising its failure as 'AwsRequestFailed' at the endpoint the env sends it to.
sendRaising :: (AWS.AWSRequest a) => AWS.Env -> a -> IO (AWS.AWSResponse a)
sendRaising env request = sendClassified failedAtEndpoint env request >>= either throwIO pure
  where
    failedAtEndpoint err = (awsRequestFailed err){arfParty = requestEndpoint env request}

-- | Raise the @amazonka@ or http-client failure of an action whose request the caller never holds, such as credential discovery, as 'AwsRequestFailed'.
retypeAwsFailure :: IO a -> IO a
retypeAwsFailure = handle (throwIO . awsRequestFailed) . handle (throwIO . awsRequestFailed . AWS.TransportError)

-- A transport failure names the host it dialled, and a service's answer names the service.
awsRequestFailed :: AWS.Error -> AwsRequestFailed
awsRequestFailed err = case err of
    AWS.TransportError httpErr -> AwsRequestFailed (dialledAuthority httpErr) transport
    AWS.ServiceError service -> AwsRequestFailed (service ^. AWS.serviceError_abbrev . AWS._Abbrev) (refusal service)
    AWS.SerializeError failure -> AwsRequestFailed (failure ^. AWS.serializeError_abbrev . AWS._Abbrev) transport
  where
    transport = AwsTransport (tfCause (classifyAwsTransport err))
    refusal service = case service ^. AWS.serviceError_code of
        AWS.ErrorCode code -> AwsStatus (statusCode (service ^. AWS.serviceError_status)) code

renderAwsFault :: AwsFault -> Text
renderAwsFault = \case
    AwsStatus status code -> "HTTP " <> show status <> " " <> code
    AwsTransport cause -> renderTransportCause cause

-- The @host:port@ an env sends a request to, by the service resolution @amazonka@ applies.
requestEndpoint :: (AWS.AWSRequest a) => AWS.Env -> a -> Text
requestEndpoint env request = decodeUtf8 (endpoint ^. AWS.endpoint_host) <> ":" <> show (endpoint ^. AWS.endpoint_port)
  where
    service = AWS.request (AWS.overrides env) request ^. AWS.request_service
    endpoint = (service ^. AWS.service_endpoint) (AWS.region env)

-- An unparseable URL is cut to its authority, as it can carry a credential.
dialledAuthority :: HTTP.HttpException -> Text
dialledAuthority = \case
    HTTP.HttpExceptionRequest request _ -> decodeUtf8 (HTTP.host request) <> ":" <> show (HTTP.port request)
    HTTP.InvalidUrlException url _ -> dialledAuthorityLabel (toText url)
