-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ edge of the transport-fault vocabulary. An adapter folds the AWS error sum into
"Ecluse.Core.Fault" as a value, or raises it past the adapter as 'AwsRequestFailed'. A credential
failure that reaches no adapter, such as a background refresh, renders through 'displayAwsFaultT'.
No form holds the request or the response headers, and a service's message keeps at most its first
line.
-}
module Ecluse.Runtime.Aws.Fault (
    -- * Faults as values
    classifyAwsTransport,
    sendClassified,
    serviceStatus,

    -- * Faults raised past the adapter
    AwsRequestFailed (..),
    AwsParty (..),
    AwsFault (..),
    AwsAnswer (..),
    sendRaising,
    retypeAwsFailure,

    -- * Failures that reach no adapter
    AwsCredentialFault (..),
    retypeAmazonkaEscapes,
    displayAwsFaultT,
) where

import Amazonka qualified as AWS
import Amazonka.Auth.Exception (AuthError (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import Lens.Micro ((^.))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (handle, handleJust, throwIO)

import Ecluse.Core.Fault (HttpFault (HttpStatus, HttpTransport), TransportCause (TransportProtocol), TransportFault, renderHttpFault, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport, displayFaultT, hostPortLabel, httpFault)

-- | Classify an @amazonka@ error into the core transport vocabulary. A service's answer is detailed as 'AwsAnswer' renders it.
classifyAwsTransport :: AWS.Error -> TransportFault
classifyAwsTransport = \case
    AWS.TransportError httpErr -> classifyTransport httpErr
    AWS.ServiceError service -> transportFault TransportProtocol (renderAnswer (serviceName service) (serviceAnswer service))
    AWS.SerializeError failure -> transportFault TransportProtocol (renderUndecodable (failure ^. AWS.serializeError_abbrev . AWS._Abbrev) failure)

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

-- | A failed @amazonka@ request raised past its adapter. It renders without the request: http-client redacts only @Authorization@, so a session-token header would print.
data AwsRequestFailed = AwsRequestFailed
    { arfParty :: AwsParty
    , arfFault :: AwsFault
    }
    deriving stock (Eq, Show)

instance Exception AwsRequestFailed where
    displayException failed = toString ("AWS request failed at " <> renderFailure failed)

-- | Where a failed request went.
data AwsParty
    = -- | The host and port it was sent to.
      AwsEndpointAt Text Int
    | -- | The service that answered, named when no request is in reach.
      AwsServiceNamed Text
    deriving stock (Eq, Show)

-- | Why a request failed.
data AwsFault
    = -- | The service refused it.
      AwsStatus AwsAnswer
    | -- | The HTTP exchange failed: a status from an endpoint outside the AWS protocol, or a transport fault.
      AwsHttp HttpFault
    deriving stock (Eq, Show)

-- | A service's refusal without its response headers. A signature error's message can quote the signed request, so it is dropped.
data AwsAnswer = AwsAnswer
    { aaStatus :: Int
    , aaCode :: Text
    , aaRequestId :: Maybe Text
    , aaMessage :: Maybe Text
    -- ^ The first line of any other message, which names an IAM action and resource.
    }
    deriving stock (Eq, Show)

-- | Send a request, raising its failure as 'AwsRequestFailed'. A service's answer is named by the endpoint the env sent the request to.
sendRaising :: (AWS.AWSRequest a) => AWS.Env -> a -> IO (AWS.AWSResponse a)
sendRaising env request = sendClassified (raised (const (requestEndpoint env request))) env request >>= either throwIO pure

-- | Raise the @amazonka@ or http-client failure of an action whose request the caller never holds, such as credential discovery, as a typed fault.
retypeAwsFailure :: IO a -> IO a
retypeAwsFailure = handle (throwIO . authFault) . handle (throwIO . raised AwsServiceNamed) . handle (throwIO . raisedHttp)

-- | Why @amazonka@ found no usable credentials when no request failed. It keeps the first line of amazonka's reason, never a file's contents.
data AwsCredentialFault
    = -- | A setting a provider reads is missing: an environment variable or a file.
      AwsCredentialMissing Text
    | -- | A credentials file or document could not be used.
      AwsCredentialUnusable Text
    | -- | No provider in the chain found credentials.
      AwsCredentialsExhausted
    deriving stock (Eq, Show)

instance Exception AwsCredentialFault where
    displayException =
        toString . \case
            AwsCredentialMissing reason -> "AWS credentials missing: " <> reason
            AwsCredentialUnusable reason -> "AWS credentials unusable: " <> reason
            AwsCredentialsExhausted -> "AWS credential discovery found no credentials"

-- | Rethrow an @amazonka@ error or credential failure escaping the action as its typed fault. Any other exception passes unchanged.
retypeAmazonkaEscapes :: (MonadUnliftIO m) => m a -> m a
retypeAmazonkaEscapes = handleJust amazonkaFault throwIO

-- | 'displayFaultT', with an @amazonka@ error or credential failure rendered through its typed fault.
displayAwsFaultT :: SomeException -> Text
displayAwsFaultT err = displayFaultT (fromMaybe err (amazonkaFault err))

-- amazonka's background refresh hands a failure to the thread that discovered the credentials, wrapped in 'AuthError'.
amazonkaFault :: SomeException -> Maybe SomeException
amazonkaFault err
    | Just awsErr <- fromException err = Just (raised AwsServiceNamed awsErr)
    | Just authErr <- fromException err = Just (authFault authErr)
    | otherwise = Nothing

authFault :: AuthError -> SomeException
authFault = \case
    RetrievalError httpErr -> raisedHttp httpErr
    AuthServiceError service -> raised AwsServiceNamed (AWS.ServiceError service)
    OtherAuthError inner -> fromMaybe inner (amazonkaFault inner)
    MissingEnvError reason -> toException (AwsCredentialMissing (firstLineOf reason))
    MissingFileError path -> toException (AwsCredentialMissing (toText path))
    InvalidFileError reason -> toException (AwsCredentialUnusable (firstLineOf reason))
    InvalidIAMError reason -> toException (AwsCredentialUnusable (firstLineOf reason))
    CredentialChainExhausted -> toException AwsCredentialsExhausted
  where
    firstLineOf = fromMaybe "" . firstLine

-- A service's answer is named by @answering@, given the service's name.
raised :: (Text -> AwsParty) -> AWS.Error -> SomeException
raised answering = \case
    AWS.TransportError httpErr -> raisedHttp httpErr
    AWS.ServiceError service -> toException (AwsRequestFailed (answering (serviceName service)) (AwsStatus (serviceAnswer service)))
    AWS.SerializeError failure ->
        let party = answering (failure ^. AWS.serializeError_abbrev . AWS._Abbrev)
         in toException (AwsRequestFailed party (AwsHttp (HttpTransport (transportFault TransportProtocol (renderUndecodable (renderParty party) failure)))))

-- An unparseable URL leaves as the client's own exception, which "Ecluse.Core.Fault.Http" renders by its authority.
raisedHttp :: HTTP.HttpException -> SomeException
raisedHttp = \case
    err@(HTTP.HttpExceptionRequest request _) -> toException (AwsRequestFailed (AwsEndpointAt (decodeUtf8 (HTTP.host request)) (HTTP.port request)) (AwsHttp (httpFault err)))
    err -> toException err

serviceAnswer :: AWS.ServiceError -> AwsAnswer
serviceAnswer service =
    AwsAnswer
        { aaStatus = statusCode (service ^. AWS.serviceError_status)
        , aaCode = codeText
        , aaRequestId = AWS.fromRequestId <$> service ^. AWS.serviceError_requestId
        , aaMessage = if code `elem` signatureCodes then Nothing else firstLine . AWS.fromErrorMessage =<< service ^. AWS.serviceError_message
        }
  where
    code = service ^. AWS.serviceError_code
    codeText = case code of
        AWS.ErrorCode text -> text

-- The codes whose message AWS documents as quoting the canonical request, spelled as AWS sends them.
signatureCodes :: [AWS.ErrorCode]
signatureCodes = map AWS.newErrorCode ["InvalidSignatureException", "SignatureDoesNotMatch", "IncompleteSignature"]

firstLine :: Text -> Maybe Text
firstLine message = case T.takeWhile (`notElem` ['\n', '\r']) message of
    "" -> Nothing
    line -> Just line

serviceName :: AWS.ServiceError -> Text
serviceName service = service ^. AWS.serviceError_abbrev . AWS._Abbrev

renderFailure :: AwsRequestFailed -> Text
renderFailure (AwsRequestFailed party fault) = case fault of
    AwsStatus answer -> renderAnswer (renderParty party) answer
    AwsHttp http -> renderHttpFault (renderParty party) http

renderParty :: AwsParty -> Text
renderParty = \case
    AwsEndpointAt endpointHost endpointPort -> hostPortLabel endpointHost endpointPort
    AwsServiceNamed service -> service

renderAnswer :: Text -> AwsAnswer -> Text
renderAnswer source answer =
    renderHttpFault source (HttpStatus (aaStatus answer))
        <> (" " <> aaCode answer)
        <> foldMap (\requestId -> " (request " <> requestId <> ")") (aaRequestId answer)
        <> foldMap (": " <>) (aaMessage answer)

-- A decoder's message can quote the body it read, so only the status is rendered.
renderUndecodable :: Text -> AWS.SerializeError -> Text
renderUndecodable source failure =
    renderHttpFault source (HttpStatus (statusCode (failure ^. AWS.serializeError_status))) <> ", an answer that could not be decoded"

-- The @host:port@ an env sends a request to, by the service resolution @amazonka@ applies.
requestEndpoint :: (AWS.AWSRequest a) => AWS.Env -> a -> AwsParty
requestEndpoint env request = AwsEndpointAt (decodeUtf8 (endpoint ^. AWS.endpoint_host)) (endpoint ^. AWS.endpoint_port)
  where
    service = AWS.request (AWS.overrides env) request ^. AWS.request_service
    endpoint = (service ^. AWS.service_endpoint) (AWS.region env)
