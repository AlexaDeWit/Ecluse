-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ edge of the transport-fault vocabulary: fold the AWS error sum into
"Ecluse.Core.Fault" at an adapter boundary.

Three adapters face the same 'Amazonka.Error', so the classification lives once, here: the SQS
mirror queue, the advisory sync's S3 transport, and the CodeArtifact store maintenance leaf. A
service-level refusal (a throttle, an access denial, a serialisation surprise) is
'TransportProtocol' with the rendered error as detail: the wire worked, the service said no.
-}
module Ecluse.Runtime.Aws.Fault (
    classifyAwsTransport,
    sendClassified,
) where

import Amazonka qualified as AWS
import Control.Monad.Trans.Resource (runResourceT)

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Text (displayExceptionT)

-- | Classify an @amazonka@ error into the core transport vocabulary.
classifyAwsTransport :: AWS.Error -> TransportFault
classifyAwsTransport = \case
    AWS.TransportError httpErr -> classifyTransport httpErr
    err -> transportFault TransportProtocol (displayExceptionT err)

{- | Send a request with the AWS error kept out of the exception channel and folded into the
caller's own fault type, which is how every adapter here reports a failure as a value.
-}
sendClassified ::
    (AWS.AWSRequest a) =>
    (AWS.Error -> e) ->
    AWS.Env ->
    a ->
    IO (Either e (AWS.AWSResponse a))
sendClassified classify env = fmap (first classify) . runResourceT . AWS.sendEither env
