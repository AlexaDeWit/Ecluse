-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ edge of the transport-fault vocabulary: fold the AWS error
sum into "Ecluse.Core.Fault" at an adapter boundary.

Both AWS adapters -- the SQS mirror queue ("Ecluse.Runtime.Queue.Sqs") and the
advisory sync's S3 transport ("Ecluse.Runtime.Cve.Sync") -- face the same
'Amazonka.Error', so the classification lives once, here. A genuine transport
failure arrives wrapped in @amazonka@'s transport channel as the same
@http-client@ exception every other adapter sees, and is classified by the
shared 'Ecluse.Core.Fault.Http.classifyTransport'; a service-level refusal (a
throttle, an access denial, a serialisation surprise) is 'TransportProtocol'
with the rendered error as detail -- the wire worked, the service said no.
-}
module Ecluse.Runtime.Aws.Fault (
    classifyAwsTransport,
) where

import Amazonka qualified as AWS

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Text (displayExceptionT)

{- | Classify an @amazonka@ error into the core transport vocabulary: the
transport channel through the shared @http-client@ classification, everything
else (service and serialisation errors) as 'TransportProtocol' with the
rendered detail carried for the log line.
-}
classifyAwsTransport :: AWS.Error -> TransportFault
classifyAwsTransport = \case
    AWS.TransportError httpErr -> classifyTransport httpErr
    err -> transportFault TransportProtocol (displayExceptionT err)
