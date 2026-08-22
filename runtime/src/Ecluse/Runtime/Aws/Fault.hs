-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ edge of the transport-fault vocabulary: fold the AWS error
sum into "Ecluse.Core.Fault" at an adapter boundary.

Both AWS adapters face the same 'Amazonka.Error', so the classification lives
once, here: the SQS mirror queue ("Ecluse.Runtime.Queue.Sqs") and the advisory
sync's S3 transport ("Ecluse.Runtime.Cve.Sync"). A genuine transport failure
arrives in @amazonka@'s transport channel as the same @http-client@ exception
every other adapter sees, and the shared
'Ecluse.Core.Fault.Http.classifyTransport' classifies it. A service-level refusal
(a throttle, an access denial, a serialisation surprise) is 'TransportProtocol'
with the rendered error as detail. The wire worked, the service said no.
-}
module Ecluse.Runtime.Aws.Fault (
    classifyAwsTransport,
) where

import Amazonka qualified as AWS

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, transportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Text (displayExceptionT)

{- | Classify an @amazonka@ error into the core transport vocabulary. The
transport channel goes through the shared @http-client@ classification.
Everything else (service and serialisation errors) becomes 'TransportProtocol',
carrying the rendered detail for the log line.
-}
classifyAwsTransport :: AWS.Error -> TransportFault
classifyAwsTransport = \case
    AWS.TransportError httpErr -> classifyTransport httpErr
    err -> transportFault TransportProtocol (displayExceptionT err)
