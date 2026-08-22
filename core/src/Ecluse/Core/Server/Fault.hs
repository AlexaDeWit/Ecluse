-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The request perimeter's fault vocabulary: what the serve boundary says about
an exception that escaped a handler before the response was committed.

The serve pipeline reports every routine failure as a value: metadata errors, fetch
faults, and rule decisions. An exception reaching the perimeter is therefore one of two
things. It is one of the few __recognised typed channels__, a response-bound breach or
the response-assembly leg's confined 'RenderEscape' marker. Otherwise it is an invariant
break nothing classified.

'classifyEscape' folds whichever it is into a 'RequestFault'. The bounded cause feeds
the @ecluse.serve.perimeter.faults@ metric, and the rendered detail feeds the
perimeter's log line. Neither ever reaches the client: the response is the route's
contract-admitted neutral 500.
-}
module Ecluse.Core.Server.Fault (
    RequestFault (..),
    classifyEscape,
    RenderEscape (..),
) where

import Ecluse.Core.Fault (boundedDetail)
import Ecluse.Core.Registry.Fault (ResponseBoundExceeded)
import Ecluse.Core.Telemetry.Metrics (RequestFaultCause (GateFault, RenderFault, UnclassifiedFault))
import Ecluse.Core.Text (displayExceptionT)

{- | One classified perimeter fault: the bounded cause a metric records and the rendered escape
for the log line. The detail is diagnostic only, never parsed, and no decision may branch on it.
-}
data RequestFault = RequestFault
    { rqCause :: RequestFaultCause
    -- ^ The closed classification (the metric label vocabulary).
    , rqDetail :: Text
    -- ^ The rendered escape, bounded to the shared log-line budget.
    }
    deriving stock (Eq, Show)

{- | The confined marker wrapping an exception that escaped the response-assembly render, which is
total by contract. It never crosses the perimeter, which folds it to 'RenderFault' and answers the
neutral 500.
-}
newtype RenderEscape = RenderEscape SomeException
    deriving stock (Show)

instance Exception RenderEscape

{- | Fold an escaped exception into the perimeter's vocabulary. A recognised typed channel
classifies by type. Everything else is 'UnclassifiedFault'.
-}
classifyEscape :: SomeException -> RequestFault
classifyEscape escape
    | Just (_ :: ResponseBoundExceeded) <- fromException escape = fault GateFault escape
    | Just (RenderEscape inner) <- fromException escape = fault RenderFault inner
    | otherwise = fault UnclassifiedFault escape
  where
    fault cause rendered = RequestFault cause (boundedDetail (displayExceptionT rendered))
