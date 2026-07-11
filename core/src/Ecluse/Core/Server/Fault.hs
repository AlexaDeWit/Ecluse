{- | The request perimeter's fault vocabulary: what the serve boundary says about
an exception that escaped a handler before the response was committed.

The serve pipeline reports every routine failure as a value (metadata errors,
fetch faults, rule decisions), so an exception reaching the perimeter is either
one of the few __recognised typed channels__ -- a wiring fault like
'RegistryUnconfigured', a response-bound breach, the response-assembly leg's
confined 'RenderEscape' marker -- or an invariant break nothing classified.
'classifyEscape' folds whichever it is into a 'RequestFault': the bounded cause
feeds the @ecluse.serve.perimeter.faults@ metric, the rendered detail feeds the
perimeter's log line, and neither ever reaches the client (the response is the
mount-shaped neutral 500).
-}
module Ecluse.Core.Server.Fault (
    RequestFault (..),
    classifyEscape,
    RenderEscape (..),
) where

import Ecluse.Core.Fault (boundedDetail)
import Ecluse.Core.Registry (RegistryUnconfigured)
import Ecluse.Core.Registry.Npm (ResponseBoundExceeded)
import Ecluse.Core.Telemetry.Metrics (RequestFaultCause (GateFault, RenderFault, UnclassifiedFault))
import Ecluse.Core.Text (displayExceptionT)

{- | One classified perimeter fault: the bounded cause a metric records and an
operator triages by, and the rendered escape for the log line. Diagnostic text
only -- it is never parsed, and no decision may branch on it.
-}
data RequestFault = RequestFault
    { rqCause :: RequestFaultCause
    -- ^ The closed classification (the metric label vocabulary).
    , rqDetail :: Text
    -- ^ The rendered escape, bounded to the shared log-line budget.
    }
    deriving stock (Eq, Show)

{- | The response-assembly leg's escape marker: the assembled-representation
render is total by contract (a pure assembly over already-validated inputs), so
an exception escaping it is an invariant break -- wrapped in this __confined__
typed marker at the one place the render runs, so the perimeter can name the
leg it escaped from. It never crosses the perimeter (which classifies it as
'RenderFault' and answers the neutral 500).
-}
newtype RenderEscape = RenderEscape SomeException
    deriving stock (Show)

instance Exception RenderEscape

{- | Fold an escaped exception into the perimeter's vocabulary: the recognised
typed channels classify by type, everything else is 'UnclassifiedFault' with its
rendering carried for the log line.
-}
classifyEscape :: SomeException -> RequestFault
classifyEscape escape
    | Just (_ :: RegistryUnconfigured) <- fromException escape = fault GateFault escape
    | Just (_ :: ResponseBoundExceeded) <- fromException escape = fault GateFault escape
    | Just (RenderEscape inner) <- fromException escape = fault RenderFault inner
    | otherwise = fault UnclassifiedFault escape
  where
    fault cause rendered = RequestFault cause (boundedDetail (displayExceptionT rendered))
