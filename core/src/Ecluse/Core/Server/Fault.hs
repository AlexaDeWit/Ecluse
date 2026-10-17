-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The request perimeter's fault vocabulary: what the serve boundary says about an exception
that escaped a handler before the response was committed. The pipeline reports every routine
failure as a value, so an escape is either the response-assembly leg's confined 'RenderEscape'
marker or an invariant break nothing classified. Nothing here reaches the client, which gets the
route's contract-admitted neutral 500.
-}
module Ecluse.Core.Server.Fault (
    RequestFault (..),
    classifyEscape,
    RenderEscape (..),
) where

import Ecluse.Core.Fault (boundedDetail)
import Ecluse.Core.Fault.Http (displayFaultT)
import Ecluse.Core.Telemetry.Metrics (RequestFaultCause (RenderFault, UnclassifiedFault))

-- | One classified perimeter fault: the bounded cause the @ecluse.serve.perimeter.faults@ metric records, and the rendered escape for the log line.
data RequestFault = RequestFault
    { rqCause :: RequestFaultCause
    -- ^ The closed classification (the metric label vocabulary).
    , rqDetail :: Text
    -- ^ The rendered escape, bounded to the shared log-line budget. Diagnostic only: never parsed, and no decision branches on it.
    }
    deriving stock (Eq, Show)

-- | The confined marker for an exception that escaped the response-assembly render, which is total by contract. The perimeter folds it to 'RenderFault'.
newtype RenderEscape = RenderEscape SomeException
    deriving stock (Show)

instance Exception RenderEscape

-- | Fold an escaped exception into the perimeter's vocabulary, 'UnclassifiedFault' by default.
classifyEscape :: SomeException -> RequestFault
classifyEscape escape
    | Just (RenderEscape inner) <- fromException escape = fault RenderFault inner
    | otherwise = fault UnclassifiedFault escape
  where
    fault cause rendered = RequestFault cause (boundedDetail (displayFaultT rendered))
