-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Core transport-fault vocabulary shared by client-library adapters.
Adapters classify exceptions at their edge, so the consumer layers use this closed value.
'tfDetail' gives diagnostic text for logs and is never parsed.
-}
module Ecluse.Core.Fault (
    -- * Transport faults
    TransportFault (..),
    transportFault,
    TransportCause (..),
    renderTransportCause,
    transportRetryable,

    -- * Retry delays
    RetryAfter (..),

    -- * The shared detail budget
    boundedDetail,
) where

import Data.Text qualified as T

{- | One classified transport failure. Build it with 'transportFault' so the detail stays
bounded.
-}
data TransportFault = TransportFault
    { tfCause :: TransportCause
    -- ^ The closed classification a consumer or an operator reads.
    , tfDetail :: Text
    {- ^ The client library's rendered detail, bounded to a log-line-sized budget.
    Diagnostic text only: it is never parsed, and no decision may branch on it.
    -}
    }
    deriving stock (Eq, Show)

{- | Why the transport could not deliver. Coarse on purpose: each constructor is a
distinction an operator reads differently, and anything finer belongs in 'tfDetail'.
-}
data TransportCause
    = -- | The peer did not answer in time (a connect or response timeout).
      TransportTimeout
    | {- | The peer could not be reached at all: a refused or reset connection, or a
      name that did not resolve.
      -}
      TransportUnreachable
    | -- | The TLS layer refused the peer (a handshake or certificate failure).
      TransportTls
    | {- | Any other client-reported fault (a malformed response, an unparseable
      URL, an internal client error): the closed catch-all, so the sum stays total
      over whatever a client library reports.
      -}
      TransportProtocol
    deriving stock (Eq, Show)

{- | Is a fault with this cause worth another attempt? A timeout and an unreachable peer
can clear on their own. A TLS refusal and a protocol fault need an operator or a fix.
-}
transportRetryable :: TransportCause -> Bool
transportRetryable = \case
    TransportTimeout -> True
    TransportUnreachable -> True
    TransportTls -> False
    TransportProtocol -> False

-- | What a transport cause says happened, for a line an operator reads.
renderTransportCause :: TransportCause -> Text
renderTransportCause = \case
    TransportTimeout -> "the peer did not answer in time"
    TransportUnreachable -> "the peer could not be reached"
    TransportTls -> "the TLS layer refused the peer"
    TransportProtocol -> "the peer's answer could not be used"

{- | A @Retry-After@ delay, in whole seconds. A 'newtype' so a raw count of seconds is
never confused with some other integer when it reaches a response header or a sweep's wait.
-}
newtype RetryAfter = RetryAfter Int
    deriving stock (Eq, Ord, Show)

{- | Build a 'TransportFault' with the detail truncated to the log-line budget, so a
pathological rendered exception cannot bloat a log line or a held error value.
-}
transportFault :: TransportCause -> Text -> TransportFault
transportFault cause detail = TransportFault cause (boundedDetail detail)

{- | Truncate a rendered detail to the shared log-line budget. Every fault vocabulary that
carries diagnostic text bounds it identically.
-}
boundedDetail :: Text -> Text
boundedDetail = T.copy . T.take maxDetailChars

-- The rendered-detail budget: generous enough for any realistic client-library
-- message, small enough that a held fault value stays log-line sized.
maxDetailChars :: Int
maxDetailChars = 512
