-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The core transport-fault vocabulary. A client-library adapter classifies its own
exceptions at its edge, so no consumer layer ever sees the library's exception type.
-}
module Ecluse.Core.Fault (
    -- * Transport faults
    TransportFault (..),
    transportFault,
    TransportCause (..),
    renderTransportCause,
    renderTransportFault,
    transportRetryable,

    -- * HTTP faults
    HttpFault (..),
    renderHttpFault,

    -- * Retry delays
    RetryAfter (..),

    -- * The shared detail budget
    boundedDetail,
) where

import Data.Text qualified as T

-- | One classified transport failure. Build it with 'transportFault' so the detail stays bounded.
data TransportFault = TransportFault
    { tfCause :: TransportCause
    -- ^ The closed classification a consumer or an operator reads.
    , tfDetail :: Text
    -- ^ What the adapter knows beyond the cause, bounded to a log line. Diagnostic only: never parsed, and no decision branches on it.
    }
    deriving stock (Eq, Show)

-- | Build a 'TransportFault' with the detail truncated, so a pathological exception cannot bloat a log line or a held value.
transportFault :: TransportCause -> Text -> TransportFault
transportFault cause detail = TransportFault cause (boundedDetail detail)

-- | Why the transport could not deliver. Coarse on purpose: each constructor reads differently to an operator, and the rest goes in 'tfDetail'.
data TransportCause
    = -- | The peer did not answer in time (a connect, response, or socket timeout).
      TransportTimeout
    | -- | The peer could not be reached at all: a refused connection, a failed lookup, or a peer that closed or reset before answering.
      TransportUnreachable
    | -- | The TLS layer refused the peer (a handshake or certificate failure).
      TransportTls
    | -- | Any other client-reported fault (a malformed response, an unparseable URL, an internal client error): the closed catch-all.
      TransportProtocol
    deriving stock (Eq, Show)

-- | What a transport cause says happened, for a line an operator reads.
renderTransportCause :: TransportCause -> Text
renderTransportCause = \case
    TransportTimeout -> "the peer did not answer in time"
    TransportUnreachable -> "the peer could not be reached"
    TransportTls -> "the TLS layer refused the peer"
    TransportProtocol -> "the peer's answer could not be used"

-- | A transport fault for a log line: its detail, then its cause.
renderTransportFault :: TransportFault -> Text
renderTransportFault fault = tfDetail fault <> " (" <> renderTransportCause (tfCause fault) <> ")"

-- | Whether another attempt can help. A timeout and an unreachable peer clear on their own, and a TLS or protocol fault needs a fix.
transportRetryable :: TransportCause -> Bool
transportRetryable = \case
    TransportTimeout -> True
    TransportUnreachable -> True
    TransportTls -> False
    TransportProtocol -> False

-- | A failed HTTP exchange without its request or its response, either of which can carry a credential.
data HttpFault
    = -- | The peer answered with this non-2xx status.
      HttpStatus Int
    | -- | The transport delivered no usable answer.
      HttpTransport TransportFault
    deriving stock (Eq, Show)

-- | A fault for a log line. A status follows its source's @host:port@, and a transport fault reads from its detail, which names the endpoint.
renderHttpFault :: Text -> HttpFault -> Text
renderHttpFault source = \case
    HttpStatus code -> source <> ": HTTP " <> show code
    HttpTransport fault -> renderTransportFault fault

-- | A @Retry-After@ delay in whole seconds, never confused with another integer on its way to a header or a sweep's wait.
newtype RetryAfter = RetryAfter Int
    deriving stock (Eq, Ord, Show)

-- | Truncate a rendered detail to the log-line budget every fault vocabulary shares.
boundedDetail :: Text -> Text
boundedDetail = T.copy . T.take maxDetailChars

-- The rendered-detail budget: generous enough for any realistic client-library
-- message, small enough that a held fault value stays log-line sized.
maxDetailChars :: Int
maxDetailChars = 512
