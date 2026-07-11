{- | The core-owned transport-fault vocabulary: why a network operation could not
deliver a response, reported as a __value__.

A client library reports a transport failure as its own exception type
(@http-client@'s @HttpException@, @amazonka@'s error sum). Carrying those types
through the agnostic tiers would couple every consumer to every client library, so
the adapter edge -- the one place a library's exception type is already in scope --
classifies the failure into this closed vocabulary, and everything above it reasons
over the value. The classification is deliberately coarse: it distinguishes only the
causes a consumer or an operator reads differently (a timeout, an unreachable peer, a
TLS refusal, or any other protocol-level fault); everything finer rides in
'tfDetail', rendered for a log line and never parsed.

This is a leaf module by design: the registry read path, the mirror queue, and the
advisory sync all speak it, so it must sit below each of them.
-}
module Ecluse.Core.Fault (
    -- * Transport faults
    TransportFault (..),
    transportFault,
    TransportCause (..),

    -- * The shared detail budget
    boundedDetail,
) where

import Data.Text qualified as T

{- | One classified transport failure: the closed cause a consumer branches on, and
the rendered client-library detail for its log line. Build it with 'transportFault'
so the detail stays bounded; the constructor is exported for pattern matches and
test fixtures.
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

{- | Why the transport could not deliver: the closed, bounded cause set. Coarse on
purpose -- each constructor is a distinction an operator reads differently in a log
or metric, and anything finer belongs in 'tfDetail'.
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
    deriving stock (Eq, Show, Bounded, Enum)

{- | Build a 'TransportFault' with the detail truncated to the log-line budget, so a
pathological rendered exception (an embedded response body, a long certificate
chain) cannot bloat a log line or a held error value.
-}
transportFault :: TransportCause -> Text -> TransportFault
transportFault cause detail = TransportFault cause (boundedDetail detail)

{- | Truncate a rendered detail to the shared log-line budget, so every fault
vocabulary that carries diagnostic text (this one, the queue's, the request
perimeter's) bounds it identically.
-}
boundedDetail :: Text -> Text
boundedDetail = T.take maxDetailChars

-- The rendered-detail budget: generous enough for any realistic client-library
-- message, small enough that a held fault value stays log-line sized.
maxDetailChars :: Int
maxDetailChars = 512
