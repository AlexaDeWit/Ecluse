-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Response-bound guards for the proxy's data plane.

Écluse parses whatever an upstream returns. This module provides the pure guard layer
that keeps hostile or oversized input from exhausting resources.

A 'Limits' budget bounds the algorithmic-complexity DoS a hostile or compromised
upstream can inflict. 'boundedRead' aborts a streamed body past 'maxBodyBytes', and
'checkVersionCount' \/ 'checkNestingDepth' reject an oversized or deeply-nested parsed
document. Every limit fails closed: exceeding one yields 'Left', never a truncated or
partial result.
-}
module Ecluse.Core.Security.Limits (
    -- * Response bounds
    Limits (..),
    defaultLimits,
    LimitError (..),
    boundedRead,
    checkVersionCount,
    checkNestingDepth,
    withinNestingBudget,
) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import Ecluse.Core.Package (PackageInfo, infoVersions)

{- | Resource budget for a single upstream response. Every field is a hard ceiling
enforced fail-closed: exceeding one aborts with a 'LimitError' rather than returning a
truncated or partially-parsed result. These bound the algorithmic-complexity DoS a
hostile or compromised upstream can inflict by returning a huge or pathological
document.

The metadata ceilings are layered. 'maxBodyBytes' (through 'boundedRead') is the
primary, pre-decode bound. It caps the parse spend before aeson runs, so a hostile body
aborts while still streaming and never reaches the decoder whole. The post-projection
'maxVersionCount' ('checkVersionCount') is a deliberate defence-in-depth semantic
backstop /behind/ it. It refuses an over-versioned packument after projection, and
bounds per-version work the byte cap already keeps finite.
-}
data Limits = Limits
    { maxBodyBytes :: Int
    {- ^ Largest response body, in bytes, that 'boundedRead' accumulates before it
    aborts. Bounds memory on the metadata path: the proxy streams artifacts rather
    than buffering them.
    -}
    , maxVersionCount :: Int
    {- ^ Largest number of versions a parsed packument may carry
    ('checkVersionCount'). Bounds per-version rule evaluation.
    -}
    , maxNestingDepth :: Int
    {- ^ Deepest JSON nesting a decoded document may reach ('checkNestingDepth').
    Bounds stack\/CPU on pathologically nested input.
    -}
    }
    deriving stock (Eq, Show)

{- | Sane defaults for 'Limits'. Generous enough for real registry documents, and
tight enough to fail closed on pathological input. The ceilings are a 12 MiB metadata
body, 100k versions, and 64 levels of JSON nesting. Override per deployment as needed.
-}
defaultLimits :: Limits
defaultLimits =
    Limits
        { maxBodyBytes = 12 * 1024 * 1024
        , maxVersionCount = 100_000
        , maxNestingDepth = 64
        }

-- | Which 'Limits' ceiling a response exceeded.
data LimitError
    = -- | The body exceeded 'maxBodyBytes'. Carries the configured ceiling.
      BodyTooLarge Int
    | {- | The packument carried more than 'maxVersionCount' versions. Carries the
      count seen and the ceiling.
      -}
      TooManyVersions Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'. Carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

{- | Read a streamed body chunk-by-chunk, aborting as soon as the accumulated size
would exceed 'maxBodyBytes'. Polymorphic over the producing monad, so the streaming
fetch can run it in 'IO' while tests drive it purely.

@readChunk@ is a chunk producer following the @http-client@ @BodyReader@ contract: each
call yields the next chunk, and an empty 'ByteString' signals end of input.
'boundedRead' pulls chunks until EOF and returns the concatenated body. It stops at the
first chunk that pushes the running total past 'maxBodyBytes' and returns
@'Left' ('BodyTooLarge' …)@, fail-closed, never a truncated body. A zero or negative
'maxBodyBytes' rejects any non-empty body. The check runs before it retains a chunk, so
memory never exceeds the limit plus one chunk.
-}
boundedRead :: (Monad m) => Limits -> m ByteString -> m (Either LimitError ByteString)
boundedRead limits readChunk = go 0 mempty
  where
    cap = maxBodyBytes limits
    -- Accumulate the body in a forward-built 'Builder': chunks appended in arrival
    -- order, finalised once at EOF. No reversed chunk list to undo.
    go !seen acc = do
        chunk <- readChunk
        if BS.null chunk
            then pure (Right (BSL.toStrict (toLazyByteString acc)))
            else
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then pure (Left (BodyTooLarge cap))
                        else go seen' (acc <> byteString chunk)

{- | Reject a parsed packument carrying more than 'maxVersionCount' versions,
returning it unchanged when within budget.

It runs after the parse pipeline projects a document to
'Ecluse.Core.Package.PackageInfo', but before per-version rule evaluation. Configuration
therefore bounds the cost of evaluating rules over every version, rather than whatever
an upstream returns. It counts the 'Ecluse.Core.Package.infoVersions' map. On breach it
returns @'Left' ('TooManyVersions' count cap)@, and otherwise the document unchanged, so
it threads through a parse pipeline.
-}
checkVersionCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkVersionCount limits info =
    if count > cap
        then Left (TooManyVersions count cap)
        else Right info
  where
    cap = maxVersionCount limits
    count = Map.size (infoVersions info)

{- | Reject a decoded JSON document nested deeper than 'maxNestingDepth',
returning it unchanged when within budget.

It runs on the already-decoded 'Value': after the parser produces it, and before the
projection to domain types. A pathologically nested payload is therefore refused
before any deep /domain/ traversal. It is not the defence against an unbounded
structure. Body size already bounds the structure by the time it reaches here, since
the @maxBodyBytes@ cap on the streamed read precedes the decode. A body the parser
never finishes reading never produces a 'Value'. This guard bounds the traversal cost
of a within-size-but-deeply-nested document. That cost is the stack\/CPU a recursive
walk would spend, which the body cap alone does not bound. A small body can still nest
deeply. Depth counts container nesting: a scalar is depth @1@, and each enclosing
'Object'\/'Array' adds one. An empty container counts as a leaf (depth @1@), since it
forces no descent. Traversal short-circuits at the first sub-tree to breach the
ceiling, so a deeply-nested branch costs no more than the ceiling to reject.
-}
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if withinNestingBudget (maxNestingDepth limits) value
        then Right value
        else Left (TooDeeplyNested (maxNestingDepth limits))

{- | True iff @value@ nests no deeper than @budget@ levels: the depth predicate
'checkNestingDepth' decides against 'maxNestingDepth'. A /selective/ decode that never
materialises the whole 'Value' (see "Ecluse.Core.Registry.Npm.SelectiveDecode") uses it
too. That decode bounds each sub-tree it walks at the same budget, and so reproduces
'checkNestingDepth' over the document exactly.

Depth counts container nesting. A scalar is depth @1@. An empty container is a leaf
(depth @1@), since it forces no descent. Each enclosing 'Object'\/'Array' adds one. It
decrements per nested container and fails fast at zero, so it never walks a huge
sub-tree whole.
-}
withinNestingBudget :: Int -> Value -> Bool
withinNestingBudget budget v =
    budget >= 1 && case v of
        Object o -> all (withinNestingBudget (budget - 1)) (KeyMap.elems o)
        Array xs -> V.all (withinNestingBudget (budget - 1)) xs
        String _ -> True
        Number _ -> True
        Bool _ -> True
        Null -> True
