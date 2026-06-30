{- | Outbound-request and response-bound guards for the proxy's data plane.

Écluse builds outbound HTTP requests from two untrusted sources — __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@) — and then parses whatever an upstream
returns. This module is the pure guard layer that keeps those steps from being
steered or exhausted by hostile input. It defends three boundaries:

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



-- ── response bounds ──────────────────────────────────────────────────────────

{- | Resource budget for a single upstream response. Every field is a hard
ceiling enforced fail-closed: exceeding one aborts with a 'LimitError' rather
than returning a truncated or partially-parsed result. These bound the
algorithmic-complexity DoS a hostile or compromised upstream can inflict by
returning a huge or pathological document.

The metadata ceilings are layered. 'maxBodyBytes' (through 'boundedRead') is the
__primary, pre-decode__ bound: it caps the parse spend before aeson runs, so a
hostile body is aborted while still streaming and never reaches the decoder whole.
The post-projection 'maxVersionCount' ('checkVersionCount') is a __deliberate
defence-in-depth__ semantic backstop /behind/ it — it refuses an over-versioned
packument after projection, bounding per-version work the byte cap already keeps
finite.
-}
data Limits = Limits
    { maxBodyBytes :: Int
    {- ^ Largest response body, in bytes, 'boundedRead' will accumulate before
    aborting. Bounds memory on the metadata path (artifacts are streamed, not
    buffered).
    -}
    , maxVersionCount :: Int
    {- ^ Largest number of versions a parsed packument may carry
    ('checkVersionCount'); bounds per-version rule evaluation.
    -}
    , maxNestingDepth :: Int
    {- ^ Deepest JSON nesting a decoded document may reach ('checkNestingDepth');
    bounds stack\/CPU on pathologically nested input.
    -}
    }
    deriving stock (Eq, Show)

{- | Sane defaults for 'Limits'. Generous enough for real registry documents and
tight enough to fail closed on pathological input: a 12 MiB metadata body, 100k
versions, and 64 levels of JSON nesting. Override per deployment as needed.
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
    = -- | The body exceeded 'maxBodyBytes'; carries the configured ceiling.
      BodyTooLarge Int
    | {- | The packument carried more than 'maxVersionCount' versions; carries the
      count seen and the ceiling.
      -}
      TooManyVersions Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'; carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

{- | Read a streamed body chunk-by-chunk, aborting as soon as the accumulated
size would exceed 'maxBodyBytes'. Polymorphic over the producing monad so the
streaming fetch can run it in 'IO' while tests drive it purely.

@readChunk@ is a chunk producer following the @http-client@ @BodyReader@ contract:
each call yields the next chunk, and an __empty__ 'ByteString' signals end of
input. 'boundedRead' pulls chunks until EOF and returns the concatenated body, or
stops at the first chunk that pushes the running total past 'maxBodyBytes' and
returns @'Left' ('BodyTooLarge' …)@ — __fail-closed__, never a truncated body. A
zero or negative 'maxBodyBytes' rejects any non-empty body. The bound is checked
__before__ a chunk is retained, so memory never exceeds the limit plus one chunk.
-}
boundedRead :: (Monad m) => Limits -> m ByteString -> m (Either LimitError ByteString)
boundedRead limits readChunk = go 0 mempty
  where
    cap = maxBodyBytes limits
    -- Accumulate the body in a forward-built 'Builder' (chunks appended in arrival
    -- order), finalised once at EOF — no reversed chunk list to undo.
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

Applied after a document is projected to 'Ecluse.Core.Package.PackageInfo' but before
per-version rule evaluation, so the cost of evaluating rules over every version is
bounded by configuration rather than by what an upstream returns. Counts the
'Ecluse.Core.Package.infoVersions' map; on breach returns @'Left' ('TooManyVersions'
count cap)@, otherwise the document unchanged so it threads through a parse
pipeline.
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

Run on the __already-decoded__ 'Value' — after the parser has produced it, before
the document is projected to domain types — so a pathologically nested payload is
refused before any deep /domain/ traversal. It is therefore __not__ the defence
against an unbounded structure: the structure is already /bounded-by-body-size/ by
the time it reaches here, since the @maxBodyBytes@ cap on the streamed read precedes
the decode (a body the parser never finishes reading never produces a 'Value'). This
guard bounds the __traversal cost__ of a within-size-but-deeply-nested document — the
stack\/CPU a recursive walk of it would spend — which the body cap alone does not
bound (a small body can still nest deeply). Depth counts container nesting: a scalar
is depth @1@, and each enclosing 'Object'\/'Array' adds one. An empty container
counts as a leaf (depth @1@), since it forces no descent. Traversal short-circuits at
the first sub-tree to breach the ceiling, so a deeply-nested branch costs no more than
the ceiling to reject.
-}
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if withinNestingBudget (maxNestingDepth limits) value
        then Right value
        else Left (TooDeeplyNested (maxNestingDepth limits))

{- | True iff @value@ nests no deeper than @budget@ levels — the depth predicate
'checkNestingDepth' decides against 'maxNestingDepth', exposed so a /selective/ decode
that never materialises the whole 'Value' (see
"Ecluse.Core.Registry.Npm.SelectiveDecode") can bound each sub-tree it walks at the same
budget and so reproduce 'checkNestingDepth' over the document exactly.

Depth counts container nesting: a scalar is depth @1@, an empty container is a leaf
(depth @1@, it forces no descent), and each enclosing 'Object'\/'Array' adds one.
Decrements per nested container and fails fast at zero, so a huge sub-tree is not fully
walked.
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
