-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Response-bound guards for the proxy's data plane.

Écluse parses whatever an upstream returns. This module provides the pure guard layer
that keeps hostile or oversized input from exhausting resources.

A 'Limits' budget bounds the algorithmic-complexity DoS a hostile or compromised
upstream can inflict. 'boundedRead' aborts a streamed body past 'maxBodyBytes', and
'checkVersionCount' \/ 'checkArtifactCount' \/ 'checkNestingDepth' reject an oversized or
deeply-nested parsed document. Every limit fails closed: exceeding one yields 'Left', never
a truncated or partial result.
-}
module Ecluse.Core.Security.Limits (
    -- * Response bounds
    Limits (..),
    defaultLimits,
    LimitError (..),
    boundedRead,
    checkVersionCount,
    checkVersionCountOf,
    checkArtifactCount,
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

import Ecluse.Core.Package (PackageInfo, infoVersions, pkgArtifacts)

{- | Resource budget for a single upstream response. Every field is a hard ceiling enforced
fail-closed: a breach aborts with a 'LimitError' rather than a truncated result, so a hostile
upstream cannot inflict an algorithmic-complexity DoS.

'maxVersionCount' and 'maxArtifactCount' are defence-in-depth backstops behind the pre-decode
'maxBodyBytes' cap. They refuse an over-versioned or over-populated document after projection.
-}
data Limits = Limits
    { maxBodyBytes :: Int
    {- ^ Largest response body, in bytes, that 'boundedRead' accumulates before it aborts.
    Applies to the metadata path only: the proxy streams artifacts rather than buffering them.
    -}
    , maxVersionCount :: Int
    {- ^ Largest number of versions a parsed document may carry
    ('checkVersionCount'). Bounds per-version rule evaluation.
    -}
    , maxArtifactCount :: Int
    {- ^ Largest number of artifacts a parsed document may carry across all its versions
    ('checkArtifactCount'). One version can hold many artifacts, so this bounds the
    projection and residency cost 'maxVersionCount' does not reach.
    -}
    , maxNestingDepth :: Int
    {- ^ Deepest JSON nesting a decoded document may reach ('checkNestingDepth').
    Bounds stack\/CPU on pathologically nested input.
    -}
    }
    deriving stock (Eq, Show)

{- | Defaults for 'Limits': a 12 MiB metadata body, 100k versions, 100k artifacts, and 64
nesting levels. Generous for real registry documents, and tight enough to fail closed on
pathological input.
-}
defaultLimits :: Limits
defaultLimits =
    Limits
        { maxBodyBytes = 12 * 1024 * 1024
        , maxVersionCount = 100_000
        , maxArtifactCount = 100_000
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
    | {- | The document carried more than 'maxArtifactCount' artifacts across its
      versions. Carries the count seen and the ceiling.
      -}
      TooManyArtifacts Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'. Carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

{- | Read a streamed body chunk by chunk, aborting with @'Left' ('BodyTooLarge' cap)@ once the
accumulated size would exceed 'maxBodyBytes', never a truncated body. @readChunk@ follows the
@http-client@ @BodyReader@ contract: an empty 'ByteString' signals end of input.

A zero or negative 'maxBodyBytes' rejects any non-empty body. The size check runs before a
chunk is retained, so memory never exceeds the cap plus one chunk.
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

{- | Reject a parsed packument carrying more than 'maxVersionCount' versions, returning it
unchanged when within budget.

It runs after projection to 'Ecluse.Core.Package.PackageInfo' and before per-version rule
evaluation, so configuration bounds that cost rather than whatever an upstream returns.
-}
checkVersionCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkVersionCount limits info = info <$ checkVersionCountOf limits (Map.size (infoVersions info))

{- | The same ceiling over a bare count, for a caller that knows how many versions a document
carries without projecting it. The npm selective decoder counts entries as it skips them.
-}
checkVersionCountOf :: Limits -> Int -> Either LimitError ()
checkVersionCountOf limits count
    | count > cap = Left (TooManyVersions count cap)
    | otherwise = Right ()
  where
    cap = maxVersionCount limits

{- | Reject a parsed document carrying more than 'maxArtifactCount' artifacts across all its
versions. It runs after 'checkVersionCount', so an over-versioned document is still named by
its version count.
-}
checkArtifactCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkArtifactCount limits info
    | seen > cap = Left (TooManyArtifacts seen cap)
    | otherwise = Right info
  where
    cap = maxArtifactCount limits
    seen = Map.foldl' (\acc details -> acc + length (pkgArtifacts details)) 0 (infoVersions info)

{- | Reject a decoded JSON document nested deeper than 'maxNestingDepth', returning it
unchanged when within budget.

It runs on the decoded 'Value', after the parser and before projection to domain types. The
body cap already bounds structure size, so this guard bounds only the traversal cost of a
small but deeply nested document. 'withinNestingBudget' defines how depth is counted.
-}
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if withinNestingBudget (maxNestingDepth limits) value
        then Right value
        else Left (TooDeeplyNested (maxNestingDepth limits))

{- | True iff @value@ nests no deeper than @budget@ levels. The selective decode in
"Ecluse.Core.Registry.Npm.SelectiveDecode" bounds each sub-tree at the same budget, so it
reproduces 'checkNestingDepth' over a document it never materialises whole.

Depth counts container nesting: a scalar is depth @1@, an empty container is a leaf since it
forces no descent, and each enclosing 'Object' or 'Array' adds one.
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
