-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Response bounds for the data plane: what an upstream may make the proxy hold or walk.

A 'Limits' budget bounds the algorithmic-complexity DoS a hostile or compromised upstream can
inflict. Every limit fails closed: a breach yields 'Left', never a truncated or partial result.
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

{- | Resource budget for a single upstream response. 'maxVersionCount' and 'maxArtifactCount'
are post-projection backstops behind the pre-decode 'maxBodyBytes' cap.
-}
data Limits = Limits
    { maxBodyBytes :: Int
    {- ^ Largest response body 'boundedRead' accumulates, in bytes. The metadata path only:
    the proxy streams artifacts rather than buffering them.
    -}
    , maxVersionCount :: Int
    -- ^ Most versions a parsed document may carry. Bounds per-version rule evaluation.
    , maxArtifactCount :: Int
    {- ^ Most artifacts a parsed document may carry across all its versions. One version can
    hold many, so this bounds the projection and residency cost 'maxVersionCount' does not reach.
    -}
    , maxNestingDepth :: Int
    -- ^ Deepest JSON nesting a decoded document may reach. Bounds stack\/CPU on nested input.
    }
    deriving stock (Eq, Show)

{- | Defaults: a 12 MiB metadata body, 100k versions, 100k artifacts, 64 nesting levels.
Generous for real documents, tight enough to fail closed on pathological input.
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
    | -- | More than 'maxVersionCount' versions. Carries the count seen and the ceiling.
      TooManyVersions Int Int
    | -- | More than 'maxArtifactCount' artifacts across the versions, then the ceiling.
      TooManyArtifacts Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'. Carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

{- | Read a streamed body chunk by chunk, refusing it whole once the accumulated size would
exceed 'maxBodyBytes'. @readChunk@ follows the @http-client@ @BodyReader@ contract, where an
empty 'ByteString' ends the input, and the size check runs before a chunk is retained.
-}
boundedRead :: (Monad m) => Limits -> m ByteString -> m (Either LimitError ByteString)
boundedRead limits readChunk = go 0 mempty
  where
    cap = maxBodyBytes limits
    -- A forward-built 'Builder': chunks appended in arrival order, finalised once at EOF.
    go !seen acc = do
        chunk <- readChunk
        if BS.null chunk
            then pure (Right (BSL.toStrict (toLazyByteString acc)))
            else
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then pure (Left (BodyTooLarge cap))
                        else go seen' (acc <> byteString chunk)

{- | Reject a parsed packument carrying more than 'maxVersionCount' versions. It runs between
projection and per-version rule evaluation, so configuration bounds that cost.
-}
checkVersionCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkVersionCount limits info = info <$ checkVersionCountOf limits (Map.size (infoVersions info))

{- | The same ceiling over a bare count, for a caller that knows how many versions a document
carries without projecting it, as the selective decoders do while they skip entries.
-}
checkVersionCountOf :: Limits -> Int -> Either LimitError ()
checkVersionCountOf limits count
    | count > cap = Left (TooManyVersions count cap)
    | otherwise = Right ()
  where
    cap = maxVersionCount limits

{- | Reject a parsed document carrying more than 'maxArtifactCount' artifacts across all its
versions. It runs after 'checkVersionCount', so an over-versioned document keeps that name.
-}
checkArtifactCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkArtifactCount limits info
    | seen > cap = Left (TooManyArtifacts seen cap)
    | otherwise = Right info
  where
    cap = maxArtifactCount limits
    seen = Map.foldl' (\acc details -> acc + length (pkgArtifacts details)) 0 (infoVersions info)

{- | Reject a decoded 'Value' nested deeper than 'maxNestingDepth'. The body cap already bounds
structure size, so this bounds only the traversal cost of a small but deeply nested document.
-}
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if withinNestingBudget (maxNestingDepth limits) value
        then Right value
        else Left (TooDeeplyNested (maxNestingDepth limits))

{- | True iff @value@ nests no deeper than @budget@ levels: a scalar and an empty container are
depth @1@, and each enclosing 'Object' or 'Array' adds one. The selective decoders bound each
sub-tree at the same budget, so they reproduce 'checkNestingDepth' without materialising whole.
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
