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
    BodyLimit (..),
    bodyLimitBytes,
    LimitError (..),
    boundedRead,
    checkVersionCountOf,
    checkArtifactCount,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo, infoVersions, pkgArtifacts)

-- | Byte ceilings by operation, followed by structural metadata backstops.
data Limits = Limits
    { maxMetadataBytes :: Int
    -- ^ Decompressed registry metadata and control-response bytes.
    , maxPublishRequestBytes :: Int
    -- ^ Client publish request bytes buffered before relay.
    , maxMirrorArtifactBytes :: Int
    -- ^ Artifact bytes buffered for mirror verification and publication.
    , maxVersionCount :: Int
    -- ^ Most versions a parsed document may carry. Bounds per-version rule evaluation.
    , maxArtifactCount :: Int
    -- ^ Total artifacts across versions, bounding projection and residency beyond the version count.
    , maxNestingDepth :: Int
    -- ^ Deepest JSON nesting a decoded document may reach. Bounds stack\/CPU on nested input.
    }
    deriving stock (Eq, Show)

-- | Default byte ceilings are 12 MiB until composition supplies each role's resolved cap.
defaultLimits :: Limits
defaultLimits =
    Limits
        { maxMetadataBytes = 12 * 1024 * 1024
        , maxPublishRequestBytes = 12 * 1024 * 1024
        , maxMirrorArtifactBytes = 12 * 1024 * 1024
        , maxVersionCount = 100_000
        , maxArtifactCount = 100_000
        , maxNestingDepth = 64
        }

-- | The selected body role and its byte ceiling, shared by reads and failures.
data BodyLimit
    = -- | Metadata and registry control responses.
      MetadataBodyLimit Int
    | -- | Inbound first-party publish requests.
      PublishRequestBodyLimit Int
    | -- | Artifacts buffered by the mirror worker.
      MirrorArtifactBodyLimit Int
    deriving stock (Eq, Show)

-- | The selected ceiling in bytes, before decoding or projection.
bodyLimitBytes :: BodyLimit -> Int
bodyLimitBytes = \case
    MetadataBodyLimit cap -> cap
    PublishRequestBodyLimit cap -> cap
    MirrorArtifactBodyLimit cap -> cap

-- | Which 'Limits' ceiling a response exceeded.
data LimitError
    = -- | The selected body role exceeded its configured byte ceiling.
      BodyTooLarge BodyLimit
    | -- | More than 'maxVersionCount' versions. Carries the count seen and the ceiling.
      TooManyVersions Int Int
    | -- | More than 'maxArtifactCount' artifacts across the versions, then the ceiling.
      TooManyArtifacts Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'. Carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

-- | Return the consumed byte count and body. An empty chunk ends the read, and an overstep refuses it whole.
boundedRead :: (Monad m) => BodyLimit -> m ByteString -> m (Either LimitError (Int, ByteString))
boundedRead bound readChunk = go 0 mempty
  where
    cap = bodyLimitBytes bound
    go !seen acc = do
        chunk <- readChunk
        if BS.null chunk
            then pure (Right (seen, BSL.toStrict (toLazyByteString acc)))
            else
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then pure (Left (BodyTooLarge bound))
                        else go seen' (acc <> byteString chunk)

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
versions. Adapters check version counts before applying the artifact ceiling.
-}
checkArtifactCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkArtifactCount limits info
    | seen > cap = Left (TooManyArtifacts seen cap)
    | otherwise = Right info
  where
    cap = maxArtifactCount limits
    seen = Map.foldl' (\acc details -> acc + length (pkgArtifacts details)) 0 (infoVersions info)
