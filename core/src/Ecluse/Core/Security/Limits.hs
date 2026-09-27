-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Response bounds for the data plane: what an upstream may make the proxy hold, walk, or wait on.

A 'Limits' budget bounds the algorithmic-complexity and stalling DoS a hostile or compromised
upstream can inflict. Every limit fails closed: a breach yields 'Left', never a truncated or partial result.
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

    -- * Upstream progress
    ProgressFloor,
    ProgressFloorError (..),
    mkProgressFloor,
    floorWindowMicros,
    floorMinBytes,
    floorServeCapMicros,
    requestTimeoutSeconds,
    serveCapMarginSeconds,
    serveCapSeconds,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)

import Ecluse.Core.Package (PackageInfo, infoVersions, pkgArtifacts)

-- | Byte ceilings by operation, structural metadata backstops, and the upstream progress floor.
data Limits = Limits
    { maxMetadataBytes :: Int
    -- ^ Decompressed registry metadata and control-response bytes.
    , maxPublishRequestBytes :: Int
    -- ^ Client publish request bytes buffered before relay.
    , maxMirrorArtifactBytes :: Int
    -- ^ Artifact bytes buffered for mirror verification and publication.
    , maxVersionCount :: Int
    -- ^ npm source versions, PyPI full releases, or PyPI selected source file positions.
    , maxArtifactCount :: Int
    -- ^ Valid projected artifacts. Selected PyPI uses its source-file scan count instead.
    , maxNestingDepth :: Int
    -- ^ Retained JSON nesting depth. Skipped metadata fields do not use this bound.
    , progressFloor :: ProgressFloor
    -- ^ The body bytes an upstream exchange must deliver per window, and the serve path's cap.
    }
    deriving stock (Eq, Show)

-- | Generous bounded metadata input, with publish and mirror caps resolved separately by composition.
defaultLimits :: Limits
defaultLimits =
    Limits
        { maxMetadataBytes = 128 * 1024 * 1024
        , maxPublishRequestBytes = 12 * 1024 * 1024
        , maxMirrorArtifactBytes = 12 * 1024 * 1024
        , maxVersionCount = 1_000_000
        , maxArtifactCount = 1_000_000
        , maxNestingDepth = 64
        , progressFloor = ProgressFloor (fromInteger (toMicros 10)) (1024 * 1024) (fromInteger (toMicros (fromIntegral serveCapSeconds)))
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

{- | The front door's per-request timeout, in seconds. Generous enough for a large packument
fetch, bounded so a stuck upstream cannot pin a handler indefinitely.
-}
requestTimeoutSeconds :: Int
requestTimeoutSeconds = 60

-- | Seconds the serve-path cap leaves under the request timeout for admission waits and the work around an exchange.
serveCapMarginSeconds :: Int
serveCapMarginSeconds = 10

-- | How long one serve-path upstream exchange may run, in seconds: the request timeout less its margin.
serveCapSeconds :: Int
serveCapSeconds = requestTimeoutSeconds - serveCapMarginSeconds

{- | A progress window, the body bytes a transfer must move within it, and the serve-path cap the
window must stay below. The private constructor keeps the three consistent.
-}
data ProgressFloor = ProgressFloor Int Int Int
    deriving stock (Eq, Show)

-- | Why a window and a byte count make no 'ProgressFloor'.
data ProgressFloorError
    = -- | The window is zero or negative.
      WindowNotPositive
    | -- | The window is not below the serve-path cap, so the floor could never fire before the cap.
      WindowNotBelowServeCap
    | -- | The byte count is zero or negative.
      MinBytesNotPositive
    deriving stock (Eq, Show)

-- | The floor for a serve-path cap, a window, and a byte count, in that order, with every refusal.
mkProgressFloor :: NominalDiffTime -> NominalDiffTime -> Int -> Either (NonEmpty ProgressFloorError) ProgressFloor
mkProgressFloor serveCap window minBytes =
    maybe (Right (ProgressFloor (fromInteger windowMicros) minBytes (fromInteger capMicros))) Left (nonEmpty refusals)
  where
    windowMicros = toMicros window
    capMicros = toMicros serveCap
    refusals =
        [WindowNotPositive | windowMicros <= 0]
            <> [WindowNotBelowServeCap | windowMicros > 0, windowMicros >= capMicros]
            <> [MinBytesNotPositive | minBytes <= 0]

-- | The waiting time, in microseconds, within which a transfer must move 'floorMinBytes'.
floorWindowMicros :: ProgressFloor -> Int
floorWindowMicros (ProgressFloor window _ _) = window

-- | The body bytes a transfer must move within each window.
floorMinBytes :: ProgressFloor -> Int
floorMinBytes (ProgressFloor _ minBytes _) = minBytes

-- | How long one serve-path exchange may run, in microseconds.
floorServeCapMicros :: ProgressFloor -> Int
floorServeCapMicros (ProgressFloor _ _ cap) = cap

toMicros :: NominalDiffTime -> Integer
toMicros seconds = round (seconds * 1_000_000)
