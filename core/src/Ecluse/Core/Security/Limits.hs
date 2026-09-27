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

    -- * Upstream exchange deadlines
    ExchangeDeadline,
    mkExchangeDeadline,
    deadlineIdleMicros,
    deadlineExchangeMicros,
    requestTimeoutSeconds,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)

import Ecluse.Core.Package (PackageInfo, infoVersions, pkgArtifacts)

-- | Byte ceilings by operation, structural metadata backstops, and the upstream exchange deadline.
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
    , exchangeDeadline :: ExchangeDeadline
    -- ^ How long an upstream body may stay silent, and how long one exchange may run.
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
        , exchangeDeadline = deadlineUnder (toMicros (fromIntegral requestTimeoutSeconds)) (toMicros 10)
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

-- | The idle interval and whole-exchange cap, in microseconds. The private constructor keeps the cap derived.
data ExchangeDeadline = ExchangeDeadline Int Int
    deriving stock (Eq, Show)

{- | The deadline for a request timeout and an idle interval, in that order. The exchange cap is the
request timeout minus the interval, which must be positive and below it.
-}
mkExchangeDeadline :: NominalDiffTime -> NominalDiffTime -> Maybe ExchangeDeadline
mkExchangeDeadline requestTimeout idle
    | 0 < idleMicros && idleMicros < requestMicros = Just (deadlineUnder requestMicros idleMicros)
    | otherwise = Nothing
  where
    idleMicros = toMicros idle
    requestMicros = toMicros requestTimeout

-- | How long one body read may wait for its next chunk, in microseconds.
deadlineIdleMicros :: ExchangeDeadline -> Int
deadlineIdleMicros (ExchangeDeadline idle _) = idle

-- | How long one exchange may run, from the request to the end of its body, in microseconds.
deadlineExchangeMicros :: ExchangeDeadline -> Int
deadlineExchangeMicros (ExchangeDeadline _ cap) = cap

-- The one derivation both builders share. Each caller keeps the interval below the timeout.
deadlineUnder :: Integer -> Integer -> ExchangeDeadline
deadlineUnder requestMicros idleMicros =
    ExchangeDeadline (fromInteger idleMicros) (fromInteger (requestMicros - idleMicros))

toMicros :: NominalDiffTime -> Integer
toMicros seconds = round (seconds * 1_000_000)
