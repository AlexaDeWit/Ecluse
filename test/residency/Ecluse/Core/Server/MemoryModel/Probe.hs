-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Isolated retained-heap probes for the shipping metadata representation.
Preparation forces derived renderings, so its allocation and high-water counters include that work.
-}
module Ecluse.Core.Server.MemoryModel.Probe (
    Shape (..),
    Measurement (..),
    packages,
    probe,
) where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo (infoVersions), pkgEcosystem)
import Ecluse.Core.Registry.CachedDocument (npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache (CacheEntry (CacheEntry), weighCacheEntry)
import Ecluse.Core.Snapshot (digestOf)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), corpusPackages, pypiCorpusPackages)

-- | Each shape gets a fresh process and an independently loaded capture.
data Shape
    = -- | Original strict input and its backing storage.
      Wire
    | -- | The decoded JSON tree without a typed projection.
      Raw
    | -- | The typed projection and any raw data it still reaches.
      Typed
    | -- | The cache entry, preserving sharing between both views.
      Shared
    deriving stock (Eq, Show, Read, Enum, Bounded)

-- | Absolute GC samples and preparation counters. Memory quantities use bytes.
data Measurement = Measurement
    { wireBytes :: Int
    , compactBytes :: Int64
    , cacheWeight :: Int
    , versions :: Int
    , baselineLive :: Word64
    , heldLive :: Word64
    , releasedLive :: Word64
    , preparationAllocated :: Word64
    , preparationMaxLive :: Word64
    }
    deriving stock (Show, Generic)

instance ToJSON Measurement
instance FromJSON Measurement

data Held = HeldWire ByteString | HeldRaw Value | HeldTyped PackageInfo | HeldShared CacheEntry

-- | Use the same complete package catalogue as the performance harnesses.
packages :: [CorpusPackage]
packages = corpusPackages <> pypiCorpusPackages

-- | Root only the selected representation across collections, then verify its release separately.
probe :: Shape -> CorpusPackage -> IO Measurement
probe shape package = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "metadata residency requires RTS -T")
    bracket (prepare shape package) (freeStablePtr . fst) (observe . fst)
    before <- sample
    (bytes, compact, weight, count, held, prepared) <-
        bracket (prepare shape package) (freeStablePtr . fst) $ \(root, (bytes, compact, weight, count)) -> do
            retained <- sample
            observe root
            pure (bytes, compact, weight, count, retained, allocated_bytes retained)
    released <- sample
    pure
        Measurement
            { wireBytes = bytes
            , compactBytes = compact
            , cacheWeight = weight
            , versions = count
            , baselineLive = live before
            , heldLive = live held
            , releasedLive = live released
            , preparationAllocated = prepared - allocated_bytes before
            , preparationMaxLive = max_live_bytes held
            }

sample :: IO RTSStats
sample = performMajorGC >> getRTSStats

-- Dereferencing after GC makes the root's continued reachability observable.
observe :: StablePtr Held -> IO ()
observe root = do
    observed <- deRefStablePtr root >>= evaluate . heldSize
    when (observed <= 0) (fail "retained metadata root is empty")

live :: RTSStats -> Word64
live = gcdetails_live_bytes . gc

-- The caller retains only a StablePtr and scalars, never the preparation closure's input graph.
{-# NOINLINE prepare #-}
prepare :: Shape -> CorpusPackage -> IO (StablePtr Held, (Int, Int64, Int, Int))
prepare shape package = do
    bytes <- BS.readFile (cpPath package)
    (held, compact, weight, count) <- case shape of
        Wire -> pure (HeldWire bytes, 0, 0, 0)
        Raw -> do
            raw <- either fail pure (eitherDecodeStrict bytes)
            _ <- forceShown raw
            compact <- evaluate (LBS.length (encode raw))
            pure (HeldRaw raw, compact, 0, 0)
        Typed -> do
            (info, _) <- project package bytes
            _ <- forceShown info
            pure (HeldTyped info, 0, 0, Map.size (infoVersions info))
        Shared -> do
            (info, raw) <- project package bytes
            document <- case pkgEcosystem (cpPackage package) of
                Npm -> pure (fst npmCached raw)
                PyPI -> pure (fst pypiSimpleCached raw)
                RubyGems -> fail "no RubyGems metadata residency corpus"
            let entry = CacheEntry info document (BS.length bytes) (digestOf bytes)
            _ <- forceShown entry
            compact <- evaluate (LBS.length (encode raw))
            weight <- evaluate (weighCacheEntry entry)
            pure (HeldShared entry, compact, weight, Map.size (infoVersions info))
    size <- evaluate (BS.length bytes)
    compactSize <- evaluate compact
    charged <- evaluate weight
    versionCount <- evaluate count
    root <- evaluate held >>= newStablePtr
    pure (root, (size, compactSize, charged, versionCount))

project :: CorpusPackage -> ByteString -> IO (PackageInfo, Value)
project package bytes = case pkgEcosystem name of
    Npm -> either (fail . show) pure (projectNpmManifest defaultLimits name bytes)
    PyPI -> either (fail . show) pure (projectPyPIIndex defaultLimits name bytes)
    RubyGems -> fail "no RubyGems metadata residency corpus"
  where
    name = cpPackage package

forceShown :: (Show a) => a -> IO Int
forceShown value = evaluate (length (show value :: String))

heldSize :: Held -> Int
heldSize = \case
    HeldWire bytes -> BS.length bytes
    HeldRaw value -> fromIntegral (LBS.length (encode value))
    HeldTyped info -> Map.size (infoVersions info)
    HeldShared entry -> weighCacheEntry entry
