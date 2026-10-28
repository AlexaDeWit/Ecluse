-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fresh-process observations separate bounded verified bodies from constant-memory streamed delivery.
module Ecluse.Core.Server.Pipeline.Tarball.VerifiedResidencySpec (spec, probeMain) where

import Control.Concurrent.MVar qualified as MVar
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Async (cancel, waitCatch, waitEither, withAsync)
import UnliftIO.Exception (evaluate)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.NativeProbe (probeChild, processMemory)
import Ecluse.Core.Package (Artifact (artSize), HashAlg (SHA256), mkHash)
import Ecluse.Core.Registry.Json.WalkProbe (allowance)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Core.Server.MemoryModel.HeapSample qualified as Heap
import Ecluse.Core.Server.Pipeline.Tarball.Verified (VerifiedArtifactStage (..), readVerifiedArtifactWith)
import Ecluse.Core.Server.Pipeline.Tarball.Verified.Internal (VerifiedArtifactBody (VerifiedArtifactBody))
import Ecluse.Test.Package (artifactWith)
import Ecluse.Test.Support (expectRight)

data ProbeInput = ProbeInput
    { inputBytes :: Int
    , inputChunkBytes :: Int
    , inputKnownSize :: Bool
    , inputCapBytes :: Int
    }
    deriving stock (Generic, Show)

instance FromJSON ProbeInput
instance ToJSON ProbeInput

data Measurement = Measurement
    { bodyBytes :: Int
    , chunkBytes :: Int
    , declaredBytes :: Maybe Int
    , resolvedCapBytes :: Int
    , effectiveReadLimitBytes :: Int
    , consumedBytes :: Int
    , consumedChunks :: Int
    , outputBytes :: Int
    , requestedChargeBytes :: Int
    , baselineLiveBytes :: Word64
    , heldLiveBytes :: Word64
    , releasedLiveBytes :: Word64
    , baselineRssBytes :: Integer
    , heldRssBytes :: Integer
    , releasedRssBytes :: Integer
    , producerAllocatedBytes :: Integer
    , allocatedBytes :: Integer
    , peakHeapBytes :: Integer
    , retainedHeapBytes :: Integer
    , releasedHeapBytes :: Integer
    , hashStarted :: Bool
    , hashCompleted :: Bool
    , digestVerified :: Bool
    , hashNanoseconds :: Maybe Word64
    , outcome :: Text
    }
    deriving stock (Generic, Show)

instance FromJSON Measurement
instance ToJSON Measurement

data Prepared = Prepared
    { preparedRoot :: Maybe (StablePtr VerifiedArtifactBody)
    , preparedConsumed :: Int
    , preparedChunks :: Int
    , preparedCharge :: Int
    , preparedProducerAllocated :: Integer
    , preparedPeak :: Integer
    , preparedHashStarted :: Bool
    , preparedHashCompleted :: Bool
    , preparedDigestVerified :: Bool
    , preparedHashNanoseconds :: Maybe Word64
    , preparedOutcome :: Text
    }

spec :: Spec
spec = describe "verified artifact buffer and hash residency" $ do
    for_ shapes $ \(size, width) -> for_ [False, True] $ \known ->
        it ("records complete buffer and hash work: " <> show (size, width, known)) $ do
            result <- child "success" (ProbeInput size width known (if known then 2 * size else size))
            outcome result `shouldBe` "verified"
            consumedBytes result `shouldBe` size
            outputBytes result `shouldBe` size
            hashStarted result `shouldBe` True
            hashCompleted result `shouldBe` True
            digestVerified result `shouldBe` True
            hashNanoseconds result `shouldSatisfy` isJust
            allocatedBytes result `shouldSatisfy` (> 0)
            requestedChargeBytes result `shouldSatisfy` (>= size)
            peakHeapBytes result `shouldSatisfy` (<= toInteger (requestedChargeBytes result) + allowance)
            retainedHeapBytes result `shouldSatisfy` (<= toInteger (bufferedChunkCharge size) + allowance)
            retainedHeapBytes result `shouldSatisfy` (>= toInteger size - allowance)
            releasedHeapBytes result `shouldSatisfy` (<= allowance)
    it "records an unknown source shorter than its reserved per-artifact cap" $ do
        result <- child "success" (ProbeInput (64 * 1024) 4096 False (1024 * 1024))
        outcome result `shouldBe` "verified"
        outputBytes result `shouldBe` 64 * 1024
        resolvedCapBytes result `shouldBe` 1024 * 1024
        peakHeapBytes result `shouldSatisfy` (<= toInteger (requestedChargeBytes result) + allowance)
        releasedHeapBytes result `shouldSatisfy` (<= allowance)
    for_ ["over-cap", "mismatch", "cancel-fetch", "cancel-hash-entry"] $ \mode ->
        it ("releases the buffered body after " <> toString mode) $ do
            result <- child mode (ProbeInput (1024 * 1024) 4096 False (1024 * 1024))
            outcome result `shouldNotBe` "verified"
            outputBytes result `shouldBe` 0
            digestVerified result `shouldBe` False
            hashCompleted result `shouldBe` (mode == "mismatch")
            requestedChargeBytes result `shouldSatisfy` (> 0)
            peakHeapBytes result `shouldSatisfy` (<= toInteger (requestedChargeBytes result) + allowance)
            releasedHeapBytes result `shouldSatisfy` (<= allowance)
            when (mode == "cancel-hash-entry") (hashStarted result `shouldBe` True)
  where
    shapes = [(16 * 1024, 1), (64 * 1024, 1)] <> [(size, width) | size <- [64 * 1024, 1024 * 1024], width <- [64, 4096, 65536]]

child :: Text -> ProbeInput -> IO Measurement
child mode input = withSystemTempDirectory "ecluse-verified-residency" $ \directory -> do
    let path = directory </> "input.json"
    LBS.writeFile path (encode input)
    probeChild "verified-artifact" mode path "128k"

-- | Raw observations precede charge assertions. Each child retains only its selected verified body.
probeMain :: String -> FilePath -> IO ()
probeMain mode path = do
    input <- BS.readFile path >>= either fail pure . eitherDecodeStrict
    measure (toText mode) input >>= LBS.putStr . encode

measure :: Text -> ProbeInput -> IO Measurement
measure mode input = do
    unless (inputBytes input > 0 && inputChunkBytes input > 0 && inputCapBytes input > 0) (fail "invalid verified artifact probe input")
    let digest = show (hashlazy (LBS.replicate (fromIntegral (inputBytes input)) 97) :: Digest SHA256) :: Text
    checksum <- expectRight (mkHash SHA256 digest)
    _ <- evaluate (length (show checksum :: String))
    let artifact = (artifactWith [checksum]){artSize = if inputKnownSize input then Just (inputBytes input) else Nothing}
    (_, beforeRss) <- processMemory
    baseline <- Heap.sample
    prepared <- prepare mode input artifact baseline
    (held, heldRss) <- case preparedRoot prepared of
        Nothing -> (,) <$> Heap.sample <*> (snd <$> processMemory)
        Just pointer -> do
            sampled <- Heap.sample
            VerifiedArtifactBody bytes _ <- deRefStablePtr pointer
            _ <- evaluate (BS.length bytes)
            (_, resident) <- processMemory
            freeStablePtr pointer
            pure (sampled, resident)
    released <- Heap.sample
    (_, releasedRss) <- processMemory
    pure
        Measurement
            { bodyBytes = inputBytes input
            , chunkBytes = inputChunkBytes input
            , declaredBytes = artSize artifact
            , resolvedCapBytes = if mode == "over-cap" then inputBytes input - 1 else inputCapBytes input
            , effectiveReadLimitBytes = fromMaybe (if mode == "over-cap" then inputBytes input - 1 else inputCapBytes input) (artSize artifact)
            , consumedBytes = preparedConsumed prepared
            , consumedChunks = preparedChunks prepared
            , outputBytes = if preparedOutcome prepared == "verified" then inputBytes input else 0
            , requestedChargeBytes = preparedCharge prepared
            , baselineLiveBytes = Heap.live baseline
            , heldLiveBytes = Heap.live held
            , releasedLiveBytes = Heap.live released
            , baselineRssBytes = beforeRss
            , heldRssBytes = heldRss
            , releasedRssBytes = releasedRss
            , producerAllocatedBytes = preparedProducerAllocated prepared
            , allocatedBytes = toInteger (Heap.sampleAllocated held) - toInteger (Heap.sampleAllocated baseline)
            , peakHeapBytes = max (preparedPeak prepared) (toInteger (Heap.live held) - toInteger (Heap.live baseline))
            , retainedHeapBytes = toInteger (Heap.live held) - toInteger (Heap.live baseline)
            , releasedHeapBytes = max 0 (toInteger (Heap.live released) - toInteger (Heap.live baseline))
            , hashStarted = preparedHashStarted prepared
            , hashCompleted = preparedHashCompleted prepared
            , digestVerified = preparedDigestVerified prepared
            , hashNanoseconds = preparedHashNanoseconds prepared
            , outcome = preparedOutcome prepared
            }

-- Only the stable pointer and scalar observations escape the read interval.
{-# NOINLINE prepare #-}
prepare :: Text -> ProbeInput -> Artifact -> Heap.HeapSample -> IO Prepared
prepare mode input artifact baseline = do
    consumed <- newIORef 0
    chunks <- newIORef 0
    charged <- newIORef 0
    producerAllocated <- newIORef 0
    highest <- newIORef 0
    nextSample <- newIORef (64 * 1024)
    started <- newIORef False
    completed <- newIORef False
    verified <- newIORef False
    hashStart <- newIORef Nothing
    hashElapsed <- newIORef Nothing
    reached <- MVar.newEmptyMVar
    blocked <- MVar.newEmptyMVar
    let sample = Heap.sample >>= \now -> modifyIORef' highest (max (toInteger (Heap.live now) - toInteger (Heap.live baseline)))
        stop = MVar.putMVar reached () >> MVar.takeMVar blocked
        next = do
            position <- readIORef consumed
            when (mode == "cancel-fetch" && position >= inputBytes input `div` 2) stop
            if position == inputBytes input
                then pure BS.empty
                else do
                    let width = min (inputChunkBytes input) (inputBytes input - position)
                    modifyIORef' consumed (+ width)
                    modifyIORef' chunks (+ 1)
                    allocationBefore <- getAllocationCounter
                    bytes <- evaluate (BS.replicate width (if mode == "mismatch" then 98 else 97))
                    allocationAfter <- getAllocationCounter
                    modifyIORef' producerAllocated (+ max 0 (toInteger allocationBefore - toInteger allocationAfter))
                    pure bytes
        pay cost = do
            modifyIORef' charged (max cost)
            position <- readIORef consumed
            threshold <- readIORef nextSample
            when (position >= threshold) (sample >> writeIORef nextSample (position + 64 * 1024))
        stage = \case
            VerifyingArtifact -> do
                writeIORef started True
                sample
                when (mode == "cancel-hash-entry") stop
                getMonotonicTimeNSec >>= writeIORef hashStart . Just
            HashingArtifactValidator -> do
                sample
                getMonotonicTimeNSec >>= writeIORef hashStart . Just
            ArtifactHashCompleted -> do
                end <- getMonotonicTimeNSec
                beginning <- readIORef hashStart
                for_ beginning $ \start -> modifyIORef' hashElapsed (Just . (+ (end - start)) . fromMaybe 0)
                writeIORef hashStart Nothing
                writeIORef completed True
                sample
            ArtifactDigestVerified -> writeIORef verified True >> sample
        cap = if mode == "over-cap" then inputBytes input - 1 else inputCapBytes input
        work = readVerifiedArtifactWith stage cap pay artifact next
    result <-
        if mode `elem` ["cancel-fetch", "cancel-hash-entry"]
            then withAsync work $ \worker -> withAsync (MVar.takeMVar reached) $ \signal -> do
                waitEither signal worker >>= \case
                    Right _ -> fail "verified artifact completed before the cancellation checkpoint"
                    Left () -> pure ()
                cancel worker
                waitCatch worker >>= \case
                    Left _ -> pure (Left "cancelled")
                    Right _ -> fail "verified artifact completed before cancellation"
            else first show <$> work
    sample
    rooted <- case result of
        Left problem -> pure (Nothing, problem)
        Right body@(VerifiedArtifactBody bytes _) -> do
            _ <- evaluate (BS.length bytes)
            pointer <- newStablePtr body
            pure (Just pointer, "verified")
    Prepared (fst rooted) <$> readIORef consumed <*> readIORef chunks <*> readIORef charged <*> readIORef producerAllocated <*> readIORef highest <*> readIORef started <*> readIORef completed <*> readIORef verified <*> readIORef hashElapsed <*> pure (snd rooted)
