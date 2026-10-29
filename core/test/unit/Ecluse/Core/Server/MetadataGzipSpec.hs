-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.MetadataGzipSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.MVar qualified as MVar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec
import UnliftIO.Async (AsyncCancelled (AsyncCancelled), cancel, race_, wait, waitCatch, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned, withServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), charge, meterSnapshot, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.MetadataGzip (GzipBody (..), gzipBufferChargeBytes, gzipLazyBody, gzipMetadata, gzipWorkspaceBytes)
import Ecluse.Server.Admission.Support (ChargeRefused (..))
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Support (incompressibleBytes)

spec :: Spec
spec = describe "gzipMetadata" $ do
    it "inflates back to the identity bytes" $
        for_ bodies $ \(label, body) -> do
            compressed <- gzipMetadata (const pass) body
            (label, LBS.toStrict (GZip.decompress (gzipLazyBody compressed))) `shouldBe` (label, body)
    it "writes a gzip member at the fastest level" $ do
        compressed <- LBS.toStrict . gzipLazyBody <$> gzipMetadata (const pass) "{\"name\":\"thing\"}"
        BS.take 3 compressed `shouldBe` "\x1f\x8b\x08"
        -- RFC 1952 sets XFL to 4 for the fastest compression.
        BS.index compressed 8 `shouldBe` 4
    it "charges the workspace first, then each output buffer before it exists" $
        for_ bodies $ \(label, body) -> do
            charges <- newIORef []
            compressed <- gzipMetadata (\cost -> modifyIORef' charges (cost :)) body
            paid <- reverse <$> readIORef charges
            let chunks = gzipChunks compressed
            (label, paid) `shouldBe` (label, gzipWorkspaceBytes : replicate (max 1 (length chunks)) gzipBufferChargeBytes)
            (label, map BS.length (drop 1 (reverse chunks))) `shouldBe` (label, map (const 32768) (drop 1 chunks))
            (label, gzipLength compressed) `shouldBe` (label, fromIntegral (LBS.length (gzipLazyBody compressed)))
    it "accounts for output larger than its input" $ do
        let body = incompressibleBytes 200000
        compressed <- gzipMetadata (const pass) body
        gzipLength compressed `shouldSatisfy` (> BS.length body)
        length (gzipChunks compressed) `shouldBe` (gzipLength compressed + 32767) `div` 32768
    it "passes a refused charge to the caller" $ do
        charged <- newIORef (0 :: Int)
        let refuseSecondBuffer _ = do
                count <- atomicModifyIORef' charged (\n -> (n + 1, n + 1))
                when (count > 2) (throwIO ChargeRefused)
        gzipMetadata refuseSecondBuffer (incompressibleBytes 200000) `shouldThrow` (== ChargeRefused)
    it "stops at cancellation while it waits for a charge" $ do
        ready <- MVar.newEmptyMVar
        blocked <- MVar.newEmptyMVar
        let pauseOnBuffer cost = when (cost == gzipBufferChargeBytes) (MVar.putMVar ready () >> MVar.takeMVar blocked)
        withAsync (gzipMetadata pauseOnBuffer (incompressibleBytes 100000)) $ \worker -> do
            race_ (MVar.takeMVar ready) (void (wait worker))
            cancel worker
            outcome <- waitCatch worker
            either (\fault -> fromException fault `shouldBe` Just AsyncCancelled) (const (expectationFailure "compression ignored its cancellation")) outcome
    it "returns its admission slot and every charge when cancelled at its third output buffer" $ do
        deadlines <- newDeadlines
        meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 10, msEntryWaitMicros = 0}
        admission <- newServeAdmissionTuned deadlines 1 0 0
        ready <- MVar.newEmptyMVar
        blocked <- MVar.newEmptyMVar
        buffers <- newIORef (0 :: Int)
        let pauseAtThird ticket cost = do
                charge ticket cost
                when (cost == gzipBufferChargeBytes) $ do
                    count <- atomicModifyIORef' buffers (\n -> (n + 1, n + 1))
                    when (count == 3) (MVar.putMVar ready () >> MVar.takeMVar blocked)
            compress = withMemoryEntry noopMetricsPort meter $ \ticket ->
                withServeAdmission noopMetricsPort admission (gzipMetadata (pauseAtThird ticket) (incompressibleBytes 200000))
        withAsync compress $ \worker -> do
            race_ (MVar.takeMVar ready) (void (wait worker))
            snChargedBytes <$> meterSnapshot meter `shouldReturn` gzipWorkspaceBytes + 3 * gzipBufferChargeBytes
            cancel worker
        snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
        withServeAdmission noopMetricsPort admission pass `shouldReturn` Right ()
  where
    bodies :: [(Text, ByteString)]
    bodies =
        [ ("empty", "")
        , ("small listing", "{\"name\":\"thing\",\"versions\":{}}")
        , ("one buffer exactly", BS.replicate 32768 97)
        , ("repetitive", BS.concat (replicate 20000 "{\"version\":\"1.2.3\",\"dist\":{}},"))
        , ("incompressible", incompressibleBytes 150000)
        ]
