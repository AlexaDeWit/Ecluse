-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cross-cutting test helpers no single subsystem owns. A helper that belongs to one subsystem
lives in that subsystem's @Ecluse.Test.*@ module instead.
-}
module Ecluse.Test.Support (
    testServeAdmission,
    testMemoryMeter,
    newTestClock,
    expectRight,
    expectRightText,
    expectRightIO,
    decodeJsonOrFail,
    parseRequestOrFail,
    TestContractEscape (..),
    incompressibleBytes,
    givesTimes,
    between,
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Bits (shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.Time (UTCTime)
import Network.HTTP.Client qualified as Client

import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MeterSettings (..), newMemoryMeter)

-- | A serve admission for suites that do not test overload. Its capacity is far above any test's load.
testServeAdmission :: IO ServeAdmission
testServeAdmission = newServeAdmission 1_000_000

-- | A memory meter for suites that do not test memory pressure. Its budget never binds.
testMemoryMeter :: IO MemoryMeter
testMemoryMeter =
    newMemoryMeter MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1_048_576, msEntryRoom = 1_000_000, msEntryWaitMicros = 0}

-- | A clock a test advances by hand, as its read action and its setter, so no case sleeps.
newTestClock :: UTCTime -> IO (IO UTCTime, UTCTime -> IO ())
newTestClock start = do
    ref <- newIORef start
    pure (readIORef ref, writeIORef ref)

-- | Assert a 'Right' and return its value, failing the running example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure

-- | 'expectRight' for a Left that already reads as a sentence, which 'Show' would quote and escape.
expectRightText :: Either Text a -> IO a
expectRightText = either (fail . toString) pure

-- | 'expectRight' over an action that answers a typed outcome.
expectRightIO :: (Show e) => IO (Either e a) -> IO a
expectRightIO action = action >>= expectRight

-- | Decode JSON, failing the running example with the aeson error rather than crashing.
decodeJsonOrFail :: (FromJSON a) => ByteString -> IO a
decodeJsonOrFail bs = either (\e -> fail ("decode failure: " <> e)) pure (eitherDecodeStrict bs)

-- | Build an HTTP request from a URL, failing the running example on an unparseable one.
parseRequestOrFail :: Text -> IO Client.Request
parseRequestOrFail = Client.parseRequest . toString

-- | The typed exception a test double that must never be called throws.
newtype TestContractEscape = TestContractEscape Text
    deriving stock (Eq, Show)

instance Exception TestContractEscape

-- | A streamed request body that hands over the chunk this many times, then ends.
givesTimes :: Int -> ByteString -> Client.GivesPopper ()
givesTimes count chunk needsPopper = do
    remaining <- newIORef count
    needsPopper $ atomicModifyIORef' remaining $ \left -> if left <= 0 then (0, BS.empty) else (left - 1, chunk)

-- | Whether a duration in seconds is at least the low bound and below the high one.
between :: Double -> Double -> Double -> Bool
between low high seconds = seconds >= low && seconds < high

-- | Deterministic bytes no compressor can shrink: an xorshift stream.
incompressibleBytes :: Int -> ByteString
incompressibleBytes size = BS.pack (take size (map fromIntegral (drop 1 (iterate next (0x9e3779b9 :: Word64)))))
  where
    next x0 = let x1 = x0 `xor` (x0 `shiftL` 13); x2 = x1 `xor` (x1 `shiftR` 7) in x2 `xor` (x2 `shiftL` 17)
