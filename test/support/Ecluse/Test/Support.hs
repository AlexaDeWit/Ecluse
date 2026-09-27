-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cross-cutting test helpers no single subsystem owns.

A helper that belongs to one subsystem lives in that subsystem's @Ecluse.Test.*@
module instead.
-}
module Ecluse.Test.Support (
    testServeAdmission,
    testMemoryAdmission,
    fullMemoryReading,
    idleMemoryReading,
    closedMemoryGate,
    awaitMemoryWaiters,
    newTestClock,
    expectRight,
    expectRightText,
    expectRightIO,
    decodeJsonOrFail,
    parseRequestOrFail,
    TestContractEscape (..),
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Time (UTCTime)
import Network.HTTP.Client qualified as Client
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission)
import Ecluse.Core.Server.Admission.Memory (GateStats (gsWaited), MemoryAdmission, newMemoryAdmission, newMemoryAdmissionTuned, publishReading, readGateStats)
import Ecluse.Core.Server.Admission.Memory.Brake (BrakeState (BrakeReleased))
import Ecluse.Core.Server.Admission.Memory.Gate (Reading (Reading), defaultGateThresholds, mkMemoryView)

{- | A serve admission for suites that do not test overload. Its capacity sits far above any
test's in-flight load, so it never sheds.
-}
testServeAdmission :: IO ServeAdmission
testServeAdmission = newServeAdmission 1_000_000

{- | A memory gate for suites that do not test memory pressure. No sampler feeds it, so it
measures no ceiling and never holds work.
-}
testMemoryAdmission :: IO MemoryAdmission
testMemoryAdmission = newMemoryAdmission 1_000_000

-- | One view at its ceiling: publishing it closes a memory gate.
fullMemoryReading :: Reading
fullMemoryReading = Reading (maybeToList (mkMemoryView 100 100)) BrakeReleased

-- | One view far below its ceiling: publishing it reopens a closed memory gate.
idleMemoryReading :: Reading
idleMemoryReading = Reading (maybeToList (mkMemoryView 0 100)) BrakeReleased

-- | A memory gate closed by 'fullMemoryReading', with its waiting room and wait budget (microseconds).
closedMemoryGate :: Int -> Int -> IO MemoryAdmission
closedMemoryGate room waitMicros = do
    gate <- newMemoryAdmissionTuned defaultGateThresholds room waitMicros
    _ <- publishReading gate fullMemoryReading
    pure gate

-- | Block until at least this many requests have waited at the gate, so a test acts while they wait.
awaitMemoryWaiters :: MemoryAdmission -> Int -> IO ()
awaitMemoryWaiters gate n = do
    stats <- readGateStats gate
    unless (gsWaited stats >= n) (threadDelay 1_000 >> awaitMemoryWaiters gate n)

{- | An IORef-backed clock a test advances by hand, so a case can elapse wall-clock time
without sleeping. The pair is the read action and the setter.
-}
newTestClock :: UTCTime -> IO (IO UTCTime, UTCTime -> IO ())
newTestClock start = do
    ref <- newIORef start
    pure (readIORef ref, writeIORef ref)

-- | Assert a 'Right' and return its value, failing the running example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure

{- | 'expectRight' for a Left that already reads as a sentence. 'Show' would quote and escape
it, which buries the message the fixture wrote.
-}
expectRightText :: Either Text a -> IO a
expectRightText = either (fail . toString) pure

{- | 'expectRight' over an action that answers a typed outcome, the shape a queue, a store,
or a request former reports through.
-}
expectRightIO :: (Show e) => IO (Either e a) -> IO a
expectRightIO action = action >>= expectRight

-- | Decode JSON, failing the running example with the aeson error rather than crashing.
decodeJsonOrFail :: (FromJSON a) => ByteString -> IO a
decodeJsonOrFail bs = either (\e -> fail ("decode failure: " <> e)) pure (eitherDecodeStrict bs)

{- | Build an HTTP request from a URL, failing the running example on an unparseable one.
'Client.parseRequest' reports the failure through 'MonadThrow', which is 'IO' here.
-}
parseRequestOrFail :: Text -> IO Client.Request
parseRequestOrFail = Client.parseRequest . toString

{- | A typed stand-in for an exception thrown past a handle's typed contract. A test double
that must never be called throws this instead of a stringly exception.
-}
newtype TestContractEscape = TestContractEscape Text
    deriving stock (Eq, Show)

instance Exception TestContractEscape
