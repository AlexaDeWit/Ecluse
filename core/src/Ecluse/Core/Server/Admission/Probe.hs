-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Spike only (admission fuzz): counters for every meter and gate transaction. Each capability
counts into its own padded block, so counting adds no shared write. A scrape sums the blocks.
-}
module Ecluse.Core.Server.Admission.Probe (
    Site (..),
    Kind (..),
    probed,
    probeTimed,
    probeRetry,
    probeAdd,
    probeMax,
    probeSnapshot,
    siteName,
    kindName,
    probeSelfTest,
    registerPeaks,
    probePeaks,
    invMeterModel,
    invStrict,
    nextTicketId,
) where

import Control.Concurrent (forkIO)

import Control.Concurrent.STM (retry)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (myThreadId, threadCapability, unsafeIOToSTM)
import GHC.Exts (Int (I#), MutableByteArray#, RealWorld, fetchAddIntArray#, newAlignedPinnedByteArray#, readIntArray#, setByteArray#, writeIntArray#)
import GHC.IO (IO (IO))
import System.IO.Unsafe (unsafePerformIO)

-- | The transaction sites of the meter and the weighted gate.
data Site
    = MeterEntry
    | MeterEntryWait
    | MeterEntryLeave
    | MeterGrow
    | MeterGrowWait
    | MeterWaiterAdd
    | MeterWaiterDel
    | MeterRelease
    | MeterFlightAdd
    | MeterFlightDel
    | MeterGiveBack
    | MeterRead
    | MeterSteer
    | MeterReconcile
    | GateDoor
    | GateWait
    | GateLeave
    | GateRelease
    | ModelOverBytes
    | ModelUnderBytes
    | SelfTest
    | SelfTestWait
    | TicketCounter
    deriving stock (Eq, Show, Enum, Bounded)

-- | What a site counts. Conflicts are attempts less commits less explicit retries.
data Kind = Attempts | Commits | Retries | Nanos | MaxNanos | Under10us | Under100us | Under1ms | Under10ms | Over10ms
    deriving stock (Eq, Show, Enum, Bounded)

data Cells = Cells (MutableByteArray# RealWorld)

capabilitySlots :: Int
capabilitySlots = 64

siteCount, kindCount, blockInts :: Int
siteCount = fromEnum (maxBound :: Site) + 1
kindCount = fromEnum (maxBound :: Kind) + 1
-- One block per capability, with a cache line of padding after it.
blockInts = ((siteCount * kindCount + 7) `div` 8) * 8 + 8

{-# NOINLINE cells #-}
cells :: Cells
cells = unsafePerformIO $ IO $ \s -> case newAlignedPinnedByteArray# bytes 64# s of
    (# s1, arr #) -> case setByteArray# arr 0# bytes 0# s1 of s2 -> (# s2, Cells arr #)
  where
    !(I# bytes) = capabilitySlots * blockInts * 8

slot :: Int -> Site -> Kind -> Int
slot capability site kind = capability * blockInts + fromEnum site * kindCount + fromEnum kind

currentCapability :: IO Int
currentCapability = (`mod` capabilitySlots) . fst <$> (threadCapability =<< myThreadId)

-- | Add to this capability's counter.
probeAdd :: Site -> Kind -> Int -> IO ()
probeAdd site kind (I# n) = do
    capability <- currentCapability
    let !(I# i) = slot capability site kind
        !(Cells arr) = cells
    IO $ \s -> case fetchAddIntArray# arr i n s of (# s1, _ #) -> (# s1, () #)

-- | Raise this capability's maximum. Two threads on one capability can race only across a preemption.
probeMax :: Site -> Kind -> Int -> IO ()
probeMax site kind (I# n) = do
    capability <- currentCapability
    let !(I# i) = slot capability site kind
        !(Cells arr) = cells
    IO $ \s -> case readIntArray# arr i s of
        (# s1, old #) -> if I# old >= I# n then (# s1, () #) else case writeIntArray# arr i n s1 of s2 -> (# s2, () #)

-- | Run a transaction, counting each attempt, the commit, and the time from start to commit.
probed :: Site -> STM a -> IO a
probed site body = do
    start <- getMonotonicTimeNSec
    result <- atomically (unsafeIOToSTM (probeAdd site Attempts 1) >> body)
    end <- getMonotonicTimeNSec
    let took = fromIntegral (end - start)
    probeAdd site Commits 1
    probeAdd site Nanos took
    probeMax site MaxNanos took
    probeAdd site (bucket took) 1
    pure result

-- | Run a plain action, counting it and its duration as one commit.
probeTimed :: Site -> IO a -> IO a
probeTimed site action = do
    start <- getMonotonicTimeNSec
    result <- action
    end <- getMonotonicTimeNSec
    let took = fromIntegral (end - start)
    probeAdd site Attempts 1
    probeAdd site Commits 1
    probeAdd site Nanos took
    probeMax site MaxNanos took
    probeAdd site (bucket took) 1
    pure result

-- The commit's duration bucket.
bucket :: Int -> Kind
bucket took
    | took < 10_000 = Under10us
    | took < 100_000 = Under100us
    | took < 1_000_000 = Under1ms
    | took < 10_000_000 = Under10ms
    | otherwise = Over10ms

-- | Block the transaction, counting the retry.
probeRetry :: Site -> STM a
probeRetry site = unsafeIOToSTM (probeAdd site Retries 1) >> retry

-- | Every counter summed over capabilities, and every maximum taken over them.
probeSnapshot :: IO [(Site, Kind, Int)]
probeSnapshot = traverse total [(site, kind) | site <- [minBound .. maxBound], kind <- [minBound .. maxBound]]
  where
    total (site, kind) = do
        values <- traverse (\capability -> readSlot (slot capability site kind)) [0 .. capabilitySlots - 1]
        pure (site, kind, if kind == MaxNanos then foldl' max 0 values else sum values)
    readSlot (I# i) = let !(Cells arr) = cells in IO $ \s -> case readIntArray# arr i s of (# s1, v #) -> (# s1, I# v #)

{-# NOINLINE peakSource #-}
peakSource :: IORef (IO (Int, Int))
peakSource = unsafePerformIO (newIORef (pure (0, 0)))

-- | Name the meter whose peaks the gauge reports: the most it held, and the most a take left it over budget.
registerPeaks :: IO (Int, Int) -> IO ()
registerPeaks = writeIORef peakSource

-- | The registered meter's peaks since boot.
probePeaks :: IO (Int, Int)
probePeaks = join (readIORef peakSource)

-- | Spike only: INV_METER_MODEL picks the meter model: stm (shipped), strict, or off (the oracle).
invMeterModel :: String
invMeterModel = unsafePerformIO (fromMaybe "off" <$> lookupEnv "INV_METER_MODEL")
{-# NOINLINE invMeterModel #-}

-- | The strict model: ticket ids from a fetch-and-add counter, and evaluated writes to shared cells.
invStrict :: Bool
invStrict = invMeterModel == "strict"

{-# NOINLINE ticketCounter #-}
ticketCounter :: Cells
ticketCounter = unsafePerformIO $ IO $ \s -> case newAlignedPinnedByteArray# 64# 64# s of
    (# s1, arr #) -> case setByteArray# arr 0# 64# 0# s1 of s2 -> (# s2, Cells arr #)

-- | The next ticket id, by one fetch-and-add with no thunk in a shared cell.
nextTicketId :: IO Int
nextTicketId = let !(Cells arr) = ticketCounter in IO $ \s -> case fetchAddIntArray# arr 0# 1# s of (# s1, old #) -> (# s1, I# old #)

siteName :: Site -> Text
siteName = show

kindName :: Kind -> Text
kindName = show

{- | Check that the probe sees conflicts: @threads@ threads each add one to a shared 'TVar' @iterations@
times, with @work@ list cells summed inside each transaction, while one thread waits for the total.
-}
probeSelfTest :: Int -> Int -> Int -> IO ()
probeSelfTest threads iterations work = do
    cell <- newTVarIO (0 :: Int)
    finished <- newEmptyMVar
    let target = threads * iterations
        bump = probed SelfTest $ do
            current <- readTVar cell
            let !spun = foldl' (+) current [1 .. work] - sum [1 .. work]
            writeTVar cell (spun + 1)
        waiter = probed SelfTestWait $ do
            current <- readTVar cell
            if current >= target then pure () else probeRetry SelfTestWait
    _ <- forkIO (waiter >> putMVar finished ())
    replicateM_ threads (forkIO (replicateM_ iterations bump >> putMVar finished ()))
    replicateM_ (threads + 1) (takeMVar finished)
    final <- readTVarIO cell
    snapshot <- probeSnapshot
    putTextLn ("final " <> show final <> " of " <> show target)
    for_ snapshot $ \(site, kind, value) -> when (site `elem` [SelfTest, SelfTestWait]) (putTextLn (siteName site <> " " <> kindName kind <> " " <> show value))
