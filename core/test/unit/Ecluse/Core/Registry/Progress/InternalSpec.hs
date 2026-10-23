-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The unboxed progress window keeps the state, waited time, and floor verdicts of the window it replaced.
module Ecluse.Core.Registry.Progress.InternalSpec (spec) where

import Hedgehog (Gen, evalEither, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Progress.Internal (Window (..), beginAt, endAt, freshWindow, waitedBy)
import Ecluse.Core.Security (ProgressFloor, floorMinBytes, mkProgressFloor)

spec :: Spec
spec =
    describe "properties" $
        modifyMaxSuccess (const 1000) $
            it "matches the replaced window after every wait opened or closed, and at every check of the floor" $
                hedgehog $ do
                    chunk <- forAll (Gen.int (Range.exponential 1 (64 * 1024)))
                    minBytes <- forAll (Gen.choice [(chunk *) <$> Gen.int (Range.linear 1 8), Gen.int (Range.exponential 1 (4 * 1024 * 1024))])
                    budget <- forAll (Gen.word64 (Range.exponential 1 30_000_000_000))
                    slack <- forAll (Gen.word64 (Range.exponential 1 1_000_000_000))
                    events <- forAll (Gen.list (Range.linear 0 100) (genEvent chunk minBytes))
                    progress <- evalEither (mkProgressFloor 60 10 minBytes)
                    let states = scanl (step progress) (freshWindow, Reference Nothing 0 0) events
                    map (asReference . fst) states === map snd states
                    for_ [(now, pair) | (Read now, pair) <- zip events states] $ \(now, (window, reference)) ->
                        for_ [budget, waitedBy now window + slack] $ \limit ->
                            verdict waitedBy limit now window === verdict referenceWaitedBy limit now reference

-- A transfer opening a wait, closing one with its bytes, or the floor's check reading its clock, each at a time in nanoseconds.
data Event
    = Begin Word64
    | End Word64 Int
    | Read Word64
    deriving stock (Show)

genEvent :: Int -> Int -> Gen Event
genEvent chunk minBytes = Gen.choice [Begin <$> time, End <$> time <*> bytes, Read <$> time]
  where
    time = Gen.word64 (Range.linear 0 60_000_000_000)
    bytes = Gen.frequency [(3, pure chunk), (1, pure 0), (1, Gen.int (Range.exponential 0 (2 * minBytes)))]

step :: ProgressFloor -> (Window, Reference) -> Event -> (Window, Reference)
step progress (window, reference) = \case
    Begin now -> (beginAt now window, referenceBegin now reference)
    End now bytes -> (endAt progress now bytes window, referenceEnd progress now bytes reference)
    Read _ -> (window, reference)

-- The waited nanoseconds, and whether the floor fires, as the check decides them.
verdict :: (Word64 -> w -> Word64) -> Word64 -> Word64 -> w -> (Word64, Bool)
verdict waited budget now window = (spent, spent >= budget)
  where
    spent = waited now window

asReference :: Window -> Reference
asReference = \case
    Waiting began waited moved -> Reference (Just began) waited moved
    Idle waited moved -> Reference Nothing waited moved

-- The window before the change, with its transitions copied as they were: the open wait's start, the time waited, the bytes moved.
data Reference = Reference (Maybe Word64) Word64 Int
    deriving stock (Eq, Show)

referenceBegin :: Word64 -> Reference -> Reference
referenceBegin now (Reference _ waited moved) = Reference (Just now) waited moved

referenceEnd :: ProgressFloor -> Word64 -> Int -> Reference -> Reference
referenceEnd progress now bytes current@(Reference _ _ moved) =
    if moved + bytes >= floorMinBytes progress
        then Reference Nothing 0 0
        else Reference Nothing (referenceWaitedBy now current) (moved + bytes)

referenceWaitedBy :: Word64 -> Reference -> Word64
referenceWaitedBy now (Reference since waited _) = waited + maybe 0 (\began -> if now > began then now - began else 0) since
