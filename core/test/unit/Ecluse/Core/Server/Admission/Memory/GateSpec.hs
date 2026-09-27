-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.Memory.GateSpec (spec) where

import Hedgehog (Gen, annotateShow, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Server.Admission.Memory.Brake (BrakeState (BrakeEngaged, BrakeReleased))
import Ecluse.Core.Server.Admission.Memory.Gate

-- | The gate's pure decisions: pressure, hysteresis, reservations, and the admit, wait, shed and brake outcomes.
spec :: Spec
spec = do
    viewSpec
    settleSpec
    decideSpec
    observeSpec
    propertySpec

viewSpec :: Spec
viewSpec = describe "measured memory" $ do
    it "refuses a view whose ceiling is not positive" $ do
        mkMemoryView 10 0 `shouldBe` Nothing
        mkMemoryView 10 (-1) `shouldBe` Nothing

    it "reports no pressure without a ceiling, so memory is ungated" $
        pressure 1_000 [] `shouldBe` Nothing

    it "takes the fullest view, counting the reservation against each" $
        pressure 10 (views [(50, 100), (80, 200)]) `shouldBe` Just 0.6

    it "reports the view closest to its ceiling" $
        (mvUsedBytes <$> bindingView (views [(50, 100), (180, 200)])) `shouldBe` Just 180

settleSpec :: Spec
settleSpec = describe "settleGate" $ do
    it "closes an open gate at the close mark" $
        settleGate thresholds GateOpen (Just 0.85) `shouldBe` GateClosed

    it "keeps a closed gate closed between the marks" $
        settleGate thresholds GateClosed (Just 0.8) `shouldBe` GateClosed

    it "keeps an open gate open between the marks" $
        settleGate thresholds GateOpen (Just 0.8) `shouldBe` GateOpen

    it "reopens a closed gate at the reopen mark" $
        settleGate thresholds GateClosed (Just 0.75) `shouldBe` GateOpen

    it "opens without a ceiling" $
        settleGate thresholds GateClosed Nothing `shouldBe` GateOpen

decideSpec :: Spec
decideSpec = describe "decide" $ do
    it "admits below the close mark and records the reservation" $ do
        let (decision, core) = decide thresholds 10 True (measured 50 100)
        decision `shouldSatisfy` isAdmit
        reservedBytes core `shouldBe` 10

    it "judges the first request on measured memory alone" $ do
        -- A reservation wider than the band between the marks would otherwise starve every request.
        let (decision, _) = decide thresholds 1_000 True (measured 50 100)
        decision `shouldSatisfy` isAdmit

    it "waits once reservations would reach the close mark" $ do
        let (_, admittedOnce) = decide thresholds 10 True (measured 70 100)
            (decision, core) = decide thresholds 10 True admittedOnce
        decision `shouldBe` Wait HoldMemory
        coreState core `shouldBe` GateClosed
        reservedBytes core `shouldBe` 10

    it "sheds a memory hold when waiting is not allowed" $
        fst (decide thresholds 10 False (closedAt 90 100)) `shouldBe` Shed HoldMemory

    it "holds heavy work while the brake is engaged, even with room to spare" $ do
        let braked = observe thresholds (Reading (views [(0, 100)]) BrakeEngaged) newGateCore
        fst (decide thresholds 10 True braked) `shouldBe` Wait HoldBrake
        fst (decide thresholds 10 False braked) `shouldBe` Shed HoldBrake

    it "admits anything without a ceiling unless the brake is engaged" $
        fst (decide thresholds maxBound True newGateCore) `shouldSatisfy` isAdmit

    it "releases exactly the admitted reservation" $ do
        let (earlier, afterOne) = decide thresholds 10 True (measured 0 100)
            (_, afterTwo) = decide thresholds 20 True afterOne
        case earlier of
            Admit key -> reservedBytes (release key afterTwo) `shouldBe` 20
            other -> expectationFailure ("expected an admission, got " <> show other)

observeSpec :: Spec
observeSpec = describe "observe" $ do
    it "expires a reservation after its lifetime in samples" $ do
        let (_, admitted) = decide thresholds 10 True (measured 0 100)
            reading = Reading (views [(0, 100)]) BrakeReleased
            samples n = iterate (observe thresholds reading) admitted !!? n
        (reservedBytes <$> samples 5) `shouldBe` Just 10
        (reservedBytes <$> samples 6) `shouldBe` Just 0

    it "makes a late release of an expired reservation a no-op" $ do
        let (decision, admitted) = decide thresholds 10 True (measured 0 100)
            expired = iterate (observe thresholds (Reading (views [(0, 100)]) BrakeReleased)) admitted !!? 6
        case decision of
            Admit key -> (reservedBytes . release key <$> expired) `shouldBe` Just 0
            other -> expectationFailure ("expected an admission, got " <> show other)

    it "reopens a closed gate once measured memory falls to the reopen mark" $
        coreState (observe thresholds (Reading (views [(75, 100)]) BrakeReleased) (closedAt 90 100)) `shouldBe` GateOpen

    it "forces a refresh only for a gate closed on memory with no recent major collection" $ do
        refreshDue 10 GateClosed BrakeReleased 10 `shouldBe` True
        refreshDue 10 GateClosed BrakeReleased 9 `shouldBe` False
        refreshDue 10 GateOpen BrakeReleased 50 `shouldBe` False
        refreshDue 10 GateClosed BrakeEngaged 50 `shouldBe` False

propertySpec :: Spec
propertySpec = describe "properties" $ do
    it "holds its state for any pressure strictly between the marks" $
        hedgehog $ do
            (t, close, reopen) <- forAll genThresholds
            current <- forAll (Gen.element [GateOpen, GateClosed])
            p <- forAll (Gen.double (Range.linearFrac reopen close))
            annotateShow t
            when (p > reopen && p < close) (settleGate t current (Just p) === current)

    it "changes state only by crossing a mark, so a hovering reading cannot flap" $
        hedgehog $ do
            (t, close, reopen) <- forAll genThresholds
            readings <- forAll (Gen.list (Range.linear 1 200) (Gen.double (Range.linearFrac 0 1.2)))
            let states = scanl (settleGate t) GateOpen (map Just readings)
                steps = zip3 states (drop 1 states) readings
            for_ steps $ \(from, to, p) -> case (from, to) of
                (GateOpen, GateClosed) -> assert (p >= close)
                (GateClosed, GateOpen) -> assert (p <= reopen)
                _ -> pass

    it "keeps the reservation equal to what is held, and never negative" $
        hedgehog $ do
            ops <- forAll (Gen.list (Range.linear 1 100) genStep)
            let (core, held) = foldl' (applyStep thresholds) (measured 0 1_000_000_000, []) ops
            assert (reservedBytes core >= 0)
            reservedBytes (foldl' (flip release) core held) === 0

    it "never admits while the brake is engaged" $
        hedgehog $ do
            bytes <- forAll (Gen.int (Range.linear 0 1_000_000))
            mayWait <- forAll Gen.bool
            used <- forAll (Gen.int (Range.linear 0 100))
            let braked = observe thresholds (Reading (views [(used, 100)]) BrakeEngaged) newGateCore
            assert (not (isAdmit (fst (decide thresholds bytes mayWait braked))))

-- One step of a random admission history: admit some bytes, or release one held key, or sample.
data Step = AdmitStep Int | ReleaseStep Int | SampleStep
    deriving stock (Show)

genStep :: Gen Step
genStep = Gen.choice [AdmitStep <$> Gen.int (Range.linear 0 10_000), ReleaseStep <$> Gen.int (Range.linear 0 50), pure SampleStep]

applyStep :: GateThresholds -> (GateCore, [ReservationKey]) -> Step -> (GateCore, [ReservationKey])
applyStep t (core, keys) = \case
    AdmitStep bytes -> case decide t bytes True core of
        (Admit key, core') -> (core', key : keys)
        (_, core') -> (core', keys)
    ReleaseStep index -> case keys !!? index of
        Just key -> (release key core, filter (/= key) keys)
        Nothing -> (core, keys)
    SampleStep -> (observe t (Reading (views [(0, 1_000_000_000)]) BrakeReleased) core, keys)

genThresholds :: Gen (GateThresholds, Double, Double)
genThresholds = do
    close <- Gen.double (Range.linearFrac 0.2 1)
    reopen <- Gen.double (Range.linearFrac 0.05 (close - 0.1))
    ticks <- Gen.int (Range.linear 1 20)
    pure (GateThresholds close reopen ticks, close, reopen)

thresholds :: GateThresholds
thresholds = defaultGateThresholds

views :: [(Int, Int)] -> [MemoryView]
views = mapMaybe (uncurry mkMemoryView)

-- A core that has seen one reading and holds nothing.
measured :: Int -> Int -> GateCore
measured used limit = observe thresholds (Reading (views [(used, limit)]) BrakeReleased) newGateCore

-- A core closed on memory at the given reading.
closedAt :: Int -> Int -> GateCore
closedAt = measured

isAdmit :: Decision -> Bool
isAdmit = \case
    Admit _ -> True
    _ -> False
