-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Dredger.PlanSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.Support (expectConfig, staticEnvVars)
import Ecluse.Composition.Types (BootRole (BootStorePreview, BootStorePruner))
import Ecluse.Config (Config (configApp))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt (HaltDeletionCap, HaltStoreFault),
    CycleOutcome (CycleOutcome, outcomeEvidence, outcomeHalt, outcomePrerequisites, outcomeTally),
    PrerequisiteStatus (PrerequisiteMet, PrerequisiteUnmet),
    SweepPacing (swpChunkPause, swpChunkSize, swpCyclePause, swpDeletionCap, swpShape),
    SweepShape (SweepCandidates, SweepEverything),
    TargetPrerequisites (TargetPrerequisites),
    deletionCapPerStore,
    unreadManifest,
 )
import Ecluse.Dredger.Plan (
    SweepMode (SweepDeletes, SweepPreviews),
    cycleEnding,
    dredgerBootRole,
    haltDetail,
    sweepPacingFor,
 )

spec :: Spec
spec = do
    pacingSpec
    capSpec
    shapeSpec
    haltSpec
    roleSpec
    endingSpec

pacingSpec :: Spec
pacingSpec = describe "sweepPacingFor" $ do
    it "carries every shipped default off the dredger group" $ do
        pacing <- pacingUnder []
        (swpChunkSize pacing, swpChunkPause pacing, swpCyclePause pacing) `shouldBe` (50, 2, 3600)

    it "carries an operator's own pacing" $ do
        pacing <- pacingUnder [("ECLUSE_DREDGER__CHUNK_SIZE", "25")]
        swpChunkSize pacing `shouldBe` 25

{- The cap bounds one cycle, and one cycle covers every store in turn, so an unset key is computed
per store rather than pinned to a total the later mounts would never reach. -}
capSpec :: Spec
capSpec = describe "the per-cycle deletion cap" $ do
    it "computes its default from the stores one cycle sweeps" $ do
        (pacing, line) <- resolvedOver 3 []
        swpDeletionCap pacing `shouldBe` 3 * deletionCapPerStore
        line `shouldSatisfy` T.isInfixOf "computed as"

    it "takes an operator's own cap over the computed one" $ do
        (pacing, line) <- resolvedOver 3 [("ECLUSE_DREDGER__DELETION_CAP", "10")]
        swpDeletionCap pacing `shouldBe` 10
        line `shouldSatisfy` T.isInfixOf "from config"

{- The full walk is opt-in, and while it is on it replaces the candidate cycle rather than running
beside it, because a walk is a superset of a candidate cycle. -}
shapeSpec :: Spec
shapeSpec = describe "the cycle's shape" $ do
    it "runs the candidate cycle by default" $ do
        pacing <- pacingUnder []
        swpShape pacing `shouldBe` SweepCandidates

    it "runs the full walk when the operator turns it on" $ do
        pacing <- pacingUnder [("ECLUSE_DREDGER__FULL_WALK", "true")]
        swpShape pacing `shouldBe` SweepEverything

{- A one-shot run reports why it halted on the ending itself, so a scheduler reads the outcome from
the exit status and the reason from the same line. -}
haltSpec :: Spec
haltSpec = describe "haltDetail" $ do
    it "carries the halt's own text onto the ending a one-shot run exits with" $ do
        let detail = haltDetail (HaltDeletionCap 10 10 Nothing)
        detail `shouldSatisfy` T.isInfixOf "the mirror sweep cycle halted"
        detail `shouldSatisfy` T.isInfixOf "deletion cap of 10"

{- The invocation settles the role before the boot vets anything, so the pass runs for the
authority this process will hold rather than for the one its flags asked for. -}
roleSpec :: Spec
roleSpec = describe "dredgerBootRole" $ do
    it "boots the deleting role for a run that deletes" $
        dredgerBootRole SweepDeletes `shouldBe` BootStorePruner

    it "boots the preview role for a dry run" $
        dredgerBootRole SweepPreviews `shouldBe` BootStorePreview

{- The preview's status follows completeness alone. An unmet prerequisite is reported and leaves
the status clean, and only evidence the cycle could not read makes it non-zero. -}
endingSpec :: Spec
endingSpec = describe "cycleEnding" $ do
    it "ends a run that deletes on the halt its own cycle raised" $
        cycleEnding SweepDeletes cappedOutcome
            `shouldSatisfy` maybe False (T.isInfixOf "deletion cap of 10")

    it "ends a complete preview cleanly, though a prerequisite it reported is unmet" $
        cycleEnding SweepPreviews completePreview `shouldBe` Nothing

    it "ends an incomplete preview on what it could not read" $ do
        let ending = cycleEnding SweepPreviews completePreview{outcomeEvidence = unreadManifest}
        ending `shouldSatisfy` maybe False (T.isInfixOf "counted from part of the store")
        ending `shouldSatisfy` maybe False (T.isInfixOf "1 package decided without the store's own metadata")

    it "ends a halted preview on the halt that stopped its enumeration" $
        cycleEnding SweepPreviews completePreview{outcomeHalt = Just storeFault}
            `shouldSatisfy` maybe False (T.isInfixOf "produced no answer")
  where
    cappedOutcome = completePreview{outcomeHalt = Just (HaltDeletionCap 10 10 Nothing)}
    storeFault = HaltStoreFault Npm "codeArtifact" "the peer did not answer in time"

{- A preview that read the whole store and still found the operator's consent absent, which is
the case the exit policy turns on. -}
completePreview :: CycleOutcome
completePreview =
    CycleOutcome
        { outcomeHalt = Nothing
        , outcomeTally = mempty
        , outcomePrerequisites = [TargetPrerequisites Npm "codeArtifact" (PrerequisiteUnmet "attach it") PrerequisiteMet]
        , outcomeEvidence = mempty
        }

-- The shipped defaults over one sweepable store, plus whatever the case layers over them.
pacingUnder :: [(String, String)] -> IO SweepPacing
pacingUnder = fmap fst . resolvedOver 1

-- The pacing and the boot line naming where the cap came from, over a chosen store count.
resolvedOver :: Int -> [(String, String)] -> IO (SweepPacing, Text)
resolvedOver stores overrides =
    flip sweepPacingFor stores . configApp <$> expectConfig (staticEnvVars <> overrides) Nothing
