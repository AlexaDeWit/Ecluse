-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Machine-readable native outcomes, constructed after allocation counters stop.
module Ecluse.Acceptance.Evidence (capturesEvidence, liveEvidence) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import System.Exit (ExitCode (ExitSuccess))

import Ecluse.Acceptance
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

-- | Preserve the native budget verdict for every actual assessed row.
capturesEvidence :: OperatingPoint -> CapturesReport -> Value
capturesEvidence op report = evidence "captures" op outcome rows
  where
    outcome = if capturesExitCode report == ExitSuccess then "passed" else "failed"
    rows =
        [ case row of
            Assessed leg -> observation (prefix <> alPackage leg <> "/" <> legKey (alLeg leg)) (if alVerdict leg == Within then "passed" else "failed")
            FailedPackage name _ -> observation (prefix <> name) "failed"
        | section <- reportSections report
        , let prefix = ecosystemName (sectionEcosystem section) <> "/"
        , row <- sectionRows section
        ]

-- | Unavailable live documents remain unavailable even when the native process exits zero.
liveEvidence :: OperatingPoint -> [(Ecosystem, [PackageOutcome])] -> Value
liveEvidence op runs = evidence "live" op outcome rows
  where
    outcomes = concatMap snd runs
    outcome
        | any failed outcomes = "failed"
        | any unavailable outcomes = "unavailable"
        | otherwise = "passed"
    failed = \case
        Failed{} -> True
        _ -> False
    unavailable = \case
        Unavailable{} -> True
        _ -> False
    rows = concat [observations (ecosystemName eco <> "/") item | (eco, items) <- runs, item <- items]
    observations prefix = \case
        Measured sample -> [observation (prefix <> sampleName sample <> "/" <> legKey leg) "passed" | (leg, _) <- sampleLegs sample]
        Failed name _ -> [observation (prefix <> name) "failed"]
        Unavailable name _ -> [observation (prefix <> name) "unavailable"]

evidence :: Text -> OperatingPoint -> Text -> [Value] -> Value
evidence mode op outcome rows =
    object
        [ "outcome" .= outcome
        , "observations" .= rows
        , "conditions"
            .= Map.fromList
                [ ("mode" :: Text, mode)
                , ("passes", show (opPasses op))
                , ("capabilities", show (opCapabilities op))
                , ("allocationAreaBytes", show (opAllocationAreaBytes op))
                ]
        ]

observation :: Text -> Text -> Value
observation identifier outcome = object ["id" .= identifier, "outcome" .= outcome]
