-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Native load observations: the run's enforcement and, for each scenario pass, its verdict, the count
of every attempt and whether it ran clean, and its cell's floor. A calibration reads the first attempts.
-}
module Ecluse.BenchLoad.Evidence (loadEvidence) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchLoad.Floors (Enforcement (Enforced, NotHeld), FloorCheck (AtLeast, NoFloor, ReportOnly, Unchecked), FloorKey, floorCheck, passKey)
import Ecluse.BenchLoad.Verdict (Attempts, attemptClean, attemptCount, passViolations)

-- | Record actual enforcement, including every reason a diagnostic run is not held.
loadEvidence :: Map Text Text -> Enforcement -> [Text] -> [(FloorKey, Attempts)] -> Value
loadEvidence conditions enforcement violations passes =
    object
        [ "outcome" .= verdict (null violations)
        , "conditions" .= Map.union floorConditions conditions
        , "observations"
            .= [ object $
                    [ "id" .= (scenario <> "/" <> passKey whichPass)
                    , "outcome" .= verdict (null (passViolations key check attempts))
                    , "attempts" .= [object ["successes" .= attemptCount attempt, "clean" .= attemptClean attempt] | attempt <- toList attempts]
                    ]
                        <> cellFields check
               | (key@(scenario, whichPass), attempts) <- passes
               , let check = floorCheck enforcement key
               ]
        ]
  where
    floorConditions = case enforcement of
        Enforced{} -> Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")]
        NotHeld reasons -> Map.fromList [("floorEnforcement", "not-held"), ("floorEnforcementReasons", T.intercalate ", " (map show (toList reasons)))]
    verdict passed = if passed then ("passed" :: Text) else "failed"

-- The floor in force for the pass, or why its cell holds none.
cellFields :: FloorCheck -> [Pair]
cellFields = \case
    AtLeast least _ -> ["floor" .= least]
    ReportOnly why -> ["floor" .= (Nothing :: Maybe Int), "reportOnly" .= fromMaybe "uncalibrated" why]
    NoFloor _ -> []
    Unchecked -> []
