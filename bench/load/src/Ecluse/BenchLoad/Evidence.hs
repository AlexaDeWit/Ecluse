-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load observations preserve floor enforcement without changing the run's verdict.
module Ecluse.BenchLoad.Evidence (loadEvidence) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchLoad.Floors (Enforcement (Enforced, NotHeld), FloorKey, floorCheck, passKey)
import Ecluse.BenchLoad.Verdict (RunEvidence, runViolations)

-- | Record actual enforcement, including every reason a diagnostic run is not held.
loadEvidence :: Map Text Text -> Enforcement -> [Text] -> [(FloorKey, Either Text RunEvidence)] -> Value
loadEvidence conditions enforcement violations outcomes =
    object
        [ "outcome" .= verdict (null violations)
        , "conditions" .= Map.union floorConditions conditions
        , "observations"
            .= [ object
                    [ "id" .= (scenario <> "/" <> passKey whichPass)
                    , "outcome" .= verdict (either (const False) (null . runViolations (floorCheck enforcement key)) result)
                    ]
               | (key@(scenario, whichPass), result) <- outcomes
               ]
        ]
  where
    floorConditions = case enforcement of
        Enforced _ _ -> Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")]
        NotHeld reasons -> Map.fromList [("floorEnforcement", "not-held"), ("floorEnforcementReasons", T.intercalate ", " (map show (toList reasons)))]
    verdict passed = if passed then ("passed" :: Text) else "failed"
