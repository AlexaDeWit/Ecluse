-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Share Cabal plan parsing and explicit field projection between temporary diagnostics.
module Ecluse.BenchReport.BuildPlan (planUnits, projectPlanUnits) where

import Data.Aeson (Value (Object), toJSON, withObject, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)

-- | Require the Cabal install-plan array before checking component identities.
planUnits :: Value -> Either String [Value]
planUnits = parseEither (withObject "plan" (.: "install-plan"))

-- | Each caller names the excluded packages and fields its comparison permits.
projectPlanUnits :: [Text] -> [Key] -> [Value] -> [Value]
projectPlanUnits packages fields units =
    [Object (foldr KeyMap.delete unit fields) | Object unit <- units, KeyMap.lookup "pkg-name" unit `notElem` map (Just . toJSON) packages]
