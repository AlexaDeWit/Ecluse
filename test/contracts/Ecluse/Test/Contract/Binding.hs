-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Check labelled Hspec registrations against independently declared coverage.
module Ecluse.Test.Contract.Binding (labelledCaseIds) where

import Test.Hspec (SpecWith)
import Test.Hspec.Core.Runner (defaultConfig, evalSpec)
import Test.Hspec.Core.Spec (Item (itemRequirement))

import Ecluse.Test.Contract.Catalogue (labelCaseId)

-- | Read labels without running examples or hooks. Specifications must not acquire resources through runIO.
labelledCaseIds :: SpecWith a -> IO [Text]
labelledCaseIds spec = do
    (_, forest) <- evalSpec defaultConfig spec
    pure $ concatMap (mapMaybe (labelCaseId . itemRequirement) . toList) forest
