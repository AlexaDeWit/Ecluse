-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Check labelled Hspec registrations against independently declared coverage.
module Ecluse.Test.Contract.Binding (labelledCaseIds, allCaseIds) where

import Test.Hspec (SpecWith)
import Test.Hspec.Core.Runner (defaultConfig, evalSpec)
import Test.Hspec.Core.Spec (Item (itemRequirement))

import Ecluse.Test.Contract.Catalogue (labelCaseId)

-- | Inspect declarations, retaining malformed labels. Specifications must not acquire resources through runIO.
labelledCaseIds :: SpecWith a -> IO [Text]
labelledCaseIds spec = mapMaybe registeredId <$> caseLabels spec
  where
    registeredId label = case labelCaseId label of
        Just identifier -> Just identifier
        Nothing -> case label of
            '[' : _ -> Just ("malformed:" <> toText label)
            _ -> Nothing

-- | Fixed suites require a stable ID on every example, including native assertions.
allCaseIds :: SpecWith a -> IO [Text]
allCaseIds spec = map (\label -> fromMaybe ("unregistered:" <> toText label) $ labelCaseId label) <$> caseLabels spec

caseLabels :: SpecWith a -> IO [String]
caseLabels spec = do
    (_, forest) <- evalSpec defaultConfig spec
    pure $ concatMap (map itemRequirement . toList) forest
