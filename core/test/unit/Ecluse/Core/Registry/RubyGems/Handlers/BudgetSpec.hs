-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.RubyGems.Handlers.BudgetSpec (spec) where

import Test.Hspec
import UnliftIO (concurrently_)

import Ecluse.Core.Registry.RubyGems.Handlers.Budget (newNativeReadBudget)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Core.Server.Pipeline.Origin (ObservedReadBoundExceeded (ObservedReadBoundExceeded))

spec :: Spec
spec = describe "cumulative native upstream bytes" $ do
    it "accepts the exact cap across simultaneous origins and refuses the next byte" $ do
        pay <- newNativeReadBudget 10
        concurrently_ (pay 4) (pay 6)
        pay 1 `shouldThrow` (\(ObservedReadBoundExceeded fault) -> fault == BodyTooLarge (MetadataBodyLimit 10))
    it "retains a failed input in the request total" $ do
        pay <- newNativeReadBudget 1
        pay 2 `shouldThrow` (\(ObservedReadBoundExceeded _) -> True)
        pay 0 `shouldThrow` (\(ObservedReadBoundExceeded _) -> True)
