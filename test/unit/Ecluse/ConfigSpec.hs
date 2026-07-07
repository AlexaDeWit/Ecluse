{-# LANGUAGE OverloadedStrings #-}

module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Config (RulePolicy (..), renderConfigError, validateDefaultConfig)

spec :: Spec
spec = describe "the embedded default configuration" $ do
    it "is a valid, self-contained backbone (decodes, parses into AppConfig, resolves its policy)" $
        validateDefaultConfig `shouldSatisfy` isRight

    it "ships exactly the expected baseline rules under their default names" $
        case validateDefaultConfig of
            Right (_, RulePolicy rules) ->
                Map.keys rules `shouldMatchList` ["min-age", "remediation-fast-track"]
            Left errs ->
                expectationFailure
                    ("the default configuration did not validate: " <> show (map renderConfigError errs))
