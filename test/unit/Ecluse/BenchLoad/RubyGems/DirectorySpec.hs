-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Small synthetic directories exercise the independent journal reader and exact fixture bytes.
module Ecluse.BenchLoad.RubyGems.DirectorySpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Fixture (fetchChecked)
import Ecluse.BenchLoad.RubyGems.Directory (directoryEntries, directoryNames, directoryStub)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = do
    describe "directoryNames" $ do
        it "retains every reconciled native version and platform token" $
            directoryEntries "---\nalpha 1.0.0,1.0.0-java a\nalpha -1.0.0,1.1.0 \n"
                `shouldBe` Right (Map.singleton "alpha" (Set.fromList ["1.0.0-java", "1.1.0"]))
        it "reconciles repeated additions and removes empty histories" $
            directoryNames "created_at: now\n---\nalpha 1.0.0 a\nbeta 2.0.0 b\nalpha -1.0.0,1.1.0 c\nbeta -2.0.0 \n"
                `shouldBe` Right (Set.singleton "alpha")
        it "retains blank native cache hints and an empty authorised directory" $ do
            directoryNames "---\nalpha 1.0.0 \n" `shouldBe` Right (Set.singleton "alpha")
            directoryNames "---\n" `shouldBe` Right Set.empty
        it "refuses missing delimiters and incomplete rows" $
            for_ ["alpha 1.0.0 a\n", "---\nalpha 1.0.0\n", "---\nalpha - a\n", "---\nalpha 1.0.0,,2.0.0 a\n"] $ \body ->
                directoryNames body `shouldSatisfy` isLeft
    describe "directoryStub" $
        it "serves exact synthetic bytes and refuses unrelated native reads" $ do
            let body = "---\nalpha 1.0.0 \n"
            testWithApplication (pure (directoryStub 0 body)) $ \port -> do
                response <- fetchChecked status200 [] (localhost port <> "/versions")
                HTTP.responseBody response `shouldBe` fromStrict body
                void (fetchChecked status404 [] (localhost port <> "/info/alpha"))
