-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.StrictSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldThrow)
import UnliftIO.Exception (evaluate, impureThrow)

import Ecluse.Core.Strict (strictElements)

spec :: Spec
spec = describe "strictElements" $
    it "evaluates every element when the container is evaluated" $ do
        let elements = [1, 2, impureThrow LaterElement] :: [Int]
        void (evaluate elements)
        evaluate (strictElements elements) `shouldThrow` (== LaterElement)

-- Thrown by an element that only element-wise evaluation reaches.
data LaterElement = LaterElement
    deriving stock (Eq, Show)

instance Exception LaterElement
