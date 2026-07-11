-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.DredgerSpec (spec) where

import Prelude hiding (get)

import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Dredger (dredgerApplication)
import Ecluse.Runtime.Server (mkServerConfig)

spec :: Spec
spec = do
    describe "Dredger worker mode" $ do
        let app = dredgerApplication (mkServerConfig [])
        with app $ do
            it "starts up and answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200
