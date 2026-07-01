module Ecluse.PilotSpec (spec) where

import Prelude hiding (get)

import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Pilot (pilotApplication)
import Ecluse.Server (mkServerConfig)

spec :: Spec
spec = do
    describe "Pilot worker mode" $ do
        let app = pilotApplication (mkServerConfig [])
        with app $ do
            it "starts up and answers /livez with 200" $
                get "/livez" `shouldRespondWith` 200

            it "answers /readyz with 200" $
                get "/readyz" `shouldRespondWith` 200
