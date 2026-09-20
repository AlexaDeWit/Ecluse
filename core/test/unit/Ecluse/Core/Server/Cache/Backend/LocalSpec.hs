-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | The local backend's eligibility and zero-capacity behaviour.
module Ecluse.Core.Server.Cache.Backend.LocalSpec (spec) where

import Control.Exception (throw)
import Test.Hspec

import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), Recency (PreserveRecency), supportsFullRetention)
import Ecluse.Core.Server.Cache.Store (SingleFlight, lookupStore, newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Test.Server.Cache (newLocalBackend)

data Weighed = Weighed
    deriving stock (Show)

instance Exception Weighed

spec :: Spec
spec = describe "newLocalBackend" $
    for_ [(0, 100), (100, 0)] $ \(entries, bytes) ->
        it ("serves without weighing at bounds " <> show (entries, bytes)) $ do
            backend <- newLocalBackend 60 entries bytes (\_ -> throw Weighed)
            supportsFullRetention LocalStorage `shouldBe` False
            store <- newSingleFlightWithBackend (Just backend) :: IO (SingleFlight () Text Text)
            resolveSingleFlight (const pass) (const pass) pass store "key" (pure (Right "value")) `shouldReturn` Right "value"
            lookupStore (const pass) pass PreserveRecency store "key" `shouldReturn` Nothing
