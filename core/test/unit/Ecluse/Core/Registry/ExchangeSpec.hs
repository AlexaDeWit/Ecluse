-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A lost response on a reused connection must not replay a destructive request.
module Ecluse.Core.Registry.ExchangeSpec (spec) where

import Network.HTTP.Client (Request (method), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, responseRaw)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Registry.Exchange (singleAttemptSettings)

spec :: Spec
spec = describe "singleAttemptSettings" $
    it "returns a lost destructive response without resending on a fresh connection" $ do
        requests <- newIORef (0 :: Int)
        let application _ respond = do
                attempt <- atomicModifyIORef' requests (\n -> (n + 1, n + 1))
                respond $
                    if attempt == 1
                        then responseLBS status200 [] "warm connection"
                        else responseRaw (\_ _ -> pure ()) (responseLBS status200 [] "")
        testWithApplication (pure application) $ \port -> do
            manager <- newManager (singleAttemptSettings defaultManagerSettings)
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            void (httpLbs request manager)
            result <- tryAny (httpLbs request{method = "DELETE"} manager)
            result `shouldSatisfy` isLeft
            readIORef requests `shouldReturn` 2
