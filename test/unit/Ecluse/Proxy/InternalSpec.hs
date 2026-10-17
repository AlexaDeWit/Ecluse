-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | What the proxy listener's Warp hook logs for a fault that escapes to the server.
module Ecluse.Proxy.InternalSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Types (status503)
import Test.Hspec

import Ecluse.Proxy.Internal (warpExceptionHook)
import Ecluse.Test.Log (captureJsonLog)
import Ecluse.Test.Stub (redirectSecrets, redirectedStatusException)

spec :: Spec
spec = describe "warpExceptionHook" $
    it "logs an escaped HTTP fault by its status at the dialled host:port, never the request or the response" $ do
        (authority, err) <- redirectedStatusException status503
        (_, logged) <- captureJsonLog (\logEnv -> warpExceptionHook logEnv Nothing (toException err))
        logged `shouldSatisfy` T.isInfixOf (authority <> ": HTTP 503")
        for_ redirectSecrets $ \secret -> logged `shouldSatisfy` (not . T.isInfixOf secret)
