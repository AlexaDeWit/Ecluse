-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Every registry response opens through the client's tracing, once per open.
module Ecluse.Core.Registry.ResponseWaitSpec (spec) where

import Network.HTTP.Client (Request, brConsume, defaultManagerSettings, newManager, parseRequest, responseBody, responseClose)
import Network.HTTP.Types (status200)
import Test.Hspec

import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecTrace), TraceExchange)
import Ecluse.Core.Registry.ResponseWait (openResponse, withResponse)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

spec :: Spec
spec = describe "the client's tracing" $ do
    it "runs once for each openResponse" $
        tracedOpens 3 $
            \client request -> openResponse client request >>= responseClose

    it "runs once for each withResponse" $
        tracedOpens 3 $
            \client request -> withResponse client request (void . brConsume . responseBody)

-- Open this many responses through a client whose tracing counts its runs, and expect one run for each.
tracedOpens :: Int -> (ExchangeClient -> Request -> IO ()) -> Expectation
tracedOpens opens exchange = do
    runs <- newIORef (0 :: Int)
    withStub status200 "ok" $ \stub -> withExchangeClient (newManager defaultManagerSettings) $ \client -> do
        request <- parseRequest (toString (stubBaseUrl stub))
        replicateM_ opens (exchange client{ecTrace = counting runs} request)
    readIORef runs `shouldReturn` opens

counting :: IORef Int -> TraceExchange
counting runs request open = modifyIORef' runs (+ 1) >> open request
