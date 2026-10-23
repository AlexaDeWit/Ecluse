-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed public cooldown policy over recorded HTTP attempts.
module Ecluse.Core.Registry.PublicSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager, parseRequest)
import Network.HTTP.Types (status200, status429, status503)
import Test.Hspec

import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Registry (BodyOutcome (SuccessBody), FetchFault (FetchCooldown))
import Ecluse.Core.Registry.Backoff (CooldownEvent (CoolingDown, Recovered), PublicResponse (..))
import Ecluse.Core.Registry.Exchange (withSuccessBodyUsing)
import Ecluse.Core.Registry.Public (ReadControl (Controlled), classifyPublicResponse)
import Ecluse.Core.Security (Limits (progressFloor), defaultLimits)
import Ecluse.Core.Telemetry.Upstream (ExchangeOutcome (ResponseStatus), PublicReadKind (Listing))
import Ecluse.Test.Deadline (newSweptClient)
import Ecluse.Test.Registry.Public (PublicFixture (..), newPublicFixture)
import Ecluse.Test.Stub (allCaptured, stubBaseUrl, withRoutedStubIO)

spec :: Spec
spec = do
    describe "classifyPublicResponse" $ do
        it "uses fallback only for a 429 with an unusable hint" $
            for_ [[], [("Retry-After", "0")], [("Retry-After", "bad")], [("Retry-After", "Sat, 17 Oct 2026 00:00:00 GMT")]] $ \headers -> do
                classifyPublicResponse now status429 headers `shouldBe` Throttled Nothing
                classifyPublicResponse now status503 headers `shouldBe` NonThrottling
        it "caps numeric and date hints and accepts a hinted 503" $
            for_ [status429, status503] $ \status -> do
                classifyPublicResponse now status [("Retry-After", "999999")] `shouldBe` Throttled (Just (RetryAfter 600))
                classifyPublicResponse now status [("Retry-After", "Sun, 18 Oct 2026 00:20:00 GMT")] `shouldBe` Throttled (Just (RetryAfter 600))
        it "does not apply a retry hint to a non-throttling response" $
            classifyPublicResponse now status200 [("Retry-After", "60")] `shouldBe` NonThrottling

    it "counts the first 429, suppresses local retries, and observes recovery" $ do
        response <- newIORef (status429, [("Retry-After", "60")], "busy")
        withRoutedStubIO (const (readIORef response)) $ \stub -> do
            fixture <- newPublicFixture
            exchanges <- newSweptClient =<< newManager defaultManagerSettings
            request <- parseRequest (toString (stubBaseUrl stub))
            let fetch = withSuccessBodyUsing (Controlled (pfControl fixture) Listing) exchanges (progressFloor defaultLimits) (const (pure (Right ()))) request
            fetch `shouldReturn` Left (FetchCooldown (RetryAfter 60))
            pfAdvance fixture 10
            fetch `shouldReturn` Left (FetchCooldown (RetryAfter 50))
            length <$> allCaptured stub `shouldReturn` 1
            pfRequests fixture `shouldReturn` [("127.0.0.1", Listing, ResponseStatus 429)]
            writeIORef response (status200, [], "ok")
            pfAdvance fixture 50
            fetch `shouldReturn` Right (SuccessBody 200 ())
            length <$> allCaptured stub `shouldReturn` 2
            map snd <$> pfTransitions fixture `shouldReturn` [CoolingDown, Recovered]
            pfRequests fixture `shouldReturn` [("127.0.0.1", Listing, ResponseStatus 429), ("127.0.0.1", Listing, ResponseStatus 200)]
  where
    now = UTCTime (fromGregorian 2026 10 18) 0
