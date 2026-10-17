-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.FaultSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Types.Status (status503)
import Test.Hspec

import Ecluse.Core.Server.Fault (
    RenderEscape (RenderEscape),
    RequestFault (rqCause, rqDetail),
    classifyEscape,
 )
import Ecluse.Core.Telemetry.Metrics (
    RequestFaultCause (RenderFault, UnclassifiedFault),
 )
import Ecluse.Test.Stub (redirectedStatusException)

-- | A typed stand-in for an escape nothing classifies.
newtype UnknownEscape = UnknownEscape Text
    deriving stock (Eq, Show)

instance Exception UnknownEscape

spec :: Spec
spec = describe "classifyEscape (the request perimeter's vocabulary)" $ do
    it "classifies the render marker as a RenderFault carrying the inner escape's detail" $ do
        let fault = classifyEscape (toException (RenderEscape (toException (UnknownEscape "assembly bottomed"))))
        rqCause fault `shouldBe` RenderFault
        -- The detail names the escape the render wrapped, not the wrapper.
        rqDetail fault `shouldSatisfy` T.isInfixOf "assembly bottomed"

    it "classifies anything unrecognised as UnclassifiedFault with its rendering carried" $ do
        let fault = classifyEscape (toException (UnknownEscape "who goes there"))
        rqCause fault `shouldBe` UnclassifiedFault
        rqDetail fault `shouldSatisfy` T.isInfixOf "who goes there"

    it "renders an HTTP escape by its status at the dialled host:port, never the request or the response" $ do
        (authority, err) <- redirectedStatusException status503
        rqDetail (classifyEscape (toException err)) `shouldBe` authority <> ": HTTP 503"

    it "bounds the carried detail to the shared log-line budget" $ do
        let fault = classifyEscape (toException (UnknownEscape (T.replicate 10_000 "x")))
        T.length (rqDetail fault) `shouldSatisfy` (<= 512)
