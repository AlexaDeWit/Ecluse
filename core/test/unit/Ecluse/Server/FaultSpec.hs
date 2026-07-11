module Ecluse.Server.FaultSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry (RegistryUnconfigured (RegistryUnconfigured))
import Ecluse.Core.Registry.Npm (ResponseBoundExceeded (ResponseBoundExceeded))
import Ecluse.Core.Security (LimitError (BodyTooLarge))
import Ecluse.Core.Server.Fault (
    RenderEscape (RenderEscape),
    RequestFault (rqCause, rqDetail),
    classifyEscape,
 )
import Ecluse.Core.Telemetry.Metrics (
    RequestFaultCause (GateFault, RenderFault, UnclassifiedFault),
 )

-- | A typed stand-in for an escape nothing classifies.
newtype UnknownEscape = UnknownEscape Text
    deriving stock (Eq, Show)

instance Exception UnknownEscape

spec :: Spec
spec = describe "classifyEscape (the request perimeter's vocabulary)" $ do
    it "classifies the unwired-handle fault as a GateFault" $
        rqCause (classifyEscape (toException RegistryUnconfigured)) `shouldBe` GateFault

    it "classifies a response-bound breach as a GateFault" $
        rqCause (classifyEscape (toException (ResponseBoundExceeded (BodyTooLarge 1024)))) `shouldBe` GateFault

    it "classifies the render marker as a RenderFault carrying the inner escape's detail" $ do
        let fault = classifyEscape (toException (RenderEscape (toException (UnknownEscape "assembly bottomed"))))
        rqCause fault `shouldBe` RenderFault
        -- The detail names the escape the render wrapped, not the wrapper.
        rqDetail fault `shouldSatisfy` T.isInfixOf "assembly bottomed"

    it "classifies anything unrecognised as UnclassifiedFault with its rendering carried" $ do
        let fault = classifyEscape (toException (UnknownEscape "who goes there"))
        rqCause fault `shouldBe` UnclassifiedFault
        rqDetail fault `shouldSatisfy` T.isInfixOf "who goes there"

    it "bounds the carried detail to the shared log-line budget" $ do
        let fault = classifyEscape (toException (UnknownEscape (T.replicate 10_000 "x")))
        T.length (rqDetail fault) `shouldSatisfy` (<= 512)
