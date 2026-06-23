module Ecluse.Registry.Npm.ServeSpec (spec) where

import Data.Aeson (Value (String), eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec

import Ecluse.Registry.Npm.Serve (npmDenialBody, npmRenderer)
import Ecluse.Server.Response (RenderedBody (..), mkHelpMessage, renderError)

{- | Decode a denial body and read its @error@ string. 'Right' the string when the
body is a JSON object carrying a string @error@; 'Left' (which fails the
@`shouldBe` Right …@ assertion) for any other shape, so the npm @{"error": …}@
contract is pinned without a partial decode.
-}
errorField :: LByteString -> Either Text Text
errorField raw =
    case eitherDecode raw of
        Right (Aeson.Object o) ->
            case KeyMap.lookup "error" o of
                Just (String msg) -> Right msg
                _ -> Left "denial body has no string \"error\" field"
        _ -> Left "denial body is not a JSON object"

spec :: Spec
spec = do
    describe "npmDenialBody — the npm {\"error\": …} shape" $ do
        it "is a JSON object with a string error field carrying the message" $
            errorField (npmDenialBody Nothing "denied because reasons")
                `shouldBe` Right "denied because reasons"
        it "appends a configured help message to the error text" $
            errorField (npmDenialBody (Just (mkHelpMessage "Contact #platform-eng.")) "denied")
                `shouldBe` Right "denied Contact #platform-eng."
        it "appends nothing when no help message is configured" $
            errorField (npmDenialBody Nothing "denied")
                `shouldBe` Right "denied"
        it "does not duplicate spacing when the message already ends in a space" $
            errorField (npmDenialBody (Just (mkHelpMessage "Help.")) "denied ")
                `shouldBe` Right "denied Help."
        it "ignores a blank help message rather than appending empty text" $
            errorField (npmDenialBody (Just (mkHelpMessage "   ")) "denied")
                `shouldBe` Right "denied"

    describe "npmRenderer — the npm mount renderer" $ do
        it "tags the rendered body application/json" $
            renderedContentType (renderError npmRenderer Nothing "denied")
                `shouldBe` "application/json"
        it "renders the full content-type + bytes pair" $
            renderError npmRenderer Nothing "denied"
                `shouldBe` RenderedBody "application/json" "{\"error\":\"denied\"}"
        it "shapes the body as the npm {\"error\": …} object" $
            errorField (renderedBytes (renderError npmRenderer Nothing "denied"))
                `shouldBe` Right "denied"
        it "appends the operator help message through the renderer" $
            errorField (renderedBytes (renderError npmRenderer (Just (mkHelpMessage "Ask #eng.")) "denied"))
                `shouldBe` Right "denied Ask #eng."
