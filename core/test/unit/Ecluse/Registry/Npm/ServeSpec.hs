-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.Npm.ServeSpec (spec) where

import Data.Aeson (Value (String), eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec

import Ecluse.Core.Registry.Npm.Serve (npmError, npmErrorCodec)
import Ecluse.Core.Server.Contract (encodeBody)
import Ecluse.Core.Server.Response (HelpMessage, mkHelpMessage)

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

-- The npm denial body: an 'NpmError' (with any operator help appended) encoded through
-- 'npmErrorCodec' -- the same codec the manifest documents, so wire and schema are one.
denialBody :: Maybe HelpMessage -> Text -> LByteString
denialBody help message = encodeBody npmErrorCodec (npmError help message)

spec :: Spec
spec = do
    describe "the npm denial body -- the {\"error\": …} codec" $ do
        it "is a JSON object with a string error field carrying the message" $
            errorField (denialBody Nothing "denied because reasons")
                `shouldBe` Right "denied because reasons"
        it "renders exactly the npm {\"error\": …} object" $
            denialBody Nothing "denied" `shouldBe` "{\"error\":\"denied\"}"
        it "appends a configured help message to the error text" $
            errorField (denialBody (Just (mkHelpMessage "Contact #platform-eng.")) "denied")
                `shouldBe` Right "denied Contact #platform-eng."
        it "appends nothing when no help message is configured" $
            errorField (denialBody Nothing "denied")
                `shouldBe` Right "denied"
        it "does not duplicate spacing when the message already ends in a space" $
            errorField (denialBody (Just (mkHelpMessage "Help.")) "denied ")
                `shouldBe` Right "denied Help."
        it "ignores a blank help message rather than appending empty text" $
            errorField (denialBody (Just (mkHelpMessage "   ")) "denied")
                `shouldBe` Right "denied"
