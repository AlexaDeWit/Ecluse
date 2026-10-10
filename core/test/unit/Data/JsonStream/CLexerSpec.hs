-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Recorded C lexer outputs remain independent of the replacement scanner.
module Data.JsonStream.CLexerSpec (spec) where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.JsonStream.CLexer (tokenParser)
import Data.JsonStream.TokenParser (Element (..), TokenResult (..))
import Test.Hspec

import Ecluse.Test.Package (hexSha256Of)

data Reference = Reference Text [ByteString] [Aeson.Value]

instance Aeson.FromJSON Reference where
    parseJSON = Aeson.withObject "C lexer reference" $ \object ->
        Reference <$> object .: "name" <*> (map BS.pack <$> object .: "chunks") <*> object .: "events"

-- | Check token values, chunk waits, failures, ASCII flags and leftover input.
spec :: Spec
spec = describe "C lexer compatibility" $
    it "matches the recorded C scanner for every directed input and chunk cut" $ do
        bytes <- readFileBS "core/test/unit/fixtures/json-stream/c-lexer-events.json"
        case Aeson.eitherDecodeStrict bytes :: Either String [Reference] of
            Left failure -> expectationFailure failure
            Right references -> do
                length references `shouldBe` referenceCount
                for_ references $ \(Reference name chunks expected) ->
                    (name, traceTokens chunks) `shouldBe` (name, expected)

traceTokens :: [ByteString] -> [Aeson.Value]
traceTokens = go (tokenParser BS.empty)
  where
    go tokens chunks = case tokens of
        PartialResult element rest -> tokenEvent element : go rest chunks
        TokFailed -> [Aeson.String "Failure"]
        TokMoreData more ->
            Aeson.String "Wait" : case chunks of
                [] -> []
                piece : rest -> go (more piece) rest

tokenEvent :: Element -> Aeson.Value
tokenEvent = \case
    ArrayBegin -> tagged "ArrayBegin" []
    ObjectBegin -> tagged "ObjectBegin" []
    ArrayEnd rest -> tagged "ArrayEnd" ["context" .= payload rest]
    ObjectEnd rest -> tagged "ObjectEnd" ["context" .= payload rest]
    StringEnd rest -> tagged "StringEnd" ["context" .= payload rest]
    StringRaw bytes ascii rest -> tagged "StringRaw" ["payload" .= payload bytes, "ascii" .= ascii, "context" .= payload rest]
    StringContent bytes -> tagged "StringContent" ["payload" .= payload bytes]
    JInteger number -> tagged "JInteger" ["integer" .= (show number :: Text)]
    JValue value -> tagged "JValue" ["value" .= (show value :: Text)]
  where
    tagged name fields = Aeson.object ("token" .= (name :: Text) : fields)

payload :: ByteString -> Aeson.Value
payload bytes = Aeson.object ["length" .= BS.length bytes, "sha256" .= hexSha256Of bytes]

referenceCount :: Int
referenceCount = 2914
