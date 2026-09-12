-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Contracts for configuration paths and key refusal precedence.
module Ecluse.Config.ParserSpec (spec) where

import Data.Aeson (Value (..), object, withObject, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseEither)
import Data.List (isInfixOf)
import Test.Hspec

import Ecluse.Config.Parser

-- | Direct decoder coverage independent of the configuration defaults.
spec :: Spec
spec = do
    describe "typed key paths" $ do
        forM_
            [ ("required", void (requiredKey "port" parsePort))
            , ("optional", void (optionalKey "port" parsePort))
            , ("defaulted", void (optionalKeyOr "port" 4873 parsePort))
            , ("plain", void (plainKey "port" :: GroupDecoder Int))
            , ("optional plain", void (optionalPlainKey "port" :: GroupDecoder (Maybe Int)))
            , ("defaulted plain", void (optionalPlainKeyOr "port" (4873 :: Int)))
            ]
            $ \(name, decoder) ->
                it (name <> " reads name the group and key on a wrong type") $
                    first (isInfixOf "$.server.port") (decodeServer decoder (object ["server" .= object ["port" .= String "bad"]]))
                        `shouldBe` Left True

        it "retains endpoint and tag labels on typed reads and duration refinements"
            $ forM_
                [ ("count", void (requiredKey "count" parsePort))
                , ("count", void (optionalKey "count" parsePort))
                , ("count", void (plainKey "count" :: GroupDecoder Int))
                , ("tokenDuration", void (requiredKey "tokenDuration" parseCodeArtifactDuration))
                ]
            $ \(key, decoder) ->
                first
                    (isInfixOf ("mirrorTarget.codeArtifact." <> Key.toString key))
                    (parseEither (taggedTarget [TagCase "codeArtifact" decoder] "mirrorTarget") (object ["codeArtifact" .= object [key .= False]]))
                    `shouldBe` Left True

        it "retains the path of a wrong nested group type" $
            first (isInfixOf "$.server") (decodeServer (requiredKey "port" parsePort) (object ["server" .= False]))
                `shouldBe` Left True

        it "keeps the required-key wording when the group is absent" $
            decodeServer (requiredKey "port" parsePort) (object [])
                `shouldBe` Left "Error in $.server: server.port is required"

        it "reports unknown keys before a wrong typed value" $
            decodeServer (requiredKey "port" parsePort) (object ["server" .= object ["port" .= String "bad", "typo" .= True]])
                `shouldBe` Left "Error in $.server: unexpected server key(s): \"typo\""

        it "reports unknown keys before a missing required value" $
            decodeServer (requiredKey "port" parsePort) (object ["server" .= object ["typo" .= True]])
                `shouldBe` Left "Error in $.server: unexpected server key(s): \"typo\""

        it "preserves the numeric refusal" $
            decodeServer (requiredKey "port" parsePort) (object ["server" .= object ["port" .= (-1 :: Int)]])
                `shouldBe` Left "Error in $.server.port: server.port must be a port in 0..65535 (0 = OS-assigned), got -1"

        it "accepts both port range endpoints" $
            forM_ [0, 65535] $ \port ->
                decodeServer (requiredKey "port" parsePort) (object ["server" .= object ["port" .= port]])
                    `shouldBe` Right port

        it "preserves absent and null optional values and defaults" $
            forM_ [object [], object ["port" .= Null]] $ \fields -> do
                let doc = object ["server" .= fields]
                decodeServer (optionalKey "port" parsePort) doc `shouldBe` Right Nothing
                decodeServer (optionalKeyOr "port" 4873 parsePort) doc `shouldBe` Right 4873

        it "retains the inner path of nested typed values" $
            first
                (isInfixOf "$.server.port[0]")
                (decodeServer (plainKey "port" :: GroupDecoder [Int]) (object ["server" .= object ["port" .= [String "bad"]]]))
                `shouldBe` Left True

decodeServer :: GroupDecoder a -> Value -> Either String a
decodeServer decoder = parseEither (withObject "document" (decodeGroup "document" (nestedKey "server" (decodeGroup "server" decoder))))
