{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.AesonSpec (spec) where

import Data.Text qualified as T

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Config (Config (..), ConfigError, loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (..))

spec :: Spec
spec = describe "decodeDocument" $ do
    it "decodes a document with one mount and a rule patch" $
        case loadConfig [] (Just singleMountDoc) of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "decodes a document carrying only a rule policy (no mounts)" $
        case loadConfig [] (Just "{\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> configMounts doc `shouldBe` mempty

    it "keys a mount by its ecosystem name, deriving the prefix from it" $
        case loadConfig [] (Just (mountDocForEcosystem "npm")) of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "rejects an unparseable JSON body" $
        loadConfig [] (Just "{not json") `shouldSatisfy` isLeft

    it "rejects an unknown top-level key, naming it (strict, not silently dropped)" $
        loadConfig [] (Just "{\"mountz\":{}}") `shouldSatisfy` decodeErrorMentions "mountz"

    it "rejects an unknown mount ecosystem key, naming it (strict, not silently dropped)" $
        loadConfig [] (Just (mountDocForEcosystem "npmm")) `shouldSatisfy` decodeErrorMentions "npmm"

    it "rejects an unknown key inside a mount, naming it" $
        loadConfig [] (Just (mountDocWithExtraKey "baseURL")) `shouldSatisfy` decodeErrorMentions "baseURL"

singleMountDoc :: ByteString
singleMountDoc =
    "{\"queueBackend\":\"sqs\",\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"credentialProvider\":\"codeartifact\"}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

mountDocForEcosystem :: Text -> ByteString
mountDocForEcosystem eco =
    encodeUtf8 $
        "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
               \\"mirrorTarget\":\"https://c\",\"credentialProvider\":\"static\"}}}"

mountDocWithExtraKey :: Text -> ByteString
mountDocWithExtraKey extra =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
        \\"mirrorTarget\":\"https://c\",\"credentialProvider\":\"static\",\""
            <> extra
            <> "\":\"x\"}}}"

decodeErrorMentions :: Text -> Either [ConfigError] a -> Bool
decodeErrorMentions phrase (Left errs) = any (\err -> phrase `T.isInfixOf` renderConfigError err) errs
decodeErrorMentions _ (Right _) = False
