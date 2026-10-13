-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native inventories agree with the independent combinator oracle across hostile input and chunk boundaries.
module Ecluse.Core.Registry.Npm.VersionListReaderSpec (spec) where

import Data.ByteString qualified as BS
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), isNestingLimit)
import Ecluse.Core.Registry.Npm.VersionListReader (npmVersionListWalk)
import Ecluse.Core.Registry.VersionList (VersionListItem (..), collectVersionList, emptyVersionList, finishVersionList)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyVersions), Limits (maxNestingDepth, maxVersionCount), defaultLimits)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genPackumentBytes, genServablePackumentBytes)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, walkJsonChunks)
import Ecluse.Test.Registry.Npm.Project (parseVersionBytes, referenceVersionListParser)

-- | Preserve source-entry counting, compact witness acceptance, and retained-depth failure classification.
spec :: Spec
spec = describe "npmVersionListWalk" $ do
    modifyMaxSuccess (const 2000) $
        it "matches reference events, inventory, bytes and failures on generated bodies" $
            hedgehog $ do
                body <- forAll (Gen.choice [genPackumentBytes, genServablePackumentBytes] >>= damaged)
                chunks <- forAll (genChunks body)
                depth <- forAll (Gen.frequency [(3, pure 64), (2, Gen.int (Range.linear 0 7))])
                versionCap <- forAll (Gen.int (Range.linear 0 20))
                eventCap <- forAll (Gen.maybe (Gen.int (Range.linear 0 20)))
                let limits = defaultLimits{maxNestingDepth = depth, maxVersionCount = versionCap}
                    (reference, walked) = bothReads limits eventCap chunks
                walked === reference

    forM_ witnessCases $ \(label, fields, expected) ->
        it label $ do
            let body = packument (release fields)
            fmap (map renderVersion) (parseVersionBytes body) `shouldBe` Right expected
            atEverySplit defaultLimits body

    forM_ ["name", "version", "hasInstallScript", "license", "_npmUser"] $ \field ->
        it ("decodes malformed retained strings in " <> show field) $ do
            let body = packument (release ("\"" <> field <> "\":\"\\x\","))
            parseVersionBytes body `shouldSatisfy` isLeft
            atEverySplit defaultLimits body

    it "refuses invalid UTF-8 in string witnesses across every split" $ do
        forM_ [BS.pack [0xff], BS.pack [0xc0, 0xaf]] $ \invalid -> do
            let body = packument (release ("\"name\":\"" <> invalid <> "\","))
            parseVersionBytes body `shouldSatisfy` isLeft
            atEverySplit defaultLimits body

    it "sorts and deduplicates version-map keys independently of release identity" $ do
        let body = "{\"versions\":{\"2\":" <> release "" <> ",\"1\":" <> release "" <> ",\"2\":" <> release "" <> "}}"
        fmap (map renderVersion) (parseVersionBytes body) `shouldBe` Right ["1", "2"]
        atEverySplit defaultLimits body

    it "decodes malformed dist witnesses" $ do
        let body = packument "{\"name\":\"thing\",\"version\":\"1\",\"dist\":{\"tarball\":\"\\x\"}}"
        parseVersionBytes body `shouldSatisfy` isLeft
        atEverySplit defaultLimits body

    it "decodes every duplicate script string after an invalid value" $ do
        let body = packument (release "\"scripts\":{\"install\":false,\"install\":\"\\x\"},")
        parseVersionBytes body `shouldSatisfy` isLeft
        atEverySplit defaultLimits body

    it "decodes repeated retained fields even when the first value wins" $ do
        let body = packument (release "\"name\":\"thing\",\"name\":\"\\x\",")
        parseVersionBytes body `shouldSatisfy` isLeft
        atEverySplit defaultLimits body

    it "checks the depth of retained fields in duplicate containers" $ do
        let limits = defaultLimits{maxNestingDepth = 4}
            body = "{\"versions\":{},\"versions\":{\"1\":{\"dist\":{\"tarball\":\"x\"}}}}"
        nativeInventory limits [body] `shouldBe` Right (BS.length body, Left True)
        atEverySplit limits body

    it "checks the depth of duplicate fields before dropping their values" $ do
        let limits = defaultLimits{maxNestingDepth = 4}
            body = "{\"versions\":{\"1\":{\"dist\":{},\"dist\":{\"tarball\":\"x\"}}}}"
        nativeInventory limits [body] `shouldBe` Right (BS.length body, Left True)
        atEverySplit limits body

    it "retains the publisher shape's depth and lexer failure precedence" $ do
        forM_ ["{\"name\":null}", "{\"name\":null,\"other\":?}"] $ \publisher -> do
            let limits = defaultLimits{maxNestingDepth = 4}
                body = packument (release ("\"_npmUser\":" <> publisher <> ","))
            atEverySplit limits body

    it "counts unusable and duplicate entries before filtering" $ do
        let limits = defaultLimits{maxVersionCount = 1}
            body = "{\"versions\":{\"1\":null,\"1\":null}}"
        nativeInventory limits [body] `shouldBe` Left (TooManyVersions 2 1)
        atEverySplit limits body

    it "does not count entries in a later duplicate container" $ do
        let limits = defaultLimits{maxVersionCount = 0}
            body = "{\"versions\":{},\"versions\":{\"1\":null}}"
        nativeInventory limits [body] `shouldBe` Right (BS.length body, Right [])
        atEverySplit limits body

    forM_ ["null", "[]", "false", "1", "\"x\"", "{}", "{\"versions\":null}", "{\"versions\":{}}"] $ \body ->
        it ("preserves root and empty-container semantics for " <> show body) $
            forM_ [0 .. 5] $
                \depth -> atEverySplit defaultLimits{maxNestingDepth = depth} body

    it "reads skipped malformed strings without decoding them" $ do
        let body = "{\"time\":\"\\x\",\"dist-tags\":\"\\x\",\"versions\":{\"1\":" <> release "\"deprecated\":\"\\x\",\"unknown\":\"\\x\"," <> "}}"
        fmap (map renderVersion) (parseVersionBytes body) `shouldBe` Right ["1"]
        atEverySplit defaultLimits body

    it "drains trailing input and enforces the bound after extraction" $ do
        let body = packument (release "") <> BS.replicate 80 0x20
            chunks = [BS.take 10 body, BS.drop 10 body]
            bound = MetadataBodyLimit (BS.length body - 1)
        void (walkJsonChunks bound (npmVersionListWalk defaultLimits (collectVersionList defaultLimits) emptyVersionList) chunks)
            `shouldBe` Left (BodyTooLarge bound)
        void (parseJsonChunks bound (referenceVersionListParser defaultLimits) (collectVersionList defaultLimits) emptyVersionList chunks)
            `shouldBe` Left (BodyTooLarge bound)
        nativeInventory defaultLimits chunks `shouldBe` Right (BS.length body, Right ["1"])

    it "matches incomplete input and trailing data at every split" $
        forM_ ["{\"versions\":{", packument (release "") <> "garbage", packument (release "") <> "}"] $ \body ->
            atEverySplit defaultLimits body

    it "drops source members whose keys exceed 64 KiB across pieces" $ do
        let limits = defaultLimits{maxVersionCount = 1}
            key = BS.replicate 70000 0x6b
            body = "{\"versions\":{\"" <> key <> "\":null,\"1\":" <> release "" <> "}}"
            chunks = [BS.take 30 body, BS.take 32768 (BS.drop 30 body), BS.drop 32798 body]
            (reference, walked) = bothReads limits Nothing chunks
        walked `shouldBe` reference
        nativeInventory limits chunks `shouldBe` Right (BS.length body, Right ["1"])

    it "preserves the lexer digit ceiling even in discarded values" $ do
        let body = "{\"ignored\":" <> BS.replicate 300000 0x31 <> "}"
            chunks = [BS.take 32768 (BS.drop offset body) | offset <- [0, 32768 .. BS.length body]]
            (reference, walked) = bothReads defaultLimits Nothing chunks
        walked `shouldBe` reference
        nativeInventory defaultLimits chunks `shouldSatisfy` either (const False) (isLeft . snd)

type Event = (Text, Maybe Text)

type Outcome = Either LimitError (Int, Either Bool (Either ParseError [Text], [Event]))

bothReads :: Limits -> Maybe Int -> [ByteString] -> (Outcome, Outcome)
bothReads limits cap chunks =
    ( outcome (parseJsonChunks bound (referenceVersionListParser limits) step initial chunks)
    , outcome (walkJsonChunks bound (npmVersionListWalk limits step initial) chunks)
    )
  where
    bound = MetadataBodyLimit (sum (map BS.length chunks))
    initial = (emptyVersionList, [])
    outcome = fmap (second (fmap (first (fmap (map renderVersion) . finishVersionList)))) . readOutcome
    step (inventory, events) field = do
        next <- collectVersionList limits inventory field
        case cap of
            Just most | length events >= most -> Left (TooManyVersions (length events) most)
            _ -> Right (next, event field : events)

event :: VersionListItem -> Event
event = \case
    VersionListObject -> ("object", Nothing)
    VersionListContainer -> ("container", Nothing)
    VersionListInvalidContainer -> ("invalid", Nothing)
    VersionListEntry version -> ("entry", renderVersion <$> version)

nativeInventory :: Limits -> [ByteString] -> Either LimitError (Int, Either Bool [Text])
nativeInventory limits chunks = do
    streamed <- walkJsonChunks (MetadataBodyLimit (sum (map BS.length chunks))) (npmVersionListWalk limits (collectVersionList limits) emptyVersionList) chunks
    pure (streamBytes streamed, first isNestingLimit (streamValue streamed >>= fmap (map renderVersion) . finishVersionList))

atEverySplit :: Limits -> ByteString -> Expectation
atEverySplit limits body = forM_ [0 .. BS.length body] $ \cut -> do
    let chunks = filter (not . BS.null) [BS.take cut body, BS.drop cut body]
        (reference, walked) = bothReads limits Nothing chunks
    walked `shouldBe` reference

packument :: ByteString -> ByteString
packument raw = "{\"versions\":{\"1\":" <> raw <> "}}"

release :: ByteString -> ByteString
release fields = "{" <> fields <> "\"name\":\"thing\",\"version\":\"1\",\"dist\":{\"tarball\":\"x\"}}"

witnessCases :: [(String, ByteString, [Text])]
witnessCases =
    [ ("keeps string witnesses without their source text", "", ["1"])
    , ("keeps null optional fields", "\"scripts\":null,\"license\":null,\"_npmUser\":null,", ["1"])
    , ("rejects a null required field", "\"name\":null,", [])
    , ("keeps the first duplicate required field", "\"name\":false,\"name\":\"thing\",", [])
    , ("rejects malformed publisher shapes", "\"_npmUser\":false,", [])
    , ("rejects an invalid publisher member", "\"_npmUser\":{\"email\":false},", [])
    , ("rejects non-object distribution fields", "\"dist\":false,", [])
    , ("rejects an invalid distribution witness", "\"dist\":{\"tarball\":false},", [])
    , ("rejects a scripts array", "\"scripts\":[],", [])
    , ("keeps a scripts object with string values", "\"scripts\":{\"install\":\"x\"},", ["1"])
    , ("retains invalid duplicate script values before valid ones", "\"scripts\":{\"install\":false,\"install\":\"x\"},", [])
    , ("retains invalid duplicate script values after valid ones", "\"scripts\":{\"install\":\"x\",\"install\":false},", [])
    , ("keeps the first scripts field after reading a later invalid field", "\"scripts\":{},\"scripts\":false,", ["1"])
    ]
