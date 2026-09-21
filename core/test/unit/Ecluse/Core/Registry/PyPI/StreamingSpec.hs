-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supported-field extraction preserves source positions across arbitrary input chunks.
module Ecluse.Core.Registry.PyPI.StreamingSpec (spec) where

import Data.Aeson (Value (Array, Null, Number, String), object, (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.PyPI.Document (simpleFiles, simpleValue)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIStream)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..), pypiFields)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), defaultLimits, maxMetadataBytes, maxNestingDepth)
import Ecluse.Test.Json (encodeStrict, fieldAt)
import Ecluse.Test.Package (requestsName)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndex, simpleIndexWith, withFileKeys)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIChunks, projectPyPIIndex)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    retainedSpec
    selectedFieldSpec
    positionSpec
    chunkSpec
    firstContainerSpec

retainedSpec :: Spec
retainedSpec = describe "supported PyPI fields" $ do
    it "omits unknown fields and sidecars before retaining the file" $ do
        let file = withFileKeys [(key, object ["unneeded" .= replicate 100 (String "payload")]) | key <- ["core-metadata", "dist-info-metadata", "data-dist-info-metadata", "unknown"]] (simpleFile filename)
            body = encodeStrict (simpleIndexWith "requests" ["unknown" .= file] [file])
        (_, document) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
        map snd (simpleFiles document) `shouldBe` [simpleFile filename]
        fieldAt "unknown" (simpleValue document) `shouldBe` Nothing

    it "retains compatibility declarations outside the policy projection" $ do
        let fields =
                [ "meta" .= object ["api-version" .= ("1.4" :: Text), "_last-serial" .= (42 :: Int), "tracks" .= ["https://upstream.test/simple/requests/" :: Text], "unknown" .= True]
                , "project-status" .= object ["status" .= ("deprecated" :: Text), "reason" .= ("use another release" :: Text), "unknown" .= True]
                , "alternate-locations" .= ["https://other.test/simple/requests/" :: Text]
                ]
        (_, document) <- expectRight (projectPyPIIndex defaultLimits requestsName (encodeStrict (simpleIndexWith "requests" fields [simpleFile filename])))
        let raw = simpleValue document
        (fieldAt "meta" raw >>= fieldAt "tracks") `shouldBe` Just (toArray ["https://upstream.test/simple/requests/"])
        (fieldAt "meta" raw >>= fieldAt "_last-serial") `shouldBe` Just (Number 42)
        (fieldAt "meta" raw >>= fieldAt "unknown") `shouldBe` Nothing
        fieldAt "project-status" raw `shouldBe` Just (object ["status" .= ("deprecated" :: Text), "reason" .= ("use another release" :: Text)])
        fieldAt "alternate-locations" raw `shouldBe` Just (toArray ["https://other.test/simple/requests/"])

    it "retains only the selected release's payload and protocol envelope" $ do
        let files = simpleFile filename : [simpleFile ("requests-2." <> show n <> ".tar.gz") | n <- [0 .. 399 :: Int]]
            body = encodeStrict (simpleIndexWith "requests" ["project-status" .= object ["reason" .= ("ignored" :: Text)]] files)
        streamed <- expectRight (projectPyPIChunks defaultLimits requestsName selected [body])
        (info, document) <- expectRight (projectPyPIStream defaultLimits requestsName streamed)
        Map.keys (infoVersions info) `shouldBe` ["1"]
        map fst (simpleFiles document) `shouldBe` [ArrayEntry 0]
        fieldAt "project-status" (simpleValue document) `shouldBe` Nothing
        streamBytes streamed `shouldBe` BS.length body
        streamDigest streamed `shouldBe` digestOf body

selectedFieldSpec :: Spec
selectedFieldSpec = describe "selected file fields" $ do
    it "joins supported fields before and after the filename without changing typed policy data" $ do
        let details =
                [ ("url", String "https://files.pythonhosted.org/requests-1.0.tar.gz")
                , ("hashes", object ["sha256" .= (replicate 64 'a' :: String)])
                , ("requires-python", String ">=3.9")
                , ("size", Number 123456)
                , ("upload-time", String "2026-01-02T03:04:05Z")
                , ("yanked", String "retiré \"quoted\" \\ newline\n")
                , ("provenance", String "https://files.pythonhosted.org/attestation.json")
                ]
            named = ("filename", String filename)
            orders = [named : details, take 3 details <> [named] <> drop 3 details, details <> [named]]
        for_ orders $ \fields -> do
            let body = rawIndex [rawObject fields]
            assertSelectedPayloads [0] body
            (expected, _) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
            streamed <- expectRight (projectPyPIChunks defaultLimits requestsName selected [body])
            (actual, _) <- expectRight (projectPyPIStream defaultLimits requestsName streamed)
            actual `shouldBe` expected
            for_ [1 .. BS.length body - 1] $ \position -> do
                let (left, right) = BS.splitAt position body
                split <- expectRight (projectPyPIChunks defaultLimits requestsName selected [left, right])
                projectPyPIStream defaultLimits requestsName split `shouldBe` projectPyPIStream defaultLimits requestsName streamed
                streamBytes split `shouldBe` BS.length body
                streamDigest split `shouldBe` digestOf body

    it "keeps every selected wheel and source distribution after skipped and malformed files" $ do
        let file name = rawObject [("hashes", object []), ("url", String ("https://files.pythonhosted.org/" <> name)), ("filename", String name)]
            body = rawIndex [file "requests-2.0.tar.gz", "null", "7", "{}", file filename, file "requests-1.0-py3-none-any.whl", file filename]
        assertSelectedPayloads [4, 5, 6] body

    for_ [Null, Array mempty, object [], String "requests-2.0.tar.gz", String "not-a-release"] $ \firstName ->
        it ("does not repair the first filename " <> show firstName) $ do
            let body = rawIndex [rawObject [("url", String "https://files.pythonhosted.org/file"), ("hashes", object []), ("filename", firstName), ("filename", String filename)]]
            assertSelectedPayloads [] body

    it "keeps a selected first filename when a later declaration names a sibling release" $ do
        let body = rawIndex [rawObject [("filename", String filename), ("filename", String "requests-2.0.tar.gz"), ("url", String "https://files.pythonhosted.org/file")]]
        assertSelectedPayloads [0] body

    for_ ["url", "requires-python", "size", "upload-time", "yanked", "provenance"] $ \key ->
        it ("keeps the first scalar declaration for " <> toString key) $ do
            let body = rawIndex [rawObject [(key, Null), (key, String "replacement"), ("filename", String filename)]]
            assertSelectedPayloads [0] body

    for_ ["null", "[]", "{}", "{\"sha256\":null,\"sha256\":\"replacement\"}"] $ \firstHashes ->
        it ("does not repair the first hashes declaration " <> show firstHashes) $ do
            let file = "{\"hashes\":" <> firstHashes <> ",\"hashes\":{\"sha512\":\"later\"},\"filename\":\"requests-1.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/file\"}"
                body = rawIndex [file]
            assertSelectedPayloads [0] body
            (expected, _) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
            streamed <- expectRight (projectPyPIChunks defaultLimits requestsName selected [body])
            (actual, _) <- expectRight (projectPyPIStream defaultLimits requestsName streamed)
            actual `shouldBe` expected

    it "preserves supported-field depth failures after rejecting a sibling filename" $ do
        let body = rawIndex [rawObject [("filename", String "requests-2.0.tar.gz"), ("hashes", object ["sha256" .= String "digest"])]]
            limits = defaultLimits{maxNestingDepth = 4}
        fmap (isLeft . streamValue) (projectPyPIChunks limits requestsName selected [body]) `shouldBe` Right True

assertSelectedPayloads :: [Int] -> ByteString -> IO ()
assertSelectedPayloads positions body = do
    full <- readEvents FullRead [body]
    kept <- readEvents selected [body]
    [position | FileField position _ <- kept] `shouldBe` [position | FileField position _ <- full]
    [(position, value) | FileField position (Just value) <- kept]
        `shouldBe` [(position, value) | FileField position (Just value) <- full, position `elem` positions]

rawIndex :: [ByteString] -> ByteString
rawIndex files = "{\"name\":\"requests\",\"files\":[" <> BS.intercalate "," files <> "]}"

rawObject :: [(Text, Value)] -> ByteString
rawObject fields = "{" <> BS.intercalate "," [encodeStrict (String key) <> ":" <> encodeStrict value | (key, value) <- fields] <> "}"

positionSpec :: Spec
positionSpec = describe "original file positions" $
    it "emits a count event for invalid, unselected and selected array items" $ do
        let body = encodeStrict (simpleIndex "requests" [Null, Number 7, object [], simpleFile "requests-2.0.tar.gz", simpleFile filename, simpleFile filename])
        events <- readEvents selected [body]
        [position | FileField position _ <- events] `shouldBe` [0 .. 5]
        [position | FileField position (Just _) <- events] `shouldBe` [4, 5]
        streamed <- expectRight (projectPyPIChunks defaultLimits requestsName selected [body])
        (_, document) <- expectRight (projectPyPIStream defaultLimits requestsName streamed)
        map fst (simpleFiles document) `shouldBe` [ArrayEntry 4, ArrayEntry 5]

chunkSpec :: Spec
chunkSpec = describe "chunk-independent projection and source identity" $ do
    it "handles split UTF-8, escapes and numbers at every byte boundary" $ do
        let file = withFileKeys [("yanked", String "retiré \"quoted\" \\ newline\n"), ("size", Number 123456)] (simpleFile filename)
            body = encodeStrict (simpleIndex "requests" [file])
        expected <- expectRight (projectPyPIIndex defaultLimits requestsName body)
        for_ [1 .. BS.length body - 1] $ \position -> do
            let (left, right) = BS.splitAt position body
            streamed <- expectRight (projectPyPIChunks defaultLimits requestsName FullRead [left, right])
            projectPyPIStream defaultLimits requestsName streamed `shouldBe` Right expected
            streamBytes streamed `shouldBe` BS.length body
            streamDigest streamed `shouldBe` digestOf body

    it "joins files before the reported name and ignores omitted deeply nested values" $ do
        let body = "{\"ignored\":" <> BS.replicate 100 91 <> "0" <> BS.replicate 100 93 <> ",\"files\":[" <> encodeStrict (simpleFile filename) <> "],\"name\":\"requests\"}"
        (info, _) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
        Map.keys (infoVersions info) `shouldBe` ["1"]

    it "hashes omitted data even when retained output stays equal" $ do
        let body suffix = "{\"name\":\"requests\",\"ignored\":\"" <> suffix <> "\"}"
        firstRead <- expectRight (projectPyPIChunks defaultLimits requestsName FullRead [body "one"])
        secondRead <- expectRight (projectPyPIChunks defaultLimits requestsName FullRead [body "two"])
        projectPyPIStream defaultLimits requestsName firstRead `shouldBe` projectPyPIStream defaultLimits requestsName secondRead
        streamDigest firstRead `shouldNotBe` streamDigest secondRead

firstContainerSpec :: Spec
firstContainerSpec = describe "first declared containers" $ do
    for_ ["[]", "null"] $ \firstFiles ->
        it ("keeps the first files value " <> show firstFiles) $ do
            let body = "{\"name\":\"requests\",\"files\":" <> firstFiles <> ",\"files\":[" <> encodeStrict (simpleFile filename) <> "]}"
            (info, document) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
            infoVersions info `shouldBe` mempty
            simpleFiles document `shouldBe` []
            streamed <- expectRight (projectPyPIChunks defaultLimits requestsName selected [body])
            projectPyPIStream defaultLimits requestsName streamed `shouldBe` Right (info, document)

    it "does not let a later hashes object repair an unusable first declaration" $ do
        let file = "{\"filename\":\"requests-1.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/requests-1.0.tar.gz\",\"hashes\":[],\"hashes\":{\"sha256\":\"abc\"}}"
        (info, _) <- expectRight (projectPyPIIndex defaultLimits requestsName ("{\"name\":\"requests\",\"files\":[" <> file <> "]}"))
        infoVersions info `shouldBe` mempty

filename :: Text
filename = "requests-1.0.tar.gz"

selected :: PyPIRead
selected = SelectedRead requestsName "1"

toArray :: [Text] -> Value
toArray = Array . fromList . map String

readEvents :: PyPIRead -> [ByteString] -> IO [PyPIField]
readEvents mode chunks = do
    streamed <- expectRight (parseJsonChunks (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (pypiFields 64 mode) (\acc field -> Right (field : acc)) [] chunks)
    reverse <$> expectRight (streamValue streamed)
