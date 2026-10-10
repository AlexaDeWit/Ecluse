-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The install oracle permits source URL changes and exposes every other byte and entry change.
module Ecluse.Test.InstalledTreeSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectory, createFileLink, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Test.InstalledTree

spec :: Spec
spec = do
    describe "normaliseNpmSources" $ do
        for_ ["package-lock.json", "node_modules/.package-lock.json"] $ \path -> do
            it ("accepts only package resolved URL changes in " <> path) $ do
                let lock proxy = "{\n  \"packages\": {\"node_modules/example\": {\"resolved\": \"" <> proxy <> "/npm/example/-/example-1.0.0.tgz\", \"integrity\": \"fixed\"}}, \"extra\": [true, null, 3, {}, []]\n}\n"
                normalise cold (file path (lock (encodeUtf8 cold))) `shouldBe` normalise hot (file path (lock (encodeUtf8 hot)))
                normalise cold (file path (lock (encodeUtf8 cold))) `shouldBe` file path (lock "<proxy>")
            it ("preserves unrelated lockfile strings in " <> path) $ do
                let lock proxy = "{\"packages\":{\"node_modules/example\":{\"resolved\":\"" <> proxy <> "/npm/example/-/example.tgz\",\"description\":\"" <> proxy <> "\"}},\"resolved\":\"" <> proxy <> "\"}"
                normalise cold (file path (lock (encodeUtf8 cold))) `shouldNotBe` normalise hot (file path (lock (encodeUtf8 hot)))
        for_ ["node_modules/example/index.js", "node_modules/example/package.json", "package.json", "node_modules/example/package-lock.json"] $ \path ->
            it ("keeps bytes exact in " <> path) $ do
                normalise cold (file path (encodeUtf8 cold)) `shouldBe` file path (encodeUtf8 cold)
                normalise cold (file path (encodeUtf8 cold)) `shouldNotBe` normalise hot (file path (encodeUtf8 hot))
        it "keeps lockfile whitespace, key order, escapes, and unrelated values exact" $ do
            let bytes = "{ \"packages\" : {\"node_modules/example\": {\"description\": \"\\u0061\", \"resolved\": \"http://127.0.0.1:10001/npm/example.tgz\"}}, \"x\":1e0 }\r\n"
                expected = "{ \"packages\" : {\"node_modules/example\": {\"description\": \"\\u0061\", \"resolved\": \"<proxy>/npm/example.tgz\"}}, \"x\":1e0 }\r\n"
            normalise cold (file "package-lock.json" bytes) `shouldBe` file "package-lock.json" expected
            normalise cold (file "package-lock.json" bytes) `shouldNotBe` normalise cold (file "package-lock.json" (bytes <> " "))
        it "does not treat an array element, root source, or nested field as a package source" $ do
            for_ ["{\"packages\":[{\"resolved\":\"http://127.0.0.1:10001/npm/x.tgz\"}]}", "{\"packages\":{\"\":{\"resolved\":\"http://127.0.0.1:10001/npm/x.tgz\"}}}", "{\"packages\":{\"node_modules/x\":{\"extra\":{\"resolved\":\"http://127.0.0.1:10001/npm/x.tgz\"}}}}"] $ \bytes ->
                normalise cold (file "package-lock.json" bytes) `shouldBe` file "package-lock.json" bytes
        it "keeps malformed JSON and non-proxy source URLs exact" $ do
            for_ ["{broken", "{\"packages\":{\"node_modules/x\":{\"resolved\":\"http://127.0.0.1:100010/npm/x.tgz\"}}}", "{\"packages\":{\"node_modules/x\":{\"resolved\":\"http://127.0.0.1:10001/other/x.tgz\"}}}"] $ \bytes ->
                normalise cold (file "package-lock.json" bytes) `shouldBe` file "package-lock.json" bytes
        it "preserves links, executable permissions, and directory entries" $ do
            let tree = Map.fromList [("node_modules", TreeDirectory), ("node_modules/.bin/x", TreeLink (toString cold)), ("package-lock.json", TreeFile True "{}")]
            normalise cold tree `shouldBe` tree
    describe "treeDifferences" $ do
        it "reports differing binary bytes and line order with both side names" $ do
            let left = BS.pack [255, 0, 10, 65, 10, 66]
                right = BS.pack [255, 0, 10, 66, 10, 65]
                diff = T.intercalate "\n" (treeDifferences ("cold", file "index.js" left) ("hot", file "index.js" right))
            for_ ["index.js", "cold", "hot", show left, show right] $ \fragment ->
                diff `shouldSatisfy` T.isInfixOf fragment
        it "reports executable permissions, links, and missing paths" $ do
            for_ [(TreeFile False "x", TreeFile True "x"), (TreeLink "x", TreeLink "y"), (TreeDirectory, TreeFile False "x")] $ \(left, right) ->
                treeDifferences ("cold", Map.singleton "path" left) ("hot", Map.singleton "path" right) `shouldSatisfy` (not . null)
            treeDifferences ("cold", mempty) ("hot", file "new" "x") `shouldSatisfy` (not . null)
    describe "snapshotTree" $
        it "records file bytes, owner executable permission, and dangling links without following them" $
            withSystemTempDirectory "installed-tree" $ \dir -> do
                createDirectory (dir </> "node_modules")
                BS.writeFile (dir </> "node_modules/bin") (BS.pack [0, 255])
                permissions <- getPermissions (dir </> "node_modules/bin")
                setPermissions (dir </> "node_modules/bin") (setOwnerExecutable True permissions)
                createFileLink "../absent" (dir </> "node_modules/link")
                snapshotTree dir ["node_modules", "absent"] `shouldReturn` Map.fromList [("node_modules", TreeDirectory), ("node_modules/bin", TreeFile True (BS.pack [0, 255])), ("node_modules/link", TreeLink "../absent")]

cold :: Text
cold = "http://127.0.0.1:10001"

hot :: Text
hot = "http://127.0.0.1:10002"

normalise :: Text -> InstalledTree -> InstalledTree
normalise proxy = normaliseNpmSources proxy "<proxy>"

file :: FilePath -> ByteString -> InstalledTree
file path bytes = Map.singleton path (TreeFile False bytes)
