-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A packument walk holds nothing for what its projection drops: repeated release keys, repeated
versions objects, and the members a kept release repeats leave the live bytes where a single copy
leaves them.
-}
module Ecluse.Core.Registry.Npm.ReaderResidencySpec (spec) where

import Data.ByteString.Builder qualified as Builder
import Test.Hspec

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Json.Shape (Trees (..))
import Ecluse.Core.Registry.Json.Walk (pureStep)
import Ecluse.Core.Registry.Json.WalkProbe (allowance, heldDuring, heldWritingDuring)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.StreamingProjection (emptyTreeRead, keepsTreeRelease, treeStep)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Registry.JsonStream (testTable)
import Ecluse.Test.Registry.Npm.Metadata (npmFullTestWalk)

-- | Eight times more dropped releases may not raise the bytes a read holds.
spec :: Spec
spec = describe "npmWalk live bytes" $ do
    forM_ [("a selected read", selectedRead), ("a full read", fullRead)] $ \(label, heldBy) -> do
        it (label <> " holds the same bytes however often a release's key repeats") $
            heldFor heldBy repeatedKeys >>= (`shouldSatisfy` level)
        it (label <> " holds the same bytes however often a kept release repeats a member") $
            heldFor heldBy repeatedMember >>= (`shouldSatisfy` level)
        it (label <> " holds the same bytes however often a kept release's dependencies repeat a name") $
            heldFor heldBy repeatedDependency >>= (`shouldSatisfy` level)
    it "a selected read holds the same bytes however many versions objects repeat the release" $
        heldFor selectedRead repeatedContainers >>= (`shouldSatisfy` level)

-- The bytes held with 2,000 and with 16,000 repeats.
heldFor :: (ByteString -> IO Integer) -> (Int -> ByteString) -> IO (Integer, Integer)
heldFor heldBy body = (,) <$> heldBy (body 2000) <*> heldBy (body 16000)

-- A selected read builds aeson's tree, and a full read packs each release as production does.
selectedRead, fullRead :: ByteString -> IO Integer
selectedRead = heldDuring (npmWalk Trees 64 (OneRelease "1.0.0") (pureStep (treeStep defaultLimits thing)) keepsTreeRelease (testTable releaseUniqueFields) emptyTreeRead)
fullRead = heldWritingDuring (npmFullTestWalk defaultLimits thing "https://registry.npmjs.org")

thing :: PackageName
thing = unscopedNpm "thing"

level :: (Integer, Integer) -> Bool
level (few, repeated) = repeated - few < allowance

-- One versions object that repeats the release key, each copy with its own main entry.
repeatedKeys :: Int -> ByteString
repeatedKeys count = render ("{\"name\":\"thing\",\"versions\":{" <> commas [release index | index <- [1 .. count]] <> "}}")

-- Versions objects that each hold the release once. Only the first claims the container.
repeatedContainers :: Int -> ByteString
repeatedContainers count = render ("{\"name\":\"thing\"," <> commas ["\"versions\":{" <> release index <> "}" | index <- [1 .. count]] <> "}")

-- One release whose main member repeats with a new value each time.
repeatedMember :: Int -> ByteString
repeatedMember count = oneRelease (commas ["\"main\":\"lib/main-" <> Builder.intDec index <> ".js\"" | index <- [1 .. count]])

-- One release whose dependencies repeat one name with a new range each time.
repeatedDependency :: Int -> ByteString
repeatedDependency count = oneRelease ("\"dependencies\":{" <> commas ["\"dep\":\"^1." <> Builder.intDec index <> "\"" | index <- [1 .. count]] <> "}")

oneRelease :: Builder.Builder -> ByteString
oneRelease members = render ("{\"name\":\"thing\",\"versions\":{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\"," <> members <> "}}}")

release :: Int -> Builder.Builder
release index =
    "\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\",\"main\":\"lib/main-"
        <> Builder.intDec index
        <> ".js\",\"dependencies\":{\"dep-"
        <> Builder.intDec index
        <> "\":\"^1\"},\"dist\":{\"tarball\":\"https://registry.npmjs.org/thing/-/thing-1.0.0.tgz\"}}"

commas :: [Builder.Builder] -> Builder.Builder
commas = mconcat . intersperse ","

render :: Builder.Builder -> ByteString
render = toStrict . Builder.toLazyByteString
