-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A Simple-index walk holds nothing for what its projection drops: repeated files arrays, the
members a file repeats, the files of other releases, and files past a tripped limit leave the live
bytes where a few leave them.
-}
module Ecluse.Core.Registry.PyPI.ReaderResidencySpec (spec) where

import Data.ByteString.Builder qualified as Builder
import Test.Hspec

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Json.WalkProbe (allowance, heldDuring)
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIStream)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (collectField, emptyProjection, keepsFile)
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Retention (boundedRetention)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.JsonStream (testTable)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIChunks)
import Ecluse.Test.Support (expectRight)

-- | Eight times more dropped files or members may not raise the bytes a read holds.
spec :: Spec
spec = describe "pypiWalk live bytes" $ do
    it (caseLabel "retention/pypi/selected/containers" "a selected read holds the same bytes however many files arrays repeat the release") $
        selectedLevel repeatedArrays
    it (caseLabel "retention/pypi/selected/members" "a selected read holds the same bytes however often a file repeats a member") $
        selectedLevel repeatedMembers
    it (caseLabel "retention/pypi/selected/other-releases" "a selected read holds the same bytes however many other releases the files name") $
        selectedLevel otherReleases
    it (caseLabel "retention/pypi/full/members" "a full read holds the same bytes however often a kept file repeats a member") $
        boundedRetention allowance (heldFor FullRead defaultLimits repeatedMembers)
    it (caseLabel "retention/pypi/full/past-limit" "a full read holds the same bytes however many files follow a tripped file limit") $
        boundedRetention allowance (heldFor FullRead defaultLimits{maxVersionCount = 2} filesPastLimit)

-- A selected read keeps its one file of the release at both sizes, and holds the same bytes.
selectedLevel :: (Int -> ByteString) -> IO ()
selectedLevel body = do
    for_ [2000, 16000] $ \count -> do
        streamed <- expectRight (projectPyPIChunks defaultLimits thing selected [body count])
        (_, document) <- expectRight (projectPyPIStream defaultLimits thing streamed)
        length (simpleFiles document) `shouldBe` 1
    boundedRetention allowance (heldFor selected defaultLimits body)

-- The release every body's kept file names, under its canonical key.
selected :: PyPIRead
selected = SelectedRead thing "1"

thing :: PackageName
thing = unscopedPyPI "thing"

heldFor :: PyPIRead -> Limits -> (Int -> ByteString) -> Int -> IO Integer
heldFor mode limits body count =
    heldDuring (pypiWalk 64 mode (collectField limits) keepsFile (testTable fileUniqueFields) (emptyProjection thing)) (body count)

-- Files arrays that each hold one file of the release. Only the first claims the key.
repeatedArrays :: Int -> ByteString
repeatedArrays count = index (commas ["\"files\":[" <> file "1.0.0.tar.gz" position <> "]" | position <- [1 .. count]])

-- One file of the release whose requires-python member repeats with a new value each time.
repeatedMembers :: Int -> ByteString
repeatedMembers count =
    index
        ( "\"files\":[{\"filename\":\"thing-1.0.0.tar.gz\",\"url\":\"https://files.example/thing-1.0.0.tar.gz\","
            <> commas ["\"requires-python\":\">=3." <> Builder.intDec position <> "\"" | position <- [1 .. count]]
            <> "}]"
        )

-- One source distribution of each of as many other releases, after one of the selected release.
otherReleases :: Int -> ByteString
otherReleases count = index ("\"files\":[" <> commas (file "1.0.0.tar.gz" 0 : [file ("2." <> Builder.intDec position <> ".tar.gz") position | position <- [1 .. count]]) <> "]")

-- Wheels of one release under distinct build tags, each with its own requires-python value.
filesPastLimit :: Int -> ByteString
filesPastLimit count = index ("\"files\":[" <> commas [file ("1.0.0-" <> Builder.intDec position <> "-py3-none-any.whl") position | position <- [1 .. count]] <> "]")

file :: Builder.Builder -> Int -> Builder.Builder
file suffix position =
    "{\"filename\":\"thing-"
        <> suffix
        <> "\",\"url\":\"https://files.example/thing-"
        <> suffix
        <> "\",\"hashes\":{\"sha256\":\"0000\"},\"requires-python\":\">=3."
        <> Builder.intDec position
        <> "\"}"

index :: Builder.Builder -> ByteString
index members = toStrict (Builder.toLazyByteString ("{\"name\":\"thing\",\"meta\":{\"api-version\":\"1.1\"}," <> members <> "}"))

commas :: [Builder.Builder] -> Builder.Builder
commas = mconcat . intersperse ","
