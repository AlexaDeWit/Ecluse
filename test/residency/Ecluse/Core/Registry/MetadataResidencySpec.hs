-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full reads retain the same heap before forcing and release served documents independently of typed views.
module Ecluse.Core.Registry.MetadataResidencySpec (spec) where

import Data.Aeson (Value (Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Unique (hashUnique, newUnique)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeak)
import Test.Hspec
import UnliftIO.Exception (bracket, evaluate, finally)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Package (PackageInfo (infoVersions), pkgEcosystem)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, cargoPacked, npmCached, npmPacked, pypiPacked, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Cargo.Document (PackedIndex (..), cargoRecords)
import Ecluse.Core.Registry.Json.Packed (DocTable, docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..))
import Ecluse.Core.Registry.PyPI.Document (PackedSimple (..), simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (gemRows), GemRow (grSource))
import Ecluse.Core.Server.MemoryModel.Probe (Evaluated (..), measureInChild, packages, project)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName, readCapture)
import Ecluse.Test.Package (hexSha256Of)

-- | Every corpus capture, measured in a fresh child process and collected in this one.
spec :: Spec
spec = do
    packedTableReleaseSpec
    describe "metadata read evaluation" $ forM_ packages $ \package -> do
        it (toString (cpName package) <> " retains the same at weak head as fully forced") $ do
            digest <- evaluate . hexSha256Of =<< readCapture package
            measureInChild Mode.Evaluation package >>= \case
                Left failure -> expectationFailure failure
                Right result -> do
                    report package digest result
                    abs (forcing result) `shouldSatisfy` (<= allowance)

        it (toString (cpName package) <> " releases the served document while its typed view is held") $ do
            void (readCapture package)
            bracket (detach package) (\(typed, _, _) -> freeStablePtr typed) $ \(typed, document, keys) -> do
                length keys `shouldSatisfy` (> 2)
                ((performMajorGC >> alive keys) `finally` freeStablePtr document) `shouldReturn` length keys
                performMajorGC
                alive keys `shouldReturn` 0
                deRefStablePtr typed >>= (`shouldSatisfy` (> 0)) . Map.size . infoVersions

-- The largest change forcing may make to live bytes, in either direction.
allowance :: Integer
allowance = 1024

-- Live bytes that forcing the rooted result added, or released when negative.
forcing :: Evaluated -> Integer
forcing result = toInteger (evaluatedForced result) - toInteger (evaluatedWeakHead result)

report :: CorpusPackage -> Text -> Evaluated -> IO ()
report package digest result =
    putStrLn . ("metadata-evaluation " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "ecosystem" .= ecosystemName (pkgEcosystem (cpPackage package))
            , "package" .= cpName package
            , "path" .= cpPath package
            , "sha256" .= digest
            , "versions" .= evaluatedVersions result
            , "weak_head_retained" .= (toInteger (evaluatedWeakHead result) - toInteger (evaluatedBaseline result))
            , "forcing" .= forcing result
            , "allowance" .= allowance
            ]

-- Only the returned roots and weak keys survive this call.
{-# NOINLINE detach #-}
detach :: CorpusPackage -> IO (StablePtr PackageInfo, StablePtr CachedDoc, [Weak ()])
detach package = do
    bytes <- BS.readFile (cpPath package)
    (info, document) <- project package bytes
    served <- evaluate document
    keys <- documentKeys served
    (,,) <$> (evaluate info >>= newStablePtr) <*> newStablePtr served <*> pure keys

-- The document and every object it serves, each with its member map, which a thunk could keep alone.
-- A packed document serves its table and each packed release.
documentKeys :: CachedDoc -> IO [Weak ()]
documentKeys document =
    (<>) <$> sequence [track document] <*> case (snd npmPacked document, snd pypiPacked document) of
        (Just packed, _) -> (<>) <$> trackMembers (packumentTop packed) <*> packedKeys (packumentTable packed) (KeyMap.elems (packumentVersions packed))
        (_, Just packed) -> (<>) <$> trackMembers (packedEnvelope packed) <*> packedKeys (packedTable packed) (map snd (packedFiles packed))
        _ | Just packed <- snd cargoPacked document -> packedKeys (indexTable packed) (map snd (indexRecords packed))
        _ -> treeKeys
  where
    treeKeys = case (snd npmCached document, snd pypiSimpleCached document) of
        (Just root, _) -> concat <$> traverse trackObject (root : releases root)
        (_, Just simple) -> (<>) <$> trackMembers (simpleEnvelope simple) <*> (concat <$> traverse (trackObject . snd) (simpleFiles simple))
        _ | Just cargo <- snd cargoCached document -> concat <$> traverse (trackObject . snd) (cargoRecords cargo)
        _ | Just native <- snd rubyGemsCached document -> do
            root <- track native
            rows <- track (gemRows native)
            objects <- traverse track (gemRows native)
            sources <- traverse (track . grSource) (gemRows native)
            pure (root : rows : objects <> sources)
        _ -> pure []
    packedKeys table values = (<>) <$> trackTable table <*> traverse track values
    -- Empty tables carry no source strings and can remain as shared static values.
    trackTable table = if table == docTable mempty then pure [] else one <$> track table
    track :: a -> IO (Weak ())
    track key = evaluate key >>= \evaluated -> mkWeak evaluated () Nothing
    trackObject value = case value of
        Object members -> (:) <$> track value <*> trackMembers members
        _ -> pure []
    -- An empty map is a shared static object that never dies.
    trackMembers members = if KeyMap.null members then pure [] else one <$> track members
    releases = \case
        Object fields | Just (Object listed) <- KeyMap.lookup "versions" fields -> KeyMap.elems listed
        _ -> []

alive :: [Weak ()] -> IO Int
alive keys = length . filter isJust <$> traverse deRefWeak keys

packedTableReleaseSpec :: Spec
packedTableReleaseSpec = it "tracks a runtime-owned nonempty table independently of its packed document" $ do
    keys <- bracket detachPackedTable (\(_, table, _) -> freeStablePtr table) $ \(document, _, tracked) -> do
        let whileHeld = do
                length tracked `shouldBe` 4
                performMajorGC
                alive tracked
        (whileHeld `finally` freeStablePtr document) `shouldReturn` 4
        performMajorGC
        alive tracked `shouldReturn` 1
        pure tracked
    performMajorGC
    alive keys `shouldReturn` 0

{-# NOINLINE detachPackedTable #-}
detachPackedTable :: IO (StablePtr CachedDoc, StablePtr DocTable, [Weak ()])
detachPackedTable = do
    marker <- show . hashUnique <$> newUnique
    let table = docTable (fromList [marker])
        rows = [(RecordEntry index, packTree [] (String (marker <> show index))) | index <- [0, 1]]
    document <- evaluate (fst cargoPacked (PackedIndex table rows))
    keys <- documentKeys document
    (,,) <$> newStablePtr document <*> newStablePtr table <*> pure keys
