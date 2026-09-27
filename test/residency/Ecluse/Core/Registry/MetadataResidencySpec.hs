-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A full metadata read is fully evaluated when it returns: held at weak head normal form, it
retains what the fully forced result retains, and its typed view never keeps the served document alive.
-}
module Ecluse.Core.Registry.MetadataResidencySpec (spec) where

import Data.Aeson (Value (Object), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeak)
import Test.Hspec
import UnliftIO.Exception (bracket, evaluate, finally)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (PackageInfo (infoVersions), pkgEcosystem)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Server.MemoryModel.Probe (Evaluation (Forced, WeakHead), Measurement (..), measureInChild, packages, project)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName)

-- | Every corpus capture, measured in fresh child processes and collected in this one.
spec :: Spec
spec = describe "metadata read evaluation" $ forM_ packages $ \package -> do
    it (toString (cpName package) <> " retains the same at weak head as fully forced") $ do
        weakHead <- measureInChild "--metadata-evaluation-probe" (show WeakHead) package
        forced <- measureInChild "--metadata-evaluation-probe" (show Forced) package
        report package weakHead forced
        abs (retained weakHead - retained forced) `shouldSatisfy` (<= allowance package (versions forced))

    it (toString (cpName package) <> " releases the served document while its typed view is held") $
        bracket (detach package) (\(typed, _, _) -> freeStablePtr typed) $ \(typed, document, keys) -> do
            length keys `shouldSatisfy` (> 2)
            ((performMajorGC >> alive keys) `finally` freeStablePtr document) `shouldReturn` length keys
            performMajorGC
            alive keys `shouldReturn` 0
            deRefStablePtr typed >>= (`shouldSatisfy` (> 0)) . Map.size . infoVersions

{- A deferred field can hold more or less than its value, so both directions count. PyPI releases
still defer PEP 440 key parts, yank reasons and file fields: under 120 bytes a release in the corpus.
-}
allowance :: CorpusPackage -> Int -> Integer
allowance package releases =
    4 * 1024 + case pkgEcosystem (cpPackage package) of
        PyPI -> 128 * toInteger releases
        _ -> 0

retained :: Measurement -> Integer
retained result = toInteger (heldLive result) - toInteger (baselineLive result)

report :: CorpusPackage -> Measurement -> Measurement -> IO ()
report package weakHead forced =
    putStrLn . ("metadata-evaluation " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "package" .= cpName package
            , "versions" .= versions forced
            , "weak_head_retained" .= retained weakHead
            , "forced_retained" .= retained forced
            , "allowance" .= allowance package (versions forced)
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

-- The document, its root and each served release or file object.
documentKeys :: CachedDoc -> IO [Weak ()]
documentKeys document =
    (:) <$> track document <*> case (snd npmCached document, snd pypiSimpleCached document) of
        (Just root, _) -> traverse track (root : releases root)
        (_, Just simple) -> (:) <$> track simple <*> traverse (track . snd) (simpleFiles simple)
        _ -> pure []
  where
    track :: a -> IO (Weak ())
    track key = evaluate key >>= \evaluated -> mkWeak evaluated () Nothing
    releases = \case
        Object fields | Just (Object listed) <- KeyMap.lookup "versions" fields -> [release | release@(Object _) <- KeyMap.elems listed]
        _ -> []

alive :: [Weak ()] -> IO Int
alive keys = length . filter isJust <$> traverse deRefWeak keys
