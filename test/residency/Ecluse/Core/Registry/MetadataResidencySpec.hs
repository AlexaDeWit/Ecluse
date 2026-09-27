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

import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Server.MemoryModel.Probe (Evaluated (..), measureInChild, packages, project)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), cpName)

-- | Every corpus capture, measured in a fresh child process and collected in this one.
spec :: Spec
spec = describe "metadata read evaluation" $ forM_ packages $ \package -> do
    it (toString (cpName package) <> " retains the same at weak head as fully forced") $
        measureInChild ["--metadata-evaluation-probe"] package >>= \case
            Left failure -> expectationFailure failure
            Right result -> do
                report package result
                abs (forcing result) `shouldSatisfy` (<= allowance)

    it (toString (cpName package) <> " releases the served document while its typed view is held") $
        bracket (detach package) (\(typed, _, _) -> freeStablePtr typed) $ \(typed, document, keys) -> do
            length keys `shouldSatisfy` (> 2)
            ((performMajorGC >> alive keys) `finally` freeStablePtr document) `shouldReturn` length keys
            performMajorGC
            alive keys `shouldReturn` 0
            deRefStablePtr typed >>= (`shouldSatisfy` (> 0)) . Map.size . infoVersions

-- | The largest change forcing may make to live bytes, in either direction.
allowance :: Integer
allowance = 1024

-- | Live bytes that forcing the rooted result added, or released when negative.
forcing :: Evaluated -> Integer
forcing result = toInteger (evaluatedForced result) - toInteger (evaluatedWeakHead result)

report :: CorpusPackage -> Evaluated -> IO ()
report package result =
    putStrLn . ("metadata-evaluation " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "package" .= cpName package
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
documentKeys :: CachedDoc -> IO [Weak ()]
documentKeys document =
    (<>) <$> sequence [track document] <*> case (snd npmCached document, snd pypiSimpleCached document) of
        (Just root, _) -> concat <$> traverse trackObject (root : releases root)
        (_, Just simple) -> (<>) <$> trackMembers (simpleEnvelope simple) <*> (concat <$> traverse (trackObject . snd) (simpleFiles simple))
        _ -> pure []
  where
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
