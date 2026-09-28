-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private and public documents of a two-source listing, drawn from one corpus capture in a
chosen shape, for the residency probe and the load harness.
-}
module Ecluse.Test.Corpus.Merge (
    MergeShape (..),
    MergeDocument (..),
    captureDocuments,
    mergeDocuments,
) where

import Data.Aeson (Value (Array, Object, String), eitherDecodeStrict, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (Artifact (artFilename), PackageDetails (pkgArtifacts, pkgPublishedAt), PackageInfo (infoName, infoVersions), pkgEcosystem)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)

-- | How the private and public documents of a two-source listing share a capture's versions.
data MergeShape
    = -- | Both hold every version, as a private mirror of the whole package does.
      Identical
    | -- | Each holds two thirds of the versions, one third of them in both.
      Overlapping
    | -- | Each holds half of the versions, none of them in both.
      Disjoint
    | -- | The private copy holds the newest third by publish time, and the public one every version.
      PublishOrder
    | {- | The private copy holds every tenth version by publish time and rendered text half the
      capture's size, and the public one every version.
      -}
      HeavyBase
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | One side of a merge: the capture as captured, or a document rewritten from it.
data MergeDocument = Captured | Rewritten Value

-- | 'mergeDocuments' for a corpus capture's bytes, projected under the default limits.
captureDocuments :: MergeShape -> CorpusPackage -> ByteString -> Either String (MergeDocument, MergeDocument)
captureDocuments shape package bytes = do
    info <- case pkgEcosystem name of
        Npm -> bimap show fst (projectNpmManifest defaultLimits name bytes)
        PyPI -> bimap show fst (projectPyPIIndex defaultLimits name bytes)
        RubyGems -> Left "no RubyGems corpus"
    mergeDocuments shape info bytes
  where
    name = cpPackage package

{- | The private and public documents of a merge in the shape, from a capture's bytes and its
projection. A rewritten document keeps the capture's other fields.
-}
mergeDocuments :: MergeShape -> PackageInfo -> ByteString -> Either String (MergeDocument, MergeDocument)
mergeDocuments shape info bytes = case shape of
    Identical -> Right (Captured, Captured)
    Overlapping -> both (byKey (\position -> position `mod` 3 /= 2)) (byKey (\position -> position `mod` 3 /= 0))
    Disjoint -> both (byKey even) (byKey odd)
    PublishOrder -> privateOnly (keepVersions info (newest (length published `div` 3)))
    HeavyBase -> privateOnly (weighDown info (BS.length bytes `div` 2) . keepVersions info (everyTenth published))
  where
    rewrite keep = Rewritten . keep <$> eitherDecodeStrict bytes
    both private public = (,) <$> rewrite (keepVersions info private) <*> rewrite (keepVersions info public)
    privateOnly keep = (,Captured) <$> rewrite keep
    byKey keeps = Set.fromList [key | (position, key) <- zip [0 :: Int ..] (Map.keys (infoVersions info)), keeps position]
    published = map fst (sortOn (\(key, details) -> (pkgPublishedAt details, key)) (Map.toList (infoVersions info)))
    newest count = Set.fromList (drop (length published - max 1 count) published)
    everyTenth keys = Set.fromList [key | (position, key) <- zip [0 :: Int ..] keys, position `mod` 10 == 0]

-- The document with only the given versions, their files, and their timestamps.
keepVersions :: PackageInfo -> Set Text -> Value -> Value
keepVersions info kept = onFields $ \field value -> case (pkgEcosystem (infoName info), field, value) of
    (Npm, "versions", Object releases) -> Object (KeyMap.filterWithKey (\key _ -> Key.toText key `Set.member` kept) releases)
    (Npm, "time", Object times) -> Object (KeyMap.filterWithKey (\key _ -> Key.toText key `Set.notMember` dropped) times)
    (PyPI, "files", Array entries) -> toJSON (filter keptFile (toList entries))
    (PyPI, "versions", Array _) -> toJSON (Set.toList kept)
    _ -> value
  where
    dropped = Map.keysSet (infoVersions info) `Set.difference` kept
    files = Set.fromList [artFilename artifact | details <- Map.elems (Map.restrictKeys (infoVersions info) kept), artifact <- toList (pkgArtifacts details)]
    keptFile = \case
        Object entry | Just (String filename) <- KeyMap.lookup "filename" entry -> filename `Set.member` files
        _ -> False

{- Add rendered text of about the given size. npm's served document keeps no top-level field past its
name, so npm carries the text in each release's deprecation notice, and PyPI in the project status. -}
weighDown :: PackageInfo -> Int -> Value -> Value
weighDown info size = \case
    Object fields -> Object $ case pkgEcosystem (infoName info) of
        Npm | Just (Object releases) <- KeyMap.lookup "versions" fields -> KeyMap.insert "versions" (Object (deprecated releases)) fields
        PyPI -> KeyMap.insert "project-status" (object ["status" .= ("active" :: Text), "reason" .= padded "heavy base" size]) fields
        _ -> fields
    other -> other
  where
    deprecated releases = KeyMap.mapWithKey (\key -> deprecate (padded (Key.toText key) (size `div` max 1 (KeyMap.size releases)))) releases
    deprecate notice = \case
        Object release -> Object (KeyMap.insert "deprecated" (String notice) release)
        other -> other
    padded label width = label <> " " <> T.replicate (max 0 (width - T.length label - 1)) "x"

onFields :: (Text -> Value -> Value) -> Value -> Value
onFields change = \case
    Object fields -> Object (KeyMap.mapWithKey (change . Key.toText) fields)
    other -> other
