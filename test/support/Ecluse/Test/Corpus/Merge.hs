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

import Data.Aeson (Value (Object, String), eitherDecodeStrict, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo (infoName, infoVersions), pkgEcosystem)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.Corpus.Subset (byPublishTime, keepNpmVersions, keepPyPIVersions, newestVersions)
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
    PublishOrder -> privateOnly (keepVersions info (newestVersions (1 % 3) info))
    HeavyBase -> privateOnly (weighDown info (BS.length bytes `div` 2) . keepVersions info (everyTenth (byPublishTime info)))
  where
    rewrite keep = Rewritten . keep <$> eitherDecodeStrict bytes
    both private public = (,) <$> rewrite (keepVersions info private) <*> rewrite (keepVersions info public)
    privateOnly keep = (,Captured) <$> rewrite keep
    byKey keeps = Set.fromList [key | (position, key) <- zip [0 :: Int ..] (Map.keys (infoVersions info)), keeps position]
    everyTenth keys = Set.fromList [key | (position, key) <- zip [0 :: Int ..] keys, position `mod` 10 == 0]

keepVersions :: PackageInfo -> Set Text -> Value -> Value
keepVersions info = case pkgEcosystem (infoName info) of
    PyPI -> keepPyPIVersions info
    _ -> keepNpmVersions

{- Add rendered text of about the given size. npm takes only its name, author and two time stamps
from the base, so its text goes in each release's deprecation notice, and PyPI's in its status. -}
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
