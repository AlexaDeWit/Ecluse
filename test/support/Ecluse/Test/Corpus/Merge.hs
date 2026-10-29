-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Corpus merge shapes preserve native records for the existing residency sampler.
module Ecluse.Test.Corpus.Merge (
    MergeShape (..),
    realisticShape,
    MergeDocument (..),
    captureDocuments,
    mergeDocuments,
) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set

import Ecluse.Core.Package (PackageInfo (infoName, infoVersions), pkgEcosystem)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.Corpus.Subset (byPublishTime, newestVersions, oldestVersions)
import Ecluse.Test.EcosystemBench (CorpusFraming (..), EcosystemBench (ebFraming, ebHeavyBase, ebKeepVersions, ebTreeRead), ecosystemBenchFor)

-- | How the private and public documents of a two-source listing share a capture's versions.
data MergeShape
    = -- | Both hold every version, as a private mirror of the whole package does.
      Identical
    | -- | Each holds two thirds of the versions, one third of them in both.
      Overlapping
    | -- | Each holds half of the versions, none of them in both.
      Disjoint
    | -- | The private copy holds the newest quarter by publish time, and the public copy every version.
      PublishOrder
    | -- | The private copy holds every tenth version and text half the capture's size.
      HeavyBase
    | -- | 'HeavyBase' with the oldest tenth of the versions, whose releases are the smallest.
      HeavyOldBase
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Whether the shape models a private copy a deployment serves, not a stress on the basis.
realisticShape :: MergeShape -> Bool
realisticShape = \case
    Identical -> True
    Overlapping -> True
    Disjoint -> True
    PublishOrder -> True
    HeavyBase -> False
    HeavyOldBase -> False

-- | One side of a merge: the capture as captured, or a document rewritten from it.
data MergeDocument = Captured | Rewritten Value | RewrittenNative ByteString

-- | 'mergeDocuments' for a corpus capture's bytes, projected under the default limits.
captureDocuments :: MergeShape -> CorpusPackage -> ByteString -> Either String (MergeDocument, MergeDocument)
captureDocuments shape package bytes = do
    ecosystem <- maybeToRight "no corpus provider" (ecosystemBenchFor (pkgEcosystem name))
    info <- bimap show fst (ebTreeRead ecosystem name bytes)
    mergeDocuments shape info bytes
  where
    name = cpPackage package

-- | Rewrite the private and public documents, preserving fields outside the selected shape.
mergeDocuments :: MergeShape -> PackageInfo -> ByteString -> Either String (MergeDocument, MergeDocument)
mergeDocuments shape info bytes = do
    ecosystem <- maybeToRight "no corpus provider" (ecosystemBenchFor (pkgEcosystem (infoName info)))
    shapedDocuments ecosystem
  where
    shapedDocuments ecosystem = case shape of
        Identical -> Right (Captured, Captured)
        Overlapping -> both (byKey (\position -> position `mod` 3 /= 2)) (byKey (\position -> position `mod` 3 /= 0))
        Disjoint -> both (byKey even) (byKey odd)
        PublishOrder -> privateOnly (ebKeepVersions ecosystem info (newestVersions (1 % 4) info))
        HeavyBase -> privateOnly (ebHeavyBase ecosystem (BS.length bytes `div` 2) . ebKeepVersions ecosystem info (everyTenth (byPublishTime info)))
        HeavyOldBase -> privateOnly (ebHeavyBase ecosystem (BS.length bytes `div` 2) . ebKeepVersions ecosystem info (oldestVersions (1 % 10) info))
      where
        rewrite keep = case ebFraming ecosystem of
            JsonFraming -> Rewritten . keep <$> eitherDecodeStrict bytes
            NativeFraming decodeValues encodeValues -> RewrittenNative <$> (first toString (decodeValues bytes) >>= first toString . encodeValues . keep)
        both private public = (,) <$> rewrite (ebKeepVersions ecosystem info private) <*> rewrite (ebKeepVersions ecosystem info public)
        privateOnly keep = (,Captured) <$> rewrite keep
        byKey keeps = Set.fromList [key | (position, key) <- zip [0 :: Int ..] (Map.keys (infoVersions info)), keeps position]
        everyTenth keys = Set.fromList [key | (position, key) <- zip [0 :: Int ..] keys, position `mod` 10 == 0]
