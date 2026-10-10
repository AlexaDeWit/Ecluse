-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The typed facts, served documents, ETags and single-version reads of each corpus capture, as
byte lengths and SHA-256 digests. @core/test/unit/fixtures/corpus-outputs.tsv@ records them, so a
change to how a read holds its result must reproduce each output byte for byte.
-}
module Ecluse.Test.Corpus.Outputs (
    CorpusRead (..),
    captureOutputs,
    recordedOutputs,
    rendered,
    releaseFacts,
    selectedFacts,
) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Package (
    Artifact (..),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    hashAlg,
    hashValue,
 )
import Ecluse.Core.Package.Filter (restrictToSurvivors)
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, weighCachedDoc)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo, manifestRaw), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Server.Conditional (renderETag)
import Ecluse.Core.Server.Pipeline.Origin (Contribution (..), fingerprintPiece)
import Ecluse.Core.Server.Pipeline.Packument (packumentETag)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (CaptureUpstream (..), CorpusPackage (cpPackage), cpName, syntheticProxyBase)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest)
import Ecluse.Test.Snapshot (digestOf)

-- | One ecosystem's reads of a capture, bound to the upstream it was captured from.
data CorpusRead = CorpusRead
    { crUpstream :: CaptureUpstream
    , crMetadata :: AdapterMetadata
    -- ^ The adapter whose production full read the outputs come from.
    , crVersionReads :: PackageName -> ByteString -> CachedDoc -> Text -> [(Text, LByteString)]
    -- ^ Labelled outputs of the reads that select one version key.
    , crDocumentReads :: PackageName -> ByteString -> [(Text, LByteString)]
    -- ^ Labelled outputs of any other read of the whole capture.
    }

{- | One tab-separated line per output of the production full read: capture, label, byte length and
SHA-256. Survivor sets are all versions, the least key and the greatest key, each served alone and merged with itself.
-}
captureOutputs :: CorpusRead -> CorpusPackage -> ByteString -> IO (Either Text [Text])
captureOutputs corpus package raw =
    (first show >=> manifestOutputs corpus package raw) <$> captureManifest (crMetadata corpus) (crUpstream corpus) (cpPackage package) [raw]

manifestOutputs :: CorpusRead -> CorpusPackage -> ByteString -> Manifest -> Either Text [Text]
manifestOutputs corpus package raw manifest = do
    let info = manifestInfo manifest
        document = manifestRaw manifest
        keys = Map.keysSet (infoVersions info)
        ends = [("first", Set.lookupMin keys), ("last", Set.lookupMax keys)]
        survivorSets = ("all", keys) : [(label, maybe mempty Set.singleton key) | (label, key) <- ends]
    served <- concat <$> traverse (servedOutputs corpus name raw document info) survivorSets
    pure $
        map (line package) $
            [("typed", typedFacts info), ("charge", rendered (weighCachedDoc document))]
                <> served
                <> [(label <> "/" <> output, bytes) | (label, Just key) <- ends, (output, bytes) <- crVersionReads corpus name raw document key]
                <> crDocumentReads corpus name raw
  where
    name = cpPackage package

servedOutputs :: CorpusRead -> PackageName -> ByteString -> CachedDoc -> PackageInfo -> (Text, Set Text) -> Either Text [(Text, LByteString)]
servedOutputs corpus name raw document info (label, survivors) =
    concat <$> traverse render [("single", [GatedSource]), ("merged", [TrustedSource, GatedSource])]
  where
    restricted = restrictToSurvivors survivors info
    render (shape, provenances) = do
        let sources = [Contribution provenance restricted document (digestOf raw) (BS.length raw) | provenance <- provenances]
        plan <- maybeToRight "no merge plan" (mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources])
        let bySource = Map.fromList (zip [0 ..] [Snapshot (srcDigest s) (srcValue s) | s <- sources])
        body <- first (const "the render refused its plan") (metadataSerialise (crMetadata corpus) (metadataAssemble (crMetadata corpus) syntheticProxyBase bySource plan (Just document)))
        let etag = packumentETag syntheticProxyBase (upstreamOrigin (crUpstream corpus) <$ sources) name (map fingerprintPiece sources)
            prefix = shape <> "/" <> label <> "/"
        pure [(prefix <> "plan", rendered plan), (prefix <> "served", body), (prefix <> "etag", encodeUtf8 (renderETag etag))]

-- | A value's derived 'Show' rendering as UTF-8 bytes.
rendered :: (Show a) => a -> LByteString
rendered value = encodeUtf8 (show value :: Text)

-- The typed view as its name, tags and dropped entries, then one line of 'releaseFacts' per version.
typedFacts :: PackageInfo -> LByteString
typedFacts info =
    encodeUtf8 . T.unlines $
        show (infoName info, infoDistTags info, infoInvalidEntries info)
            : [key <> "\t" <> releaseFacts details | (key, details) <- Map.toAscList (infoVersions info)]

{- | A typed release's availability and what the rules, merge, ETag, assembly and mirror read from
it. A change to the typed model keeps each of these facts, so its rendering stays byte for byte.
-}
releaseFacts :: PackageDetails -> Text
releaseFacts details =
    show
        ( pkgName details
        , pkgVersion details
        , pkgPublishedAt details
        , pkgInstallCode details
        , pkgAvailability details
        , [artifactFacts art | art <- toList (pkgArtifacts details)]
        )
  where
    artifactFacts art = (artEntryKey art, artFilename art, artUrl art, [(hashAlg h, hashValue h) | h <- artHashes art], artSize art)

-- | A selected read with its typed release reduced to 'releaseFacts'.
selectedFacts :: VersionRead -> (Maybe (Text, Maybe CachedDoc), Int, Maybe Version)
selectedFacts selected =
    ( (\doc -> (releaseFacts (vdDetails doc), vdRaw doc)) <$> vrVersion selected
    , vrBodyBytes selected
    , vrUpstreamLatest selected
    )

line :: CorpusPackage -> (Text, LByteString) -> Text
line package (label, bytes) = T.intercalate "\t" [cpName package, label, show (BSL.length bytes), show (hashWith SHA256 (toStrict bytes))]

-- | The recorded lines for one capture, in recorded order.
recordedOutputs :: CorpusPackage -> IO [Text]
recordedOutputs package = filter ((== cpName package) . T.takeWhile (/= '\t')) . lines . decodeUtf8 <$> readFileBS "core/test/unit/fixtures/corpus-outputs.tsv"
