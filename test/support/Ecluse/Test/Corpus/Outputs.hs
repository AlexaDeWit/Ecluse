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
    pkgEcosystem,
 )
import Ecluse.Core.Package.Filter (restrictToSurvivors)
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, weighCachedDoc)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Server.Conditional (renderETag)
import Ecluse.Core.Server.Pipeline.Origin (Contribution (..), fingerprintPiece)
import Ecluse.Core.Server.Pipeline.Packument (packumentETag)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Corpus (CaptureUpstream (..), CorpusPackage (cpPackage), cpName, syntheticProxyBase)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest, captureVersion)

-- | One ecosystem's reads of a capture, bound to the upstream it was captured from.
data CorpusRead = CorpusRead
    { crUpstream :: CaptureUpstream
    , crMetadata :: AdapterMetadata
    -- ^ The adapter whose production full and selected reads the outputs come from.
    , crVersionReads :: Either MetadataError VersionRead -> CachedDoc -> Version -> [(Text, LByteString)]
    -- ^ Labelled outputs of one version's selected read, beside the full read's document.
    , crDocumentReads :: PackageName -> ByteString -> [(Text, LByteString)]
    -- ^ Labelled outputs of any other read of the whole capture.
    }

{- | One tab-separated line per output of the production reads: capture, label, length and SHA-256.
The survivor sets are all versions, the least key and the greatest, each served alone and merged.
-}
captureOutputs :: CorpusRead -> CorpusPackage -> ByteString -> IO (Either Text [Text])
captureOutputs corpus package raw =
    captureManifest (crMetadata corpus) (crUpstream corpus) name [raw] >>= \case
        Left refused -> pure (Left (show refused))
        Right manifest -> do
            let keys = Map.keysSet (infoVersions (manifestInfo manifest))
                ends = [("first", Set.lookupMin keys), ("last", Set.lookupMax keys)]
            selected <- concat <$> traverse (versionOutputs corpus name raw (manifestRaw manifest)) [(label, key) | (label, Just key) <- ends]
            pure (manifestOutputs corpus package manifest (("all", keys) : map (second (maybe mempty Set.singleton)) ends) (selected <> crDocumentReads corpus name raw))
  where
    name = cpPackage package

-- The ecosystem's outputs of one version key's production selected read, under the key's label.
versionOutputs :: CorpusRead -> PackageName -> ByteString -> CachedDoc -> (Text, Text) -> IO [(Text, LByteString)]
versionOutputs corpus name raw document (label, key) = do
    selected <- captureVersion (crMetadata corpus) (crUpstream corpus) name version [raw]
    pure [(label <> "/" <> output, bytes) | (output, bytes) <- crVersionReads corpus selected document version]
  where
    version = mkVersion (pkgEcosystem name) key

manifestOutputs :: CorpusRead -> CorpusPackage -> Manifest -> [(Text, Set Text)] -> [(Text, LByteString)] -> Either Text [Text]
manifestOutputs corpus package manifest survivorSets otherReads = do
    served <- concat <$> traverse (servedOutputs corpus (cpPackage package) manifest) survivorSets
    pure . map (line package) $
        [("typed", typedFacts (manifestInfo manifest)), ("charge", rendered (weighCachedDoc (manifestRaw manifest)))] <> served <> otherReads

servedOutputs :: CorpusRead -> PackageName -> Manifest -> (Text, Set Text) -> Either Text [(Text, LByteString)]
servedOutputs corpus name manifest (label, survivors) =
    concat <$> traverse render [("single", [GatedSource]), ("merged", [TrustedSource, GatedSource])]
  where
    document = manifestRaw manifest
    restricted = restrictToSurvivors survivors (manifestInfo manifest)
    render (shape, provenances) = do
        let sources = [Contribution provenance restricted document (manifestDigest manifest) (manifestBodyBytes manifest) | provenance <- provenances]
        plan <- maybeToRight "no merge plan" (mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources])
        let bySource = Map.fromList (zip [0 ..] [Snapshot (srcDigest s) (srcValue s) | s <- sources])
        body <- first (const "the render refused its plan") (metadataSerialise (crMetadata corpus) (metadataAssemble (crMetadata corpus) syntheticProxyBase bySource plan (Just document)))
        let etag = packumentETag syntheticProxyBase (upstreamOrigin (crUpstream corpus) <$ sources) name (map fingerprintPiece sources)
            prefix = shape <> "/" <> label <> "/"
        pure [(prefix <> "plan", rendered plan), (prefix <> "served", body), (prefix <> "etag", encodeUtf8 (renderETag etag))]

-- | A value's derived 'Show' rendering as UTF-8 bytes.
rendered :: (Show a) => a -> LByteString
rendered value = encodeUtf8 (show value :: Text)

-- The typed view as its name, tags and dropped entries, then a line of 'releaseFacts' per version.
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
