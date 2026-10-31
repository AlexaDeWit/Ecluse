-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Frozen corpus facts and rendered bytes record output changes independently from timing.
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

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
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
import Ecluse.Core.Registry.CachedDocument (CachedDoc, weighCachedDoc)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead)
import Ecluse.Core.Server.Conditional (renderETag)
import Ecluse.Core.Server.Pipeline.Packument.Representation (Contribution (..), fingerprintPiece, packumentETag)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Corpus (CaptureUpstream (..), CorpusPackage (cpPackage, cpPath), cpName, syntheticProxyBase)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest, captureVersion)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (metadataAssemble, metadataSerialise))

-- | One ecosystem's reads of a capture, bound to the upstream it was captured from.
data CorpusRead = CorpusRead
    { crUpstream :: CaptureUpstream
    , crRead :: EcosystemRead
    -- ^ The held-body reader, with origin discovery already fixed by the fixture.
    , crMetadata :: MetadataRendering
    -- ^ Assembly and serialisation of the capture's admitted records.
    , crVersionReads :: Either MetadataError VersionRead -> CachedDoc -> Version -> [(Text, LByteString)]
    -- ^ Labelled outputs of one version's selected read, beside the full read's document.
    , crDocumentReads :: PackageName -> ByteString -> [(Text, LByteString)]
    -- ^ Labelled outputs of any other read of the whole capture.
    }

-- | Record byte lengths and SHA-256 for all versions and both end keys, served alone and merged.
captureOutputs :: CorpusRead -> CorpusPackage -> ByteString -> IO (Either Text [Text])
captureOutputs corpus package raw =
    captureManifest (crRead corpus) (crUpstream corpus) name [raw] >>= \case
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
    selected <- captureVersion (crRead corpus) (crUpstream corpus) name version [raw]
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

-- | Preserve the typed facts consumed by admission, serving, and mirroring.
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
line package (label, bytes) = T.intercalate "\t" [captureOutputKey package, label, show (BSL.length bytes), show (hashWith SHA256 (toStrict bytes))]

-- | The recorded lines for one capture, in recorded order.
recordedOutputs :: CorpusPackage -> IO [Text]
recordedOutputs package = recordedOutputsFrom package . lines . decodeUtf8 <$> readFileBS "core/test/unit/fixtures/corpus-outputs.tsv"

-- New captures use versioned paths. Only fixed historical owners receive unqualified keys.
captureOutputKey :: CorpusPackage -> Text
captureOutputKey package = case Map.lookup (pkgEcosystem (cpPackage package), cpPath package) legacyOutputOwners of
    Just owner | owner == cpName package -> owner
    _ -> "v2:" <> toText (cpPath package)

-- Preserve owned rows and their order, including malformed output fields for the exact comparison.
recordedOutputsFrom :: CorpusPackage -> [Text] -> [Text]
recordedOutputsFrom package = filter ((== captureOutputKey package) . T.takeWhile (/= '\t'))

-- This map records v1 ownership, not current package discovery.
legacyOutputOwners :: Map (Ecosystem, FilePath) Text
legacyOutputOwners =
    Map.fromList
        [ ((Npm, "bench/corpus/npm/types-node.full.json"), "@types/node")
        , ((Npm, "bench/corpus/npm/webpack.full.json"), "webpack")
        , ((Npm, "bench/corpus/npm/aws-sdk-client-s3.full.json"), "@aws-sdk/client-s3")
        , ((Npm, "bench/corpus/npm/express.full.json"), "express")
        , ((Npm, "bench/corpus/npm/typescript.full.json"), "typescript")
        , ((Npm, "bench/corpus/npm/babel-core.full.json"), "@babel/core")
        , ((Npm, "bench/corpus/npm/react.full.json"), "react")
        , ((Npm, "bench/corpus/npm/request.full.json"), "request")
        , ((Npm, "bench/corpus/npm/lodash.full.json"), "lodash")
        , ((PyPI, "bench/corpus/pypi/boto3.simple.json"), "boto3")
        , ((PyPI, "bench/corpus/pypi/numpy.simple.json"), "numpy")
        , ((PyPI, "bench/corpus/pypi/requests.simple.json"), "requests")
        , ((Cargo, "bench/corpus/cargo/aws-sdk-ec2.index.ndjson"), "aws-sdk-ec2")
        , ((Cargo, "bench/corpus/cargo/windows.index.ndjson"), "windows")
        , ((Cargo, "bench/corpus/cargo/tokio.index.ndjson"), "tokio")
        ]
