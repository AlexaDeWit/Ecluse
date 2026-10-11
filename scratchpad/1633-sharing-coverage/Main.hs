-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Bounded source reads for sharing coverage, with unchanged output witnesses.
module Main (main) where

import Relude

import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import GHC.Conc (getNumCapabilities)
import System.Environment (getArgs)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (infoVersions, pkgEcosystem)
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry (RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionDoc (vdDetails), VersionRead (vrVersion))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Npm.Metadata (selectNpmVersionDoc)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Test.Registry.Npm.Project (parseVersionList)
#ifdef SHARING_STATS
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes), defaultLimits)
#else
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
#endif
import Ecluse.Core.Server.Conditional (evaluateETag, renderETag)
import Ecluse.Core.Server.Pipeline.Origin (Contribution (..), fingerprintPiece)
import Ecluse.Core.Server.Pipeline.Packument (packumentETag)
import Ecluse.Test.Corpus (CaptureUpstream (upstreamOrigin), CorpusPackage (..), CorpusTier (Large, Medium), corpusPackages, cpName, npmCaptureUpstream, pypiCaptureUpstream, pypiCorpusPackages, syntheticProxyBase)
import Ecluse.Test.Corpus.Merge (MergeDocument (Captured, Rewritten), MergeShape (Overlapping), captureDocuments)
import Ecluse.Test.Corpus.Outputs (CorpusRead (..), captureOutputs, recordedOutputs, releaseFacts, rendered, selectedFacts)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks, captureManifest)
import Ecluse.Test.Snapshot (digestOf)
import Network.HTTP.Types.Header (hIfNoneMatch)

#ifdef SHARING_STATS
import Control.Monad.ST (stToIO)
import Data.Aeson.KeyMap qualified as KeyMap
import Ecluse.Core.Registry.Json.Intern (SipKey (..), newInternTable)
import Ecluse.Core.Registry.Json.Shape (Mode (Share, Keep), Shape (Generic, ObjectWith), knownMembers, prepareMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Walk (finish), readJsonWalkST, withElement)
import Ecluse.Core.Registry.Json.Writer (newWriter, writerStatistics)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Test.Registry.JsonStream (heldChunks)
#endif

-- | Read four captures and their overlapping sources, preserving failed partials.
main :: IO ()
main = do
    arguments <- getArgs
    directory <- case arguments of
        [path] -> pure path
        _ -> fail "usage: sharing-coverage OUTPUT_DIRECTORY"
    capabilities <- getNumCapabilities
    emit directory "runtime" (object ["capabilities" .= capabilities, "defaultLimits" .= (show defaultLimits :: Text), "chunkBytes" .= (32768 :: Int)])
    scalarFixture directory
    forM_ [(adapterMetadata npmAdapter, npmCaptureUpstream, selected corpusPackages), (adapterMetadata pypiAdapter, pypiCaptureUpstream, selected pypiCorpusPackages)] $ \(metadata, upstream, packages) ->
        forM_ packages (measurePackage directory metadata upstream)
  where
    selected = filter (\package -> cpTier package == Medium || cpTier package == Large) . filter (\package -> cpName package `elem` ["lodash", "typescript", "requests", "numpy"])

corpusRead :: AdapterMetadata -> CaptureUpstream -> CorpusPackage -> CorpusRead
corpusRead metadata upstream package = CorpusRead upstream metadata versions documents
  where
    versions selected document version = case pkgEcosystem (cpPackage package) of
        Npm -> [("selected", rendered (selectedFacts <$> selected)), ("mirror", rendered (selectNpmVersionDoc version document))]
        PyPI -> [("selected", rendered (fmap (releaseFacts . vdDetails) . vrVersion <$> selected))]
        RubyGems -> []
    documents _ raw = case pkgEcosystem (cpPackage package) of
        Npm -> [("versions", rendered (parseVersionList (RegistryResponse 200 (BS.length raw) raw)))]
        PyPI -> []
        RubyGems -> []

preflight :: FilePath -> FilePath -> AdapterMetadata -> CaptureUpstream -> CorpusPackage -> Text -> ByteString -> IO ()
preflight directory label metadata upstream package source raw = do
    putTextLn ("PREFLIGHT " <> toText label <> " " <> source)
    actual <- captureOutputs (corpusRead metadata upstream package) package raw >>= either (fail . toString) pure
    emit directory (label <> "-" <> toString source <> "-preflight") (object ["outputs" .= actual])
    when (source == "captured") $ do
        expected <- recordedOutputs package
        unless (actual == expected) (fail "complete frozen corpus preflight differs")

measurePackage :: FilePath -> AdapterMetadata -> CaptureUpstream -> CorpusPackage -> IO ()
measurePackage directory metadata upstream package = do
    raw <- BS.readFile (cpPath package)
    captured <- readSource "captured" raw
    (private, public) <- either fail pure (captureDocuments Overlapping package raw)
    privateManifest <- readSource "overlapping-private" (sourceBytes raw private)
    publicManifest <- readSource "overlapping-public" (sourceBytes raw public)
    emit directory label (object ["captured" .= outcome captured, "private" .= outcome privateManifest, "public" .= outcome publicManifest])
    case (privateManifest, publicManifest) of
        (Right privateRead, Right publicRead) -> do
            serveWitness directory (label <> "-merge") metadata upstream package [(TrustedSource, privateRead), (GatedSource, publicRead)]
        _ -> fail "bounded source read failed. Partial rows remain in the output directory."
  where
    label = toString (show (pkgEcosystem (cpPackage package)) :: Text) <> "-" <> toString (cpName package)
    readSource source bytes = do
        BS.writeFile (directory <> "/" <> label <> "-" <> toString source <> ".json") bytes
        emit directory (label <> "-" <> toString source <> "-input") (object ["name" .= cpName package, "tier" .= (show (cpTier package) :: Text), "source" .= source, "bytes" .= BS.length bytes, "effectiveBodyCap" .= max (maxMetadataBytes defaultLimits) (BS.length bytes)])
        preflight directory label metadata upstream package source bytes
        putTextLn ("BEGIN " <> toText label <> " " <> source)
        result <- captureManifest metadata upstream (cpPackage package) (captureChunks bytes)
        putTextLn ("END " <> toText label <> " " <> source <> " " <> either show (const "success") result)
        forM_ (rightToMaybe result) $ \manifest -> do
            let provenance = if source == "overlapping-private" then TrustedSource else GatedSource
            serialised <- either (fail . show) pure (metadataSerialise metadata (manifestRaw manifest))
            BL.writeFile (directory <> "/" <> label <> "-" <> toString source <> "-serialised.json") serialised
            serveWitness directory (label <> "-" <> toString source <> "-served") metadata upstream package [(provenance, manifest)]
        pure result
    outcome = either (object . (: []) . ("failure" .=) . (show :: MetadataError -> Text)) $ \manifest ->
        object ["bodyBytes" .= manifestBodyBytes manifest, "digest" .= (show (manifestDigest manifest) :: Text), "versions" .= Map.size (infoVersions (manifestInfo manifest)), "info" .= (show (manifestInfo manifest) :: Text), "facts" .= [(key, releaseFacts facts) | (key, facts) <- Map.toAscList (infoVersions (manifestInfo manifest))]]

serveWitness :: FilePath -> FilePath -> AdapterMetadata -> CaptureUpstream -> CorpusPackage -> [(Provenance, Manifest)] -> IO ()
serveWitness directory label metadata upstream package inputs = do
    let sources = [Contribution provenance (manifestInfo manifest) (manifestRaw manifest) (manifestDigest manifest) (manifestBodyBytes manifest) | (provenance, manifest) <- inputs]
    plan <- maybe (fail "bounded source has no plan") pure (mergePackuments [(srcProvenance source, Snapshot (srcDigest source) (srcInfo source)) | source <- sources])
    let documents = Map.fromList (zip [0 ..] [Snapshot (srcDigest source) (srcValue source) | source <- sources])
        baseDocument = case inputs of
            (_, manifest) : _ -> Just (manifestRaw manifest)
            [] -> Nothing
        etag = packumentETag syntheticProxyBase (upstreamOrigin upstream <$ sources) (cpPackage package) (map fingerprintPiece sources)
        conditional headers = show (evaluateETag headers etag) :: Text
    served <- either (fail . show) pure (metadataSerialise metadata (metadataAssemble metadata syntheticProxyBase documents plan baseDocument))
    BL.writeFile (directory <> "/" <> label <> ".json") served
    emit directory label (object ["plan" .= (show plan :: Text), "servedBytes" .= BL.length served, "servedDigest" .= (show (digestOf (BL.toStrict served)) :: Text), "etag" .= renderETag etag, "withoutValidator" .= conditional [], "matchingValidator" .= conditional [(hIfNoneMatch, encodeUtf8 (renderETag etag))], "staleValidator" .= conditional [(hIfNoneMatch, "\"stale\"")]])

sourceBytes :: ByteString -> MergeDocument -> ByteString
sourceBytes captured = \case
    Captured -> captured
    Rewritten value -> BL.toStrict (encode value)

emit :: FilePath -> FilePath -> Value -> IO ()
emit directory label value = BL.writeFile (directory <> "/" <> label <> "-result.json") (encode value)

#ifdef SHARING_STATS
scalarFixture :: FilePath -> IO ()
scalarFixture directory = do
    let raw = "{\"a\":\"same\",\"b\":\"same\",\"unique\":\"x\",\"url\":\"same\",\"escaped\":\"s\\u0061me\",\"unicode\":\"\\u00e9\",\"a\":\"drop\"}"
        table = newInternTable (SipKey 1 2) ["url"]
        prepared = ObjectWith 8 (prepareMembers table (knownMembers ["a", "b", "unique", "url", "escaped", "unicode"] (Generic 7))) (Generic 8)
        common = [([("domain", "observer-context"), ("action", "underflow")], 0), ([("role", "member-key"), ("policy", "share")], 7), ([("role", "string-value"), ("policy", "share")], 5), ([("role", "string-value"), ("policy", "keep")], 2), ([("role", "member-key"), ("lookup", "hit")], 2), ([("role", "member-key"), ("lookup", "miss")], 5), ([("role", "string-value"), ("lookup", "hit")], 2), ([("role", "string-value"), ("lookup", "miss")], 3), ([("role", "string-value"), ("input", "decoded")], 2), ([("role", "string-value"), ("policy", "share"), ("occurrence", "repeated")], 2), ([("utf8Bytes", "2"), ("jsonBytes", "4"), ("characters", "1")], 1)]
    check "scalar-ordinary" raw (Generic 8) Share (common <> [([("action", "member-added")], 6), ([("action", "duplicate-value-read-then-drop")], 1), ([("action", "packed-member-dropped")], 1), ([("domain", "packed-shape"), ("pathDepth", "1")], 14)] <> [([("name", "prepared")], 0)])
    check "scalar-suspended" raw (Generic 8) Share [([("role", "member-key"), ("policy", "share")], 7), ([("role", "string-value"), ("policy", "share")], 5), ([("role", "string-value"), ("policy", "keep")], 2), ([("domain", "packed-shape"), ("pathDepth", "1")], 14), ([("action", "packed-member-dropped")], 1)]
    check "scalar-prepared" raw prepared Share (common <> [([("role", "member-key"), ("name", "prepared")], 7)])
    check "scalar-keep" "{\"a\":\"same\",\"b\":\"same\"}" (Generic 8) Keep [([("role", "member-key"), ("policy", "keep")], 2), ([("role", "string-value"), ("policy", "keep")], 2), ([("role", "string-value"), ("occurrence", "repeated")], 1)]
  where
    check label raw shape mode expected = do
        writer <- stToIO (newWriter Nothing)
        chunks <- heldChunks (if label == "scalar-suspended" then map BS.singleton (BS.unpack raw) else [raw])
        result <- readJsonWalkST stToIO (MetadataBodyLimit (BS.length raw)) (\cursor -> withElement cursor $ \element rest -> readShape writer shape mode (newInternTable (SipKey 1 2) ["url"]) element rest (\() _ _ -> finish ())) chunks
        streamed <- either (fail . show) pure result
        either (fail . show) pure (streamValue streamed)
        statistics <- stToIO (writerStatistics writer)
        emit directory label statistics
        case statistics of
            Object fields | Just (Array pending) <- KeyMap.lookup "openPath" fields, null pending -> pure ()
            _ -> fail (label <> ": observer context remains open")
        forM_ expected $ \(dimensions, count) ->
            unless (matching dimensions statistics == count) (fail (label <> ": independently known count differs for " <> toString (show dimensions :: Text)))

matching :: [(Text, Text)] -> Value -> Int
matching constraints statistics = sum [count | (dimensions, count) <- rows statistics, let fields = Map.fromList (zip columns dimensions), all (\(column, value) -> Map.lookup column fields == Just value) constraints]
  where
    columns = ["role", "policy", "input", "lookup", "name", "utf8Bytes", "jsonBytes", "characters", "occurrence", "domain", "path", "pathDepth", "disposition", "action"]

rows :: Value -> [([Text], Int)]
rows (Object fields) = case KeyMap.lookup "rows" fields of
    Just (Array values) -> mapMaybe row (toList values)
    _ -> []
  where
    row (Object fields') = do
        Array dimensions <- KeyMap.lookup "dimensions" fields'
        Number count <- KeyMap.lookup "count" fields'
        names <- traverse (\case String text -> Just text; _ -> Nothing) (toList dimensions)
        pure (names, round count)
    row _ = Nothing
rows _ = []
#else
scalarFixture :: FilePath -> IO ()
scalarFixture _ = pure ()
#endif
