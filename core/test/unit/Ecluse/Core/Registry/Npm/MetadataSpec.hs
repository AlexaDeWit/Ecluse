-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Differential checks for full and selective npm metadata reads.
module Ecluse.Core.Registry.Npm.MetadataSpec (spec) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), decode, eitherDecodeStrict, encode, object, parseJSON, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.JsonStream.Lexer (Cursor)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Hedgehog (MonadTest, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Types.Header (hIfNoneMatch)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (artEntryKey, artHashes),
    Availability (Deprecated),
    CodeExecSignal (RunsCodeOnInstall),
    InvalidEntry,
    InvalidEntryKind (..),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    invalidKey,
    invalidKind,
    invalidValue,
    mkInvalidEntry,
    renderPackageName,
 )
import Ecluse.Core.Package.Entry (EntryKey (ObjectEntry))
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, npmPacked)
import Ecluse.Core.Registry.Json.Intern (InternTable, Interned (..), decodedName, internName, tableTexts)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, docTable, packedEncodedLength, withoutHole)
import Ecluse.Core.Registry.Json.Walk (Steps, Walked (..))
import Ecluse.Core.Registry.Json.Writer (Pick (..), Writer, decodePicked, decodeWhole, discard, newWriter, replacedMember, sealValue)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
    VersionDoc (vdRaw),
    VersionRead (vrBodyBytes, vrUpstreamLatest, vrVersion),
 )
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, streamError, validateReportedName)
import Ecluse.Core.Registry.Npm.DependencySource (installableFieldName)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..), tarballHole, tarballUrl)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedDocument, serialiseMergedDocument)
import Ecluse.Core.Registry.Npm.Metadata (npmPackumentWalk, projectNpmPacked, projectNpmStream, selectNpmVersionDoc)
import Ecluse.Core.Registry.Npm.Project (projectName, projectVersionEntryResult)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (WholePackument), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Request (packageUrl)
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmFieldOf (..), versionListFields)
import Ecluse.Core.Registry.Npm.Wire (distFields)
import Ecluse.Core.Registry.ServedDocument (RenderRefused, rebaseArtifactUrl)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement, parsePublishTime)
import Ecluse.Core.Security (
    BodyLimit (MetadataBodyLimit),
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions),
    Limits (maxMetadataBytes, maxNestingDepth, maxVersionCount),
    checkVersionCountOf,
    defaultLimits,
 )
import Ecluse.Core.Server.Conditional (Conditional (..), ETag, evaluateETag, renderETag)
import Ecluse.Core.Server.Pipeline.Packument.Representation (packumentETag)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Contract.Metadata.Projection (MetadataFraming (SingleDocument), ProjectionDriver (..), metadataProjectionContract)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), corpusPackages, cpName)
import Ecluse.Test.Json (fieldAt, isObject, withKeys)
import Ecluse.Test.Package (unscopedNpm, validSha1, validSha512Sri)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genPackumentBytes, genServablePackumentBytes)
import Ecluse.Test.Registry.JsonStream (byteChunks, testTable, walkJsonChunks, walkWritingChunks)
import Ecluse.Test.Registry.Npm.Metadata (npmFullTestWalk, projectNpmManifest, projectNpmVersion)
import Ecluse.Test.Registry.Packed (walkedLength)
import Ecluse.Test.Snapshot (digestOf, readDetails)
import Ecluse.Test.Support (expectRight)

-- | Metadata projection outcomes, including duplicate-key and optional-container parity.
spec :: Spec
spec = do
    metadataProjectionContract "npm" projectionDriver
    projectNpmManifestSpec
    projectNpmVersionSpec
    packedReadSpec
    integratedFactsSpec

packedReadSpec :: Spec
packedReadSpec = describe "packed full read" $ do
    -- Fewer tests let "a tarball served as read" fall under its bar by chance.
    modifyMaxSuccess (const 2000) $
        it "reads what the tree read reads, for hostile packuments, chunkings and damage" $
            hedgehog $ do
                body <- forAll (Gen.choice [genPackumentBytes, genServablePackumentBytes] >>= damaged)
                chunks <- forAll (genChunks body)
                let (tree, packed) = bothFullReads chunks
                coverListing (mergedListing [tree])
                fmap (second (snd npmCached)) tree === fmap (second (snd npmCached)) packed
                served [tree] === served [packed]
                fmap snd (packedFullRead chunks) === baseFullRead (readLimits chunks) (packageNameOf chunks) (bodyLimit chunks) chunks

    modifyMaxSuccess (const 1000) $
        it "renders a listing merged from two sources, each packed over its own table, as the tree reads do" $
            hedgehog $ do
                sources <- forAll (Gen.list (Range.singleton 2) genServablePackumentBytes)
                let fullReads = map (bothFullReads . one) sources
                coverListing (mergedListing (map fst fullReads))
                cover 20 "a survivor from the second source" (maybe False (elem 1 . mpSurvivors . fst) (mergedListing (map fst fullReads)))
                served (map fst fullReads) === served (map snd fullReads)

    forM_ corpusPackages $ \package -> do
        it ("packs, decodes and renders every release of the capture " <> cpPath package) $ do
            bytes <- readFileBS (cpPath package)
            let (tree, packed) = bothFullReads [bytes]
            isRight tree `shouldBe` True
            fmap (second (snd npmCached)) tree `shouldBe` fmap (second (snd npmCached)) packed
            served [tree] `shouldBe` served [packed]
            fmap snd (packedFullRead [bytes]) `shouldBe` baseFullRead (readLimits [bytes]) (packageNameOf [bytes]) (bodyLimit [bytes]) [bytes]

        it ("stores the length a walk of the release measures, with and without its hole, for every release of the capture " <> cpPath package) $ do
            bytes <- readFileBS (cpPath package)
            (table, (_, packument)) <- expectRight (packedFullRead [bytes])
            let releases = KeyMap.toList (packumentVersions packument)
                walked = walkedLength table
                wrong = [(version, packedEncodedLength form, walked form) | (version, release) <- releases, form <- [release, withoutHole release], packedEncodedLength form /= walked form]
            Just (length releases) `shouldBe` Map.lookup (cpName package) captureReleases
            -- A wrong length is wrong in most releases, and the first few name the fault.
            take 3 wrong `shouldBe` []

-- Freeze complete capture counts, including releases that policy rejects.
captureReleases :: Map Text Int
captureReleases =
    Map.fromList
        [ ("@types/node", 2373)
        , ("webpack", 889)
        , ("@aws-sdk/client-s3", 777)
        , ("express", 289)
        , ("typescript", 3829)
        , ("@babel/core", 231)
        , ("react", 2953)
        , ("request", 126)
        , ("lodash", 117)
        ]

bothFullReads :: [ByteString] -> (Either MetadataError (PackageInfo, CachedDoc), Either MetadataError (PackageInfo, CachedDoc))
bothFullReads chunks =
    ( second (fst npmCached) <$> (first MetadataBoundExceeded (walkJsonChunks (bodyLimit chunks) (npmPackumentWalk limits name WholePackument (testTable releaseUniqueFields)) chunks) >>= projectNpmStream limits name captureRegistry)
    , second (fst npmPacked) . snd <$> packedFullRead chunks
    )
  where
    limits = readLimits chunks
    name = packageNameOf chunks

packedFullRead :: [ByteString] -> Either MetadataError (InternTable, (PackageInfo, PackedPackument))
packedFullRead chunks = do
    streamed <- first MetadataBoundExceeded (walkWritingChunks (bodyLimit chunks) (npmFullTestWalk limits name captureRegistry) chunks)
    Walked table _ <- first (streamError limits) (streamValue streamed)
    (table,) <$> projectNpmPacked limits name captureRegistry streamed
  where
    limits = readLimits chunks
    name = packageNameOf chunks

bodyLimit :: [ByteString] -> BodyLimit
bodyLimit = MetadataBodyLimit . sum . map BS.length

readLimits :: [ByteString] -> Limits
readLimits chunks = defaultLimits{maxMetadataBytes = max 1 (sum (map BS.length chunks))}

captureRegistry :: Text
captureRegistry = "https://registry.npmjs.org"

packageNameOf :: [ByteString] -> PackageName
packageNameOf chunks = case eitherDecodeStrict (mconcat chunks) of
    Right (Object fields) | Just (String reported) <- KeyMap.lookup "name" fields, Right parsed <- projectName reported -> parsed
    _ -> unscopedNpm "thing"

served :: [Either MetadataError (PackageInfo, CachedDoc)] -> Maybe (Either RenderRefused LByteString)
served = fmap snd . mergedListing

coverListing :: (MonadTest m) => Maybe (MergePlan, Either RenderRefused LByteString) -> m ()
coverListing listing = do
    cover 10 "listing rendered" (isJust listing)
    cover 5 "a tarball rebased" (any rebased tarballs)
    cover 5 "a tarball served as read" (not (all rebased tarballs))
  where
    rebased = T.isPrefixOf "https://proxy.example/npm/"
    tarballs = case listing >>= rightToMaybe . snd >>= decode of
        Just (Object document) | Just (Object versions) <- KeyMap.lookup "versions" document -> mapMaybe tarballUrl (KeyMap.elems versions)
        _ -> []

-- The first source is trusted and supplies the base document.
mergedListing :: [Either MetadataError (PackageInfo, CachedDoc)] -> Maybe (MergePlan, Either RenderRefused LByteString)
mergedListing results = (\(plan, body, _) -> (plan, body)) <$> listingEvidence (zip (map (show :: Int -> ByteString) [0 ..]) results)

listingEvidence :: [(ByteString, Either MetadataError (PackageInfo, CachedDoc))] -> Maybe (MergePlan, Either RenderRefused LByteString, ETag)
listingEvidence results = do
    sources <- traverse (traverse rightToMaybe) results
    (_, (_, base)) <- listToMaybe sources
    let snapshots = [(index, Snapshot (digestOf bytes) doc, Snapshot (digestOf bytes) info) | (index, (bytes, (info, doc))) <- zip [0 ..] sources]
        provenance index = if index == 0 then TrustedSource else GatedSource
        mount = "https://proxy.example/npm/"
        pieces = [(provenance index, digestOf bytes, [(key, map artEntryKey (toList (pkgArtifacts details))) | (key, details) <- Map.toList (infoVersions info)]) | (index, (bytes, (info, _))) <- zip [0 :: Int ..] sources]
    plan <- mergePackuments [(provenance index, info) | (index, _, info) <- snapshots]
    pure (plan, serialiseMergedDocument (assembleMergedDocument mount (Map.fromList [(index, doc) | (index, doc, _) <- snapshots]) plan (Just base)), packumentETag mount [captureRegistry] (unscopedNpm "thing") pieces)

integratedFactsSpec :: Spec
integratedFactsSpec = describe "production direct facts against the frozen base" $ do
    it "projects a successful release with key identity, empty hooks, time and integrity" $ do
        let body = factsBody (policyRelease [("name", String "other-name"), ("version", String "other-version"), ("hasInstallScript", Bool False), ("scripts", object ["install" .= ("" :: Text), "build" .= ("work" :: Text)]), ("deprecated", Bool True), ("dist", object ["tarball" .= ("https://registry.npmjs.org/thing/-/one.tgz" :: Text), "shasum" .= validSha1, "integrity" .= validSha512Sri])])
        assertIntegrated defaultLimits body
        (info, _) <- expectRight (directFullRead defaultLimits (unscopedNpm "thing") (bodyLimit [body]) [body])
        details <- expectRight (maybeToRight ("expected alias release" :: Text) (Map.lookup "alias" (infoVersions info)))
        pkgName details `shouldBe` unscopedNpm "thing"
        pkgVersion details `shouldBe` mkVersion Npm "alias"
        pkgInstallCode details `shouldBe` RunsCodeOnInstall "declares install script(s): install"
        pkgAvailability details `shouldBe` Deprecated
        pkgPublishedAt details `shouldSatisfy` isJust
        map artEntryKey (toList (pkgArtifacts details)) `shouldBe` [ObjectEntry "alias"]
        map (length . artHashes) (toList (pkgArtifacts details)) `shouldBe` [2]
        infoDistTags info `shouldBe` Map.singleton "latest" (mkVersion Npm "alias")
        infoInvalidEntries info `shouldBe` []
    for_ integratedReleases $ \(label, release) ->
        it label $ assertIntegrated defaultLimits (factsBody release)

    it "omits unretained authors and keeps sorted diagnostics through invalid fallback" $ do
        let bad author = policyRelease [("author", author), ("scripts", Bool False)]
            body = "{\"name\":\"thing\",\"versions\":{\"z\":" <> bad (String "original Alice") <> ",\"a\":" <> bad Null <> ",\"ok\":" <> policyRelease [] <> "},\"dist-tags\":{\"z\":false,\"a\":7},\"time\":{\"ok\":false,\"z\":false}}"
        assertIntegrated defaultLimits body
        (info, _) <- expectRight (directFullRead defaultLimits (unscopedNpm "thing") (bodyLimit [body]) [body])
        map (\entry -> (invalidKind entry, invalidKey entry)) (infoInvalidEntries info)
            `shouldBe` [(InvalidVersionManifest, "a"), (InvalidVersionManifest, "z"), (InvalidDistTag, "a"), (InvalidDistTag, "z"), (InvalidPublishTime, "ok")]
        map (fieldAt "author" . invalidValue) (take 2 (infoInvalidEntries info)) `shouldBe` [Nothing, Nothing]
        let absent = factsBody (policyRelease [("scripts", Bool False)])
        (withoutAuthor, _) <- expectRight (directFullRead defaultLimits (unscopedNpm "thing") (bodyLimit [absent]) [absent])
        map (fieldAt "author" . invalidValue) (infoInvalidEntries withoutAuthor) `shouldBe` [Nothing]

    it "counts duplicate keys before name validation" $ do
        let body = "{\"name\":\"different\",\"versions\":{\"alias\":" <> policyRelease [] <> ",\"alias\":null}}"
            name = unscopedNpm "thing"
            readWith limits bytes = directFullRead limits name (bodyLimit [bytes]) [bytes]
        assertIntegrated defaultLimits{maxVersionCount = 1} body
        readWith defaultLimits{maxVersionCount = 1} body `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))
        assertIntegrated defaultLimits{maxVersionCount = 2} body
        readWith defaultLimits{maxVersionCount = 2} body `shouldBe` Left (MetadataNameMismatch "different")

    it "preserves first versions, containers, timestamps, tags and malformed first fields" $
        for_ duplicateBodies (assertIntegrated defaultLimits)

    it "preserves depth and late body refusals after successful release packing" $ do
        let body = factsBody (policyRelease [("scripts", object ["install" .= ("" :: Text)]), ("license", object ["type" .= ("MIT" :: Text)])])
        for_ [0 .. 8] $ \depth -> assertIntegrated defaultLimits{maxNestingDepth = depth} body
        let chunks = [body, " trailing source bytes"]
            bound = bodyLimit [body]
            readWith walk = walk defaultLimits (unscopedNpm "thing") bound chunks
        readWith directFullRead `shouldBe` readWith baseFullRead
        readWith directFullRead `shouldBe` Left (MetadataBoundExceeded (BodyTooLarge bound))
        for_ ["{\"name\":\"thing\",\"versions\":{\"alias\":" <> policyRelease [] <> "},\"time\":{\"alias\":", body <> " trailing"] $
            assertIntegrated defaultLimits

    it "preserves mixed packed and tree assembly, supported updates, ETags and conditional decisions" $ do
        let bodies = [richPackumentBytes "thing" ["1.0.0", "2.0.0"], richPackumentBytes "thing" ["2.0.0", "3.0.0"]]
            readWith walk body = second (fst npmPacked) <$> walk defaultLimits (unscopedNpm "thing") (bodyLimit [body]) [body]
        baseSources <- traverse (expectRight . readWith baseFullRead) bodies
        directSources <- traverse (expectRight . readWith directFullRead) bodies
        for_ [False, True] $ \firstTree -> for_ [False, True] $ \secondTree -> do
            let treeIf wanted source = if wanted then traverse (fmap (fst npmCached) . snd npmCached) source else Just source
                forms = zipWithM treeIf [firstTree, secondTree]
                updated (info, doc) = (info{infoVersions = Map.map (\details -> details{pkgPublishedAt = Nothing}) (Map.delete "1.0.0" (infoVersions info)), infoDistTags = mempty}, doc)
                evidence sources = listingEvidence (zip bodies (map Right sources))
            baseForms <- expectRight (maybeToRight ("expected base document forms" :: Text) (forms baseSources))
            directForms <- expectRight (maybeToRight ("expected direct document forms" :: Text) (forms directSources))
            for_ [id, map updated] $ \update -> do
                expected@(_, _, tag) <- expectRight (maybeToRight ("expected base listing" :: Text) (evidence (update baseForms)))
                actual@(_, rendered, actualTag) <- expectRight (maybeToRight ("expected direct listing" :: Text) (evidence (update directForms)))
                actual `shouldBe` expected
                rendered `shouldSatisfy` isRight
                for_ [[], [(hIfNoneMatch, "*")], [(hIfNoneMatch, "\"different\"")], [(hIfNoneMatch, encodeUtf8 (renderETag tag))], [(hIfNoneMatch, "W/" <> encodeUtf8 (renderETag tag))]] $ \headers ->
                    evaluateETag headers actualTag `shouldBe` evaluateETag headers tag
                evaluateETag [(hIfNoneMatch, encodeUtf8 (renderETag tag))] actualTag `shouldBe` NotModified tag
                evaluateETag [] actualTag `shouldBe` Modified tag

assertIntegrated :: Limits -> ByteString -> Expectation
assertIntegrated limits body = for_ [[body], byteChunks body] $ \chunks ->
    directFullRead limits name (bodyLimit chunks) chunks `shouldBe` baseFullRead limits name (bodyLimit chunks) chunks
  where
    name = unscopedNpm "thing"

directFullRead :: Limits -> PackageName -> BodyLimit -> [ByteString] -> Either MetadataError (PackageInfo, PackedPackument)
directFullRead limits name bound chunks = do
    streamed <- first MetadataBoundExceeded (walkWritingChunks bound (npmFullTestWalk limits name captureRegistry) chunks)
    projectNpmPacked limits name captureRegistry streamed

baseFullRead :: Limits -> PackageName -> BodyLimit -> [ByteString] -> Either MetadataError (PackageInfo, PackedPackument)
baseFullRead limits name bound chunks = do
    streamed <- first MetadataBoundExceeded (walkWritingChunks bound setup chunks)
    Walked table acc <- first (streamError limits) (streamValue streamed)
    baseFinishPacked name pointer (docTable (tableTexts table)) acc
  where
    pointer = "See " <> fromRight (captureRegistry <> "/" <> renderPackageName name) (packageUrl captureRegistry name)
    setup :: ST st (Cursor -> ST st (Steps (ST st) (Walked BasePackedRead)))
    setup = do
        let Interned authorKey withKey = internName (decodedName "author") (testTable releaseUniqueFields)
            Interned author seeded = internName (decodedName pointer) withKey
        writer <- newWriter (Just (authorKey, author))
        pure (npmWalk writer (maxNestingDepth limits) WholePackument (basePackedStep writer limits name) baseKeepsPackedRelease seeded baseEmptyPackedRead)

policyRelease :: [(Text, Value)] -> ByteString
policyRelease fields = rawObject (fields <> [("name", String "thing"), ("version", String "wire-version"), ("dist", object ["tarball" .= ("https://registry.npmjs.org/thing/-/one.tgz" :: Text)])])

factsBody :: ByteString -> ByteString
factsBody release = "{\"name\":\"thing\",\"versions\":{\"alias\":" <> release <> "},\"time\":{\"alias\":\"2020-01-01T00:00:00Z\"},\"dist-tags\":{\"latest\":\"alias\"}}"

integratedReleases :: [(String, ByteString)]
integratedReleases =
    [("optional " <> toString field <> " " <> show value, policyRelease [(field, value)]) | field <- ["scripts", "license", "_npmUser", "hasInstallScript", "deprecated", "name", "version"], value <- values]
        <> [("dist " <> toString (Key.toText field) <> " " <> show value, policyRelease [("dist", withKeys [(field, value)] dist)]) | field <- ["tarball", "shasum", "integrity", "unpackedSize"], value <- values <> [Number 1.5, Number 9223372036854775808]]
        <> [("object " <> toString field <> "." <> toString member <> " " <> show value, policyRelease [(field, object [Key.fromText member .= value])]) | (field, members) <- [("license", ["type", "url"]), ("_npmUser", ["name", "email", "url"])], member <- members, value <- values]
        <> [("script command " <> show value, policyRelease [("scripts", object ["build" .= value, "install" .= ("" :: Text)])]) | value <- values]
        <> [("duplicate " <> toString field <> " " <> show firstValue, policyRelease [(field, firstValue), (field, laterValue)]) | field <- ["license", "_npmUser", "hasInstallScript", "scripts", "name", "version", "dist"], (firstValue, laterValue) <- [(Null, Bool False), (Bool False, Null), (object [], Array mempty), (Array mempty, object [])]]
        <> [("first script " <> show firstValue, BS.dropEnd 1 (policyRelease []) <> ",\"scripts\":" <> rawObject [("build", firstValue), ("build", laterValue), ("install", String "")] <> "}") | (firstValue, laterValue) <- [(String "ok", Bool False), (Bool False, String "ok"), (Null, String "ok")]]
        <> [("nested duplicate " <> toString field <> "." <> toString member <> " " <> show firstValue, BS.dropEnd 1 (rawObject [("name", String "thing"), ("version", String "wire-version")]) <> "," <> BL.toStrict (encode field) <> ":" <> rawObject ([(member, firstValue), (member, laterValue)] <> required) <> (if field == "dist" then "}" else ",\"dist\":{\"tarball\":\"a.tgz\"}}")) | (field, member, required) <- ([("license", "type", []), ("license", "url", []), ("_npmUser", "email", []), ("dist", "tarball", []), ("dist", "integrity", [("tarball", String "a.tgz")])] :: [(Text, Text, [(Text, Value)])]), (firstValue, laterValue) <- [(String "ok", Bool False), (Bool False, String "ok"), (Null, String "ok")]]
        <> [("tarball hole " <> toString url, policyRelease [("dist", object ["tarball" .= url])]) | url <- (["https://source.example/", "https://source.example/a.tgz ", "relative.tgz", "https://source.example/\233.tgz", "https://source.example/%2F.tgz", ""] :: [Text])]
        <> [("Unicode commands and legacy licence", policyRelease [("scripts", object ["build" .= ("\233\128578" :: Text), "postinstall" .= ("" :: Text)]), ("license", object ["type" .= ("MIT OR Apache-2.0" :: Text)]), ("_npmUser", object ["name" .= ("\233\128578" :: Text)]), ("hasInstallScript", Bool False)])]
  where
    values = [Null, Bool False, Number 7, String "", String "work", Array mempty, object [], object ["nested" .= Bool True]]
    dist = object ["tarball" .= ("https://registry.npmjs.org/thing/-/one.tgz" :: Text)]

duplicateBodies :: [ByteString]
duplicateBodies =
    ["{\"name\":\"thing\",\"versions\":{\"alias\":" <> firstValue <> ",\"alias\":" <> laterValue <> "},\"versions\":{\"other\":" <> policyRelease [] <> "}}" | (firstValue, laterValue) <- [(policyRelease [], "null"), ("null", policyRelease []), (policyRelease [("scripts", Bool False)], policyRelease [])]]
        <> ["{\"name\":\"thing\",\"versions\":" <> firstValue <> ",\"versions\":" <> laterValue <> "}" | (firstValue, laterValue) <- [("null", versions), ("[]", versions), (versions, "[]"), (versions, "null")]]
        <> ["{\"name\":\"thing\",\"versions\":" <> versions <> ",\"time\":{\"alias\":false,\"alias\":\"2020-01-01T00:00:00Z\"},\"time\":{},\"dist-tags\":{\"latest\":7,\"latest\":\"alias\"},\"dist-tags\":{\"other\":\"alias\"}}"]
        <> ["{\"name\":" <> firstValue <> ",\"name\":\"thing\",\"versions\":" <> versions <> "}" | firstValue <- ["null", "false", "\"thing\"", "\"different\""]]
  where
    versions = "{\"alias\":" <> policyRelease [] <> "}"

projectNpmManifestSpec :: Spec
projectNpmManifestSpec = describe "projectNpmManifest" $ do
    it "pairs a valid policy projection with its compact source representation" $
        case projectNpmManifest defaultLimits (unscopedNpm "is-odd") (manifestBytes "is-odd" ["3.0.1"]) of
            Right (info, raw) -> do
                renderPackageName (infoName info) `shouldBe` "is-odd"
                Map.keys (infoVersions info) `shouldBe` ["3.0.1"]
                raw `shouldSatisfy` isObject
            other -> expectationFailure ("expected a projection, got: " <> show other)

    it "refuses an empty signature object beyond the retained nesting budget" $ do
        let version = withKeys [("dist", object ["tarball" .= ("https://example.test/is-odd-1.0.0.tgz" :: Text), "signatures" .= [object []]])] (versionObject "is-odd" "1.0.0")
            body = BL.toStrict (encode (object ["name" .= ("is-odd" :: Text), "versions" .= object ["1.0.0" .= version]]))
            limited = defaultLimits{maxNestingDepth = 5}
        projectNpmManifest limited (unscopedNpm "is-odd") body `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 5))
        projectNpmVersion limited (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 5))
        projectNpmManifest defaultLimits{maxNestingDepth = 6} (unscopedNpm "is-odd") body `shouldSatisfy` isRight

    it "reports a nesting-depth breach as a bound breach" $
        projectNpmManifest (defaultLimits{maxNestingDepth = 2}) (unscopedNpm "is-odd") (manifestBytes "is-odd" ["1.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))

projectNpmVersionSpec :: Spec
projectNpmVersionSpec = describe "projectNpmVersion" $ do
    it "matches the full projection over a real multi-version packument (express, 288 versions)" $ do
        body <- readFileBS "core/test/unit/fixtures/npm/express.full.json"
        case projectNpmManifest defaultLimits (unscopedNpm "express") body of
            Right (info, _raw) -> do
                let keys = Map.keys (infoVersions info)
                    n = length keys
                    sample = mapMaybe (keys !!?) (ordNub [0, n `div` 4, n `div` 2, (3 * n) `div` 4, n - 1])
                length sample `shouldSatisfy` (> 0)
                forM_ sample $ \v ->
                    selectedDetails defaultLimits (unscopedNpm "express") (mkVersion Npm v) body
                        `shouldBe` Right (Map.lookup v (infoVersions info))
            Left err -> expectationFailure ("the express fixture did not project: " <> show err)

    it "yields the PackageDetails identical to a full projection + lookup, for every version" $ do
        let versions = ["1.0.0", "2.1.3", "10.0.0-beta.1"]
            body = richPackumentBytes "is-odd" versions
        case projectNpmManifest defaultLimits (unscopedNpm "is-odd") body of
            Right (info, _raw) ->
                forM_ versions $ \v ->
                    selectedDetails defaultLimits (unscopedNpm "is-odd") (mkVersion Npm v) body
                        `shouldBe` Right (Map.lookup v (infoVersions info))
            Left err -> expectationFailure ("the rich fixture did not project: " <> show err)

    it "pairs the selected version object as decoded beside the projected details" $ do
        let versions = ["1.0.0", "2.1.3"]
            body = richPackumentBytes "is-odd" versions
        forM_ versions $ \v -> do
            selected <- expectRight (projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm v) body)
            fmap vdRaw (vrVersion selected) `shouldBe` Just (Just (fst npmCached (compactRichVersion "is-odd" v)))

    it "matches a selected version object to the same source's full projection" $ do
        let versions = ["1.0.0", "2.1.3", "10.0.0-beta.1"]
            body = richPackumentBytes "is-odd" versions
        (_info, raw) <- expectRight (projectNpmManifest defaultLimits (unscopedNpm "is-odd") body)
        forM_ versions $ \v -> do
            selected <- expectRight (projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm v) body)
            selectNpmVersionDoc (mkVersion Npm v) (fst npmCached raw) `shouldBe` (vdRaw =<< vrVersion selected)
        selectNpmVersionDoc (mkVersion Npm "9.9.9") (fst npmCached raw) `shouldBe` Nothing

    it "drops a malformed requested-version object as a forwarded miss (Right Nothing), as the full path would" $ do
        let body = BL.toStrict . encode $ object ["name" .= ("is-odd" :: Text), "versions" .= object ["1.0.0" .= object ["name" .= ("is-odd" :: Text)]]]
        selectedDetails defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` Right Nothing

    it "extracts usable data without requiring trailing-data validity" $ do
        let body = richPackumentBytes "is-odd" ["1.0.0"]
        selectedDetails defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (body <> " trailing")
            `shouldBe` selectedDetails defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body

    it "reports a nesting-depth breach as a bound breach" $
        selectedDetails (defaultLimits{maxNestingDepth = 2}) (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-odd" ["1.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))

    it "stops at the version ceiling before completing name validation" $
        selectedDetails (defaultLimits{maxVersionCount = 1}) (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") (richPackumentBytes "is-even" ["1.0.0", "2.0.0"])
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

    it "reads the document's own dist-tags.latest, matching the full projection" $ do
        let versions = ["1.0.0", "2.1.3"]
            body = richPackumentBytes "is-odd" versions
        case projectNpmManifest defaultLimits (unscopedNpm "is-odd") body of
            Right (info, _raw) ->
                fmap vrUpstreamLatest (projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body)
                    `shouldBe` Right (Map.lookup "latest" (infoDistTags info))
            Left err -> expectationFailure ("the rich fixture did not project: " <> show err)

    it "reads no latest from a document that declares none" $ do
        let body = BL.toStrict (encode (object ["name" .= ("is-odd" :: Text), "versions" .= object []]))
        fmap vrUpstreamLatest (projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body)
            `shouldBe` Right Nothing

    it "reads no latest from a non-string tag target, as the full projection drops it" $ do
        let body =
                BL.toStrict . encode $
                    object ["name" .= ("is-odd" :: Text), "dist-tags" .= object ["latest" .= (7 :: Int)], "versions" .= object []]
        fmap vrUpstreamLatest (projectNpmVersion defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body)
            `shouldBe` Right Nothing

    optionalContainerParity
    duplicateKeyParity

optionalContainerParity :: Spec
optionalContainerParity = describe "optional containers match the full projection" $
    forM_ containers $ \(key, populated) -> describe (toString key) $ do
        forM_ [("absent", []), ("null", [Null]), ("empty", [object []]), ("array", [toJSON ([] :: [Value])]), ("string", [String "bad"]), ("boolean", [Bool True]), ("number", [toJSON (7 :: Int)])] $ \(label, values) ->
            it label $ parity defaultLimits (bodyFor key values)
        forM_ [("null then object", [Null, populated]), ("object then null", [populated, Null]), ("null then invalid", [Null, Bool True]), ("invalid then null", [Bool True, Null])] $ \(label, values) ->
            it ("keeps the first duplicate: " <> label) $ parity defaultLimits (bodyFor key values)
        forM_ [[], [Null], [object []]] $ \values ->
            it ("accepts missing, null or empty containers: " <> show values) $ do
                projectNpmManifest defaultLimits name (bodyFor key values) `shouldSatisfy` isRight
                projectNpmVersion defaultLimits name version (bodyFor key values) `shouldSatisfy` isRight
        forM_ [Array mempty, String "invalid", Bool True, Number 0] $ \invalid -> do
            it ("refuses an invalid first container: " <> show invalid) $ do
                let body = bodyFor key [invalid, populated]
                projectNpmManifest defaultLimits name body `shouldBe` Left MetadataUndecodable
                projectNpmVersion defaultLimits name version body `shouldBe` Left MetadataUndecodable
            it ("ignores an invalid duplicate after a valid container: " <> show invalid) $ do
                let body = bodyFor key [populated, invalid]
                projectNpmManifest defaultLimits name body `shouldBe` projectNpmManifest defaultLimits name (bodyFor key [populated])
                sameSelected body (bodyFor key [populated])
            it ("keeps a null first container before an invalid duplicate: " <> show invalid) $ do
                let body = bodyFor key [Null, invalid]
                projectNpmManifest defaultLimits name body `shouldBe` projectNpmManifest defaultLimits name (bodyFor key [Null])
                sameSelected body (bodyFor key [Null])
        it "skips deeply nested duplicate data after the first container" $ do
            let limits = defaultLimits{maxNestingDepth = 5}
                deep = foldr (\_ value -> object ["nested" .= value]) Null [1 :: Int .. 6]
            parity limits (bodyFor key [Null, deep])
  where
    name :: PackageName
    name = unscopedNpm "is-odd"
    version :: Version
    version = mkVersion Npm "1.0.0"
    containers :: [(Text, Value)]
    containers =
        [ ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
        , ("time", object ["1.0.0" .= ("2020-01-01T00:00:00Z" :: Text)])
        , ("dist-tags", object ["latest" .= ("1.0.0" :: Text)])
        ]
    bodyFor :: Text -> [Value] -> ByteString
    bodyFor key values = rawObject (("name", String "is-odd") : filter ((/= key) . fst) containers <> map (key,) values)
    sameSelected :: ByteString -> ByteString -> Expectation
    sameSelected body original = do
        actual <- expectRight (projectNpmVersion defaultLimits name version body)
        expected <- expectRight (projectNpmVersion defaultLimits name version original)
        vrVersion actual `shouldBe` vrVersion expected
        vrUpstreamLatest actual `shouldBe` vrUpstreamLatest expected
        vrBodyBytes actual `shouldBe` BS.length body
    parity :: Limits -> ByteString -> Expectation
    parity limits body = do
        selectedDetails limits name version body `shouldBe` fullVersionOutcome limits name "1.0.0" body
        fmap vrUpstreamLatest (projectNpmVersion limits name version body)
            `shouldBe` fmap (Map.lookup "latest" . infoDistTags . fst) (projectNpmManifest limits name body)

duplicateKeyParity :: Spec
duplicateKeyParity = describe "duplicate top-level keys keep the first occurrence" $ do
    it "counts only the first versions object, not the sum across duplicate versions keys" $ do
        let body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
                    , ("versions", object ["2.0.0" .= versionObject "is-odd" "2.0.0", "3.0.0" .= versionObject "is-odd" "3.0.0"])
                    ]
            limits = defaultLimits{maxVersionCount = 1}
        selectedDetails limits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome limits (unscopedNpm "is-odd") "1.0.0" body

    it "serves the first versions object's manifest, not a later duplicate's" $ do
        let firstManifest = distTarballObject "is-odd" "1.0.0" "https://example.test/is-odd-1.0.0-first.tgz"
            secondManifest = distTarballObject "is-odd" "1.0.0" "https://example.test/is-odd-1.0.0-second.tgz"
            body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("versions", object ["1.0.0" .= firstManifest])
                    , ("versions", object ["1.0.0" .= secondManifest])
                    ]
        selectedDetails defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome defaultLimits (unscopedNpm "is-odd") "1.0.0" body

    it "treats a version absent from the first versions object as absent, ignoring a later duplicate" $ do
        let body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("versions", object [])
                    , ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
                    ]
        selectedDetails defaultLimits (unscopedNpm "is-odd") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome defaultLimits (unscopedNpm "is-odd") "1.0.0" body

    it "validates the first top-level name, not a later duplicate (anti-shadowing)" $ do
        let body =
                rawObject
                    [ ("name", String "is-odd")
                    , ("name", String "evil")
                    , ("versions", object ["1.0.0" .= versionObject "is-odd" "1.0.0"])
                    ]
        selectedDetails defaultLimits (unscopedNpm "evil") (mkVersion Npm "1.0.0") body
            `shouldBe` fullVersionOutcome defaultLimits (unscopedNpm "evil") "1.0.0" body

manifestBytes :: Text -> [Text] -> ByteString
manifestBytes name versions =
    BL.toStrict . encode $
        object
            [ "name" .= name
            , "dist-tags" .= object ["latest" .= latestOf versions]
            , "versions" .= object [Key.fromText v .= versionObject name v | v <- versions]
            ]

latestOf :: [Text] -> Text
latestOf = \case
    (v : _) -> v
    [] -> "0.0.0"

versionObject :: Text -> Text -> Value
versionObject name v =
    object
        [ "name" .= name
        , "version" .= v
        , "dist" .= object ["tarball" .= ("https://example.test/" <> name <> "-" <> v <> ".tgz")]
        ]

selectedDetails :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
selectedDetails limits name version body = readDetails <$> projectNpmVersion limits name version body

richPackumentBytes :: Text -> [Text] -> ByteString
richPackumentBytes nm versions =
    BL.toStrict . encode $
        object
            [ "name" .= nm
            , "dist-tags" .= object ["latest" .= latestOf versions]
            , "versions" .= object [Key.fromText v .= richVersionObject nm v | v <- versions]
            , "time"
                .= object
                    ( ("created" .= ("2009-01-01T00:00:00.000Z" :: Text))
                        : ("modified" .= ("2030-01-01T00:00:00.000Z" :: Text))
                        : [Key.fromText v .= (stampFor i :: Text) | (i, v) <- zip [0 :: Int ..] versions]
                    )
            ]
  where
    stampFor :: Int -> Text
    stampFor i = "20" <> show (10 + i) <> "-03-14T15:09:26.000Z"

richVersionObject :: Text -> Text -> Value
richVersionObject nm v =
    object
        [ "name" .= nm
        , "version" .= v
        , "dist"
            .= object
                [ "tarball" .= ("https://example.test/" <> nm <> "-" <> v <> ".tgz")
                , "integrity" .= validSha512Sri
                , "shasum" .= validSha1
                ]
        , "dependencies" .= object ["left-pad" .= ("^1.0.0" :: Text), "lodash" .= ("^4.17.0" :: Text)]
        , "devDependencies" .= object ["jest" .= ("^29.0.0" :: Text)]
        , "scripts" .= object ["postinstall" .= ("node ./build.js" :: Text)]
        , "license" .= ("MIT" :: Text)
        , "maintainers" .= [object ["name" .= ("alice" :: Text), "email" .= ("alice@example.test" :: Text)]]
        , "_npmUser" .= object ["name" .= ("bob" :: Text)]
        ]

distTarballObject :: Text -> Text -> Text -> Value
distTarballObject name v tarball =
    object
        [ "name" .= name
        , "version" .= v
        , "dist" .= object ["tarball" .= tarball]
        ]

fullVersionOutcome :: Limits -> PackageName -> Text -> ByteString -> Either MetadataError (Maybe PackageDetails)
fullVersionOutcome limits name v body =
    (\(info, _raw) -> Map.lookup v (infoVersions info)) <$> projectNpmManifest limits name body

-- encode alone cannot preserve duplicate object keys.
rawObject :: [(Text, Value)] -> ByteString
rawObject members =
    "{" <> BS.intercalate "," [BL.toStrict (encode k) <> ":" <> BL.toStrict (encode v) | (k, v) <- members] <> "}"

compactRichVersion :: Text -> Text -> Value
compactRichVersion name version = case richVersionObject name version of
    Object fields -> Object (KeyMap.insert "author" (String ("See https://registry.npmjs.org/" <> name)) (KeyMap.delete "maintainers" fields))
    other -> other

-- Base b795016660d171db729bc773a9a8c495e38d3294, StreamingProjection blob 613b23160dbc3f3aa0ff9335bb2ea5b256306bd7.
type BaseTypedRelease = Either InvalidEntry PackageDetails
data BaseNpmProjection = BaseNpmProjection
    { baseProjectedName :: Maybe Value
    , baseProjectedVersions :: Map Text BaseTypedRelease
    , baseProjectedTimes :: Map Text (Either InvalidEntry UTCTime)
    , baseProjectedTags :: Map Text (Either InvalidEntry Version)
    , baseProjectedBookkeeping :: Map Text Value
    , baseProjectedCount :: Int
    , baseProjectedContainers :: Set NpmContainer
    , baseProjectedActiveContainer :: Maybe NpmContainer
    , baseProjectedInvalidContainer :: Bool
    }
baseEmptyProjection :: BaseNpmProjection
baseEmptyProjection =
    BaseNpmProjection
        { baseProjectedName = Nothing
        , baseProjectedVersions = mempty
        , baseProjectedTimes = mempty
        , baseProjectedTags = mempty
        , baseProjectedBookkeeping = mempty
        , baseProjectedCount = 0
        , baseProjectedContainers = mempty
        , baseProjectedActiveContainer = Nothing
        , baseProjectedInvalidContainer = False
        }
baseKeepsRelease :: BaseNpmProjection -> Text -> Bool
baseKeepsRelease acc key = baseProjectedActiveContainer acc == Just VersionsContainer && Map.notMember key (baseProjectedVersions acc)
baseCollectRelease :: Limits -> BaseNpmProjection -> NpmFieldOf release -> Text -> BaseTypedRelease -> Either LimitError BaseNpmProjection
baseCollectRelease limits acc field key typed = (\counted -> counted{baseProjectedVersions = Map.insert key typed (baseProjectedVersions counted)}) <$> baseCollectField limits acc field
baseCollectField :: Limits -> BaseNpmProjection -> NpmFieldOf release -> Either LimitError BaseNpmProjection
baseCollectField limits acc = \case
    IgnoredField -> Right acc{baseProjectedActiveContainer = Nothing}
    BeginContainer container ->
        Right
            acc
                { baseProjectedContainers = Set.insert container (baseProjectedContainers acc)
                , baseProjectedActiveContainer = if Set.member container (baseProjectedContainers acc) then Nothing else Just container
                }
    InvalidContainer container ->
        Right
            acc
                { baseProjectedContainers = Set.insert container (baseProjectedContainers acc)
                , baseProjectedActiveContainer = Nothing
                , baseProjectedInvalidContainer = baseProjectedInvalidContainer acc || Set.notMember container (baseProjectedContainers acc)
                }
    NameField value -> Right acc{baseProjectedName = baseProjectedName acc <|> Just value}
    VersionField _ _ | baseProjectedActiveContainer acc /= Just VersionsContainer -> Right acc
    VersionField _ _ -> do
        let count = baseProjectedCount acc + 1
        checkVersionCountOf limits count
        pure acc{baseProjectedCount = count}
    TimeField _ _ | baseProjectedActiveContainer acc /= Just TimeContainer -> Right acc
    TimeField key _ | Map.member key (baseProjectedTimes acc) -> Right acc
    TimeField key value ->
        Right
            acc
                { baseProjectedTimes = baseFirstInsert key (decodeEntry parsePublishTime force InvalidPublishTime key value) (baseProjectedTimes acc)
                , baseProjectedBookkeeping =
                    if key == "created" || key == "modified"
                        then baseFirstInsert key value (baseProjectedBookkeeping acc)
                        else baseProjectedBookkeeping acc
                }
    TagField _ _ | baseProjectedActiveContainer acc /= Just TagsContainer -> Right acc
    TagField key _ | Map.member key (baseProjectedTags acc) -> Right acc
    TagField key value -> Right acc{baseProjectedTags = baseFirstInsert key (decodeEntry parseJSON (mkVersion Npm) InvalidDistTag key value) (baseProjectedTags acc)}
  where
    decodeEntry parse convert kind key value = case parseEither parse value of
        Left err -> Left $! mkInvalidEntry kind key value (toText err)
        Right typed -> Right $! convert typed

baseFirstInsert :: (Ord k) => k -> a -> Map k a -> Map k a
baseFirstInsert = Map.insertWith (\_ old -> old)
baseProjectRelease :: PackageName -> Text -> Value -> Either Text PackageDetails
baseProjectRelease name key projected = case projectVersionEntryResult name (mkVersion Npm key) Nothing projected of
    Left err -> Left (toText err)
    Right details -> Right $! details
baseInvalidRelease :: Text -> Value -> Text -> BaseTypedRelease
baseInvalidRelease key whole reason = Left $! mkInvalidEntry InvalidVersionManifest key whole reason
data BaseNpmParts = BaseNpmParts Value (Map Text Value)
baseFinishParts :: PackageName -> BaseNpmProjection -> Either MetadataError (PackageInfo, BaseNpmParts)
baseFinishParts requested acc = do
    when (baseProjectedInvalidContainer acc) (Left MetadataUndecodable)
    reported <- validateReportedName projectName (baseProjectedName acc)
    _ <- projectionResult (checkNameAgreement requested reported ())
    pure (package, BaseNpmParts (fromMaybe Null (baseProjectedName acc)) (baseProjectedBookkeeping acc))
  where
    versions = Map.mapMaybe rightToMaybe (baseProjectedVersions acc)
    times = Map.mapMaybe rightToMaybe (baseProjectedTimes acc)
    tags = Map.mapMaybe rightToMaybe (baseProjectedTags acc)
    stamp key details = details{pkgPublishedAt = Map.lookup key times}
    drops = lefts . Map.elems
    package =
        PackageInfo
            { infoName = requested
            , infoVersions = Map.mapWithKey stamp versions
            , infoDistTags = tags
            , infoInvalidEntries =
                strictElements
                    ( drops (baseProjectedVersions acc)
                        <> drops (baseProjectedTags acc)
                        <> drops (Map.restrictKeys (baseProjectedTimes acc) (Map.keysSet versions))
                    )
            }
data BasePackedRead = BasePackedRead BaseNpmProjection [(Text, Packed)]
baseEmptyPackedRead :: BasePackedRead
baseEmptyPackedRead = BasePackedRead baseEmptyProjection []
baseKeepsPackedRelease :: BasePackedRead -> Text -> Bool
baseKeepsPackedRelease (BasePackedRead acc _) = baseKeepsRelease acc

basePackedStep :: Writer st -> Limits -> PackageName -> BasePackedRead -> NpmFieldOf () -> (Either LimitError BasePackedRead -> ST st r) -> ST st r
basePackedStep writer limits name (BasePackedRead acc heldReleases) field next = case field of
    VersionField key (Just ())
        | baseKeepsRelease acc key -> do
            sealed <- sealValue writer tarballHole
            picked <- decodePicked writer baseTypedMembers sealed
            typed <- case baseProjectRelease name key picked of
                Right details -> pure (Right details)
                Left reason -> (\whole original -> baseInvalidRelease key (asRead original whole) reason) <$> decodeWhole writer sealed <*> replacedMember writer
            let !release = if baseRebases picked then sealed else withoutHole sealed
            next $! do
                kept <- baseCollectRelease limits acc field key typed
                pure (BasePackedRead kept ((key, release) : heldReleases))
        | otherwise -> discard writer >> unchanged
    _ -> unchanged
  where
    unchanged = next $! (`BasePackedRead` heldReleases) <$> baseCollectField limits acc field
    asRead original = \case
        Object fields -> Object (maybe (KeyMap.delete "author") (KeyMap.insert "author") original fields)
        other -> other
baseRebases :: Value -> Bool
baseRebases release = isJust (tarballUrl release >>= rebaseArtifactUrl Just)
baseTypedMembers :: Pick
baseTypedMembers = Only [(field, if field == "dist" then Only [(member, Whole) | member <- distFields] else Whole) | field <- versionListFields <> map installableFieldName [minBound .. maxBound]]
baseFinishPacked :: PackageName -> Text -> DocTable -> BasePackedRead -> Either MetadataError (PackageInfo, PackedPackument)
baseFinishPacked requested authorPointer table (BasePackedRead acc heldReleases) = second document <$> baseFinishParts requested acc
  where
    document (BaseNpmParts name time) =
        PackedPackument
            { packumentTop =
                KeyMap.fromList
                    [ ("name", name)
                    , ("author", String authorPointer)
                    , ("time", Object (KeyMap.fromList [(Key.fromText key, raw) | (key, raw) <- Map.toList time]))
                    ]
            , packumentTable = table
            , packumentVersions = KeyMap.fromMap (Map.fromDistinctAscList [(Key.fromText key, release) | (key, release) <- sortBy (comparing fst) heldReleases])
            }

projectionDriver :: ProjectionDriver
projectionDriver =
    ProjectionDriver
        { projectFull = \limits bytes -> pure (fst <$> projectNpmManifest limits (unscopedNpm "is-odd") bytes)
        , projectSelected = \limits version bytes -> pure (selectedDetails limits (unscopedNpm "is-odd") (mkVersion Npm version) bytes)
        , projectionFraming = SingleDocument
        , projectionDocument = richPackumentBytes
        , projectionPackageName = "is-odd"
        , projectionOtherName = "is-even"
        , projectionVersions = ["1.0.0", "2.0.0"]
        , projectionMissingName = BL.toStrict (encode (object ["versions" .= object []]))
        }
