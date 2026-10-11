-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Direct facts against the actual base's independently frozen decoder and projector.
Base: b795016660d171db729bc773a9a8c495e38d3294.
Wire blob: b3baa16dd234d7b5a6c95366687799d151d0d000.
Project blob: fd25145e7a9f98aa2b6fcdb59dd05a4827130825.
-}
module Ecluse.Core.Registry.Npm.VersionWriterSpec (spec) where

import Control.Monad.ST (ST)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), encode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.ByteString qualified as BS
import Data.JsonStream.TokenParser (TokenResult)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Artifact (..), Availability (..), CodeExecSignal (..), Hash, HashAlg (SHA1), PackageDetails (..), PackageInfo (..), PackageName, mkHash, mkSriHashes)
import Ecluse.Core.Package.Entry (EntryKey (ObjectEntry))
import Ecluse.Core.Package.Merge (Provenance (..), mergePackuments)
import Ecluse.Core.Registry.Json.Intern (Entry, InternTable, Interned (..), decodedName, internName, tableTexts)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, docTable, renderPlan)
import Ecluse.Core.Registry.Json.Shape (Mode (Share), Shape (Generic), readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), Walked (..), withElement)
import Ecluse.Core.Registry.Json.Writer (decodeWhole, newWriter, replacedMember, sealValue)
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..), tarballHole)
import Ecluse.Core.Registry.Npm.Filter (assemblePackedPackument)
import Ecluse.Core.Registry.Npm.Project (projectVersionEntryResult, projectVersionManifest)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Streaming (NpmField, NpmFieldOf (..), NpmRead (FullRead), npmFields)
import Ecluse.Core.Registry.Npm.VersionWriter
import Ecluse.Core.Registry.Npm.Wire (VersionManifest (vmScripts))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Text (urlFilename)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), corpusPackages)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genPackumentBytes, genServablePackumentBytes)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkWritingChunks)
import Ecluse.Test.Registry.Npm qualified as Fixture
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "VersionWriter" $ do
    literalSpec
    growthSpec
    generatedSpec
    assemblySpec
    authorSpec
    it "preserves every committed npm capture against the frozen base" $ do
        for_ corpusPackages $ \capture -> do
            body <- readFileBS (cpPath capture)
            assertParity 64 True [body]

literalSpec :: Spec
literalSpec = describe "literal parity" $ do
    for_ adverseReleases $ \(label, release) ->
        it label $ for_ [True, False] $ \share -> assertParity 64 share [packument release]
    it "retains the first ignored script command even when a later duplicate is invalid" $ do
        let release = extended "\"scripts\":{\"build\":\"ok\",\"build\":false,\"install\":\"\"}"
        for_ [True, False] $ \share -> do
            observed <- successful (directRead 64 share [packument release])
            map (fst . snd) (observedFacts observed) `shouldBe` [True]
            observedHooks observed `shouldBe` [Just (Map.singleton "install" "")]
    it "rejects an invalid ignored command even when a later duplicate is valid" $ do
        observed <- successful (directRead 64 True [packument (extended "\"scripts\":{\"build\":false,\"build\":\"ok\"}")])
        map (fst . snd) (observedFacts observed) `shouldBe` [False]
    it "preserves first containers, alias keys and source field order" $ do
        let body = "{\"time\":{\"alias\":\"2020-01-01T00:00:00Z\"},\"dist-tags\":{\"latest\":\"alias\"},\"name\":\"thing\",\"versions\":{\"alias\":" <> baseRelease <> ",\"alias\":null},\"versions\":{\"other\":null}}"
        assertParity 64 True [body]
    it "matches depth refusals across each retained level and chunk boundary" $
        for_ [0 .. 10] $
            \depth -> assertParity depth True (byteChunks (packument (extended "\"license\":{\"type\":\"MIT\"},\"scripts\":{\"install\":\"\"}")))
    it "preserves body-limit refusals before and after a complete release" $
        for_ [0, 1, BS.length body - 1, BS.length body, BS.length body + 1] $ \bound ->
            directBound bound 64 True (byteChunks body) `shouldBe` baseBound bound 64 True (byteChunks body)
  where
    body = packument baseRelease

growthSpec :: Spec
growthSpec = describe "script and table growth" $ do
    for_ [0, 1, 17, 32, 64, 127, 128, 256, 300, 1024] $ \count ->
        it ("keeps three hooks while packing " <> show count <> " ignored commands") $ do
            let script n = "\"build" <> show n <> "\":\"command" <> show n <> "\""
                scripts = BS.intercalate "," ([script n | n <- [0 .. count - 1 :: Int]] <> ["\"postinstall\":\"\"", "\"install\":\"do work\"", "\"preinstall\":\"\""])
                body = packument (extended ("\"scripts\":{" <> scripts <> "}"))
            for_ [True, False] $ \share -> do
                assertParity 64 share [body]
                observed <- successful (directRead 64 share [body])
                observedHooks observed `shouldBe` [Just (Map.fromList [(key, "") | key <- ["preinstall", "install", "postinstall"]])]
    it "resets the writer after sealing earlier releases" $ do
        let body = "{\"name\":\"thing\",\"versions\":{\"1\":" <> extended "\"scripts\":{\"install\":\"\"}" <> ",\"2\":" <> baseRelease <> ",\"3\":null,\"4\":" <> baseRelease <> "}}"
        assertParity 64 True (byteChunks body)
        observed <- successful (directRead 64 True [body])
        observedHooks observed `shouldBe` [Just (Map.singleton "install" ""), Just mempty, Nothing, Just mempty]
    it "discards scratch before the next release without carrying its scripts or licence" $ do
        let body = "{\"versions\":{\"skip\":" <> extended "\"scripts\":{\"install\":\"work\"},\"license\":false" <> ",\"keep\":" <> baseRelease <> "}}"
        observed <- successful (directBoundSkipping (Just "skip") (BS.length body) 64 True (byteChunks body))
        map fst (observedFacts observed) `shouldBe` ["keep"]
        map (fst . snd) (observedFacts observed) `shouldBe` [True]
        observedHooks observed `shouldBe` [Just mempty]

generatedSpec :: Spec
generatedSpec = modifyMaxSuccess (const 2000) $ do
    it "matches frozen facts, diagnostics and packed bytes on generated malformed bodies" $ hedgehog $ do
        body <- forAll (genPackumentBytes >>= damaged)
        chunks <- forAll (genChunks body)
        depth <- forAll (Gen.frequency [(3, pure 64), (2, Gen.int (Range.linear 0 8))])
        share <- forAll Gen.bool
        directRead depth share chunks === baseRead depth share chunks
        fieldsOf (directRead depth share chunks) === referenceRead depth chunks
    it "matches frozen facts on generated usable releases with hostile optional fields" $ hedgehog $ do
        body <- forAll genServablePackumentBytes
        chunks <- forAll (genChunks body)
        directRead 64 True chunks === baseRead 64 True chunks

assemblySpec :: Spec
assemblySpec = it "assembles mixed sources from unchanged blobs and frozen policy facts" $ do
    let source version url = toStrict (encode (Fixture.packumentValue "thing" version [(version, Fixture.versionValue (Fixture.versionSpec "thing" version url))] [] []))
        bodies = [source "1.0.0" "https://one.example/one.tgz?x=1", source "2.0.0" "https://two.example/two.tgz#file", source "1.0.0" "https://trusted.example/winner.tgz"]
    direct <- traverse (successful . directRead 64 True . (: [])) bodies
    frozen <- traverse (successful . baseRead 64 True . (: [])) bodies
    assemble direct `shouldBe` assemble frozen
    assemble direct `shouldSatisfy` isJust

authorSpec :: Spec
authorSpec = it "preserves the source author for fallback diagnostics while packing its replacement" $ do
    let body = extended "\"author\":{\"name\":\"owner\"}"
        setup :: ST st (TokenResult -> ST st (Steps (ST st) (Bool, Value, Maybe Value)))
        setup = do
            let (table, key, pointer) = authorEntries
            writer <- newVersionWriter (Just (key, pointer))
            pure $ \tokens -> withElement tokens $ \element rest ->
                readShape writer (Generic 8) Share table element rest $ \value _ _ -> do
                    sealed <- sealVersion writer tarballHole
                    raw <- decodeWhole (packedWriter writer) sealed
                    original <- replacedMember (packedWriter writer)
                    pure (Finished (isJust (versionFacts value), raw, original))
    (_, result) <- expectRight (readOutcome (walkWritingChunks (MetadataBodyLimit (BS.length body)) setup [body]))
    (valid, raw, original) <- expectRight result
    valid `shouldBe` True
    original `shouldBe` Just (Object (KeyMap.singleton "name" (String "owner")))
    case raw of
        Object fields -> KeyMap.lookup "author" fields `shouldBe` Just (String "See the source")
        _ -> expectationFailure "expected the packed release object"

authorEntries :: (InternTable, Entry, Entry)
authorEntries = case internName (decodedName "author") (testTable releaseUniqueFields) of
    Interned key keyed -> case internName (decodedName "See the source") keyed of
        Interned pointer table -> (table, key, pointer)

data Observation = Observation
    { observedFields :: [NpmField]
    , observedFacts :: [(Text, (Bool, Either String PackageDetails))]
    , observedHooks :: [Maybe (Map Text Text)]
    , observedVersions :: [(Text, Packed)]
    , observedTable :: DocTable
    }
    deriving stock (Eq, Show)

emptyObservation :: Observation
emptyObservation = Observation [] [] [] [] (docTable mempty)

type Outcome = Either LimitError (Int, Either Bool Observation)

directRead :: Int -> Bool -> [ByteString] -> Outcome
directRead depth share chunks = directBound (sum (map BS.length chunks)) depth share chunks

directBound :: Int -> Int -> Bool -> [ByteString] -> Outcome
directBound = directBoundSkipping Nothing

directBoundSkipping :: Maybe Text -> Int -> Int -> Bool -> [ByteString] -> Outcome
directBoundSkipping skipped bound depth share chunks = finished (walkWritingChunks (MetadataBodyLimit bound) setup chunks)
  where
    setup :: ST st (TokenResult -> ST st (Steps (ST st) (Walked Observation)))
    setup = do
        writer <- newVersionWriter Nothing
        let step observed field next = case field of
                VersionField key (Just _) | Just key == skipped -> discardVersion writer >> next (Right observed)
                VersionField key (Just value) -> do
                    sealed <- sealVersion writer tarballHole
                    raw <- decodeWhole (packedWriter writer) sealed
                    let facts = versionFacts value
                        typed = maybe (projectVersionEntryResult name (mkVersion Npm key) Nothing raw) (Right . projectVersionManifest name (mkVersion Npm key) Nothing) facts
                    next (Right (recordRelease key raw sealed (isJust facts, typed) (vmScripts <$> facts) observed))
                _ -> next (Right (recordField (withoutRelease field) observed))
        pure (npmWalk writer depth WholePackument step (\_ _ -> share) (testTable releaseUniqueFields) emptyObservation)

baseRead :: Int -> Bool -> [ByteString] -> Outcome
baseRead depth share chunks = baseBound (sum (map BS.length chunks)) depth share chunks

baseBound :: Int -> Int -> Bool -> [ByteString] -> Outcome
baseBound bound depth share chunks = finished (walkWritingChunks (MetadataBodyLimit bound) setup chunks)
  where
    setup :: ST st (TokenResult -> ST st (Steps (ST st) (Walked Observation)))
    setup = do
        writer <- newWriter Nothing
        let step observed field next = case field of
                VersionField key (Just ()) -> do
                    sealed <- sealValue writer tarballHole
                    raw <- decodeWhole writer sealed
                    let typed = frozenProjectVersionEntryResult name (mkVersion Npm key) Nothing raw
                        hooks = if isRight typed then fmap (Map.map (const "") . Map.filterWithKey (\key' _ -> key' `elem` frozenInstallHooks) . frozenVmScripts) (parseMaybe parseJSON raw) else Nothing
                    next (Right (recordRelease key raw sealed (isRight typed, typed) hooks observed))
                _ -> next (Right (recordField (withoutRelease field) observed))
        pure (npmWalk writer depth WholePackument step (\_ _ -> share) (testTable releaseUniqueFields) emptyObservation)

finished :: Either LimitError (StreamResult (Walked Observation)) -> Outcome
finished = fmap (second (fmap ordered)) . readOutcome
  where
    ordered (Walked table observed) =
        observed
            { observedFields = reverse (observedFields observed)
            , observedFacts = reverse (observedFacts observed)
            , observedHooks = reverse (observedHooks observed)
            , observedVersions = reverse (observedVersions observed)
            , observedTable = docTable (tableTexts table)
            }

recordRelease :: Text -> Value -> Packed -> (Bool, Either String PackageDetails) -> Maybe (Map Text Text) -> Observation -> Observation
recordRelease key raw sealed facts hooks observed =
    (recordField (VersionField key (Just raw)) observed)
        { observedFacts = (key, facts) : observedFacts observed
        , observedHooks = hooks : observedHooks observed
        , observedVersions = (key, sealed) : observedVersions observed
        }

recordField :: NpmField -> Observation -> Observation
recordField field observed = observed{observedFields = field : observedFields observed}

withoutRelease :: NpmFieldOf a -> NpmField
withoutRelease = \case
    VersionField key _ -> VersionField key Nothing
    NameField value -> NameField value
    TimeField key value -> TimeField key value
    TagField key value -> TagField key value
    BeginContainer container -> BeginContainer container
    InvalidContainer container -> InvalidContainer container
    IgnoredField -> IgnoredField

fieldsOf :: Outcome -> Either LimitError (Int, Either Bool [NpmField])
fieldsOf = fmap (second (fmap observedFields))

referenceRead :: Int -> [ByteString] -> Either LimitError (Int, Either Bool [NpmField])
referenceRead depth chunks = fmap (second (fmap reverse)) (readOutcome (parseJsonChunks (MetadataBodyLimit (sum (map BS.length chunks))) (npmFields depth FullRead) (\fields field -> Right (field : fields)) [] chunks))

assertParity :: Int -> Bool -> [ByteString] -> Expectation
assertParity depth share chunks = do
    let direct = directRead depth share chunks
    direct `shouldBe` baseRead depth share chunks
    fieldsOf direct `shouldBe` referenceRead depth chunks

successful :: Outcome -> IO Observation
successful result = do
    (_, parsed) <- expectRight result
    expectRight parsed

assemble :: [Observation] -> Maybe ByteString
assemble observations = do
    let entries = zip [0 ..] observations
        sources = Map.fromList [(sid, Snapshot (digestOf ("source" <> show sid)) (document observation)) | (sid, observation) <- entries]
        infos = [(if sid == 0 then GatedSource else TrustedSource, Snapshot (digestOf ("source" <> show sid)) (info observation)) | (sid, observation) <- entries]
    plan <- mergePackuments infos
    renderPlan (assemblePackedPackument "https://proxy.example/npm" sources plan (document <$> listToMaybe observations))
  where
    document observation = PackedPackument (KeyMap.singleton "name" (String "thing")) (observedTable observation) (KeyMap.fromList [(Key.fromText key, value) | (key, value) <- observedVersions observation])
    info observation = PackageInfo name (Map.fromList [(key, details) | (key, (_, Right details)) <- observedFacts observation]) mempty []

name :: PackageName
name = unscopedNpm "thing"

baseRelease :: ByteString
baseRelease = toStrict (encode (Fixture.versionValue (Fixture.versionSpec "thing" "1.0.0" "https://registry.npmjs.org/thing/-/one.tgz")))

extended :: ByteString -> ByteString
extended fields = BS.dropEnd 1 baseRelease <> "," <> fields <> "}"

packument :: ByteString -> ByteString
packument release = "{\"name\":\"thing\",\"versions\":{\"1.0.0\":" <> release <> "}}"

byteChunks :: ByteString -> [ByteString]
byteChunks = map BS.singleton . BS.unpack

adverseReleases :: [(String, ByteString)]
adverseReleases =
    [("release " <> toString literal, literal) | literal <- ["null", "true", "7", "\"release\"", "[]", "{}"]]
        <> [("required " <> toString key <> " is " <> toString value, requiredRelease key value) | key <- ["name", "version", "dist"], value <- ["null", "false", "7", "[]", "{}", "\"\""]]
        <> [("missing " <> toString key, requiredRelease key "") | key <- ["name", "version", "dist"]]
        <> [("dist " <> toString key <> " is " <> toString value, requiredRelease "dist" ("{\"tarball\":\"a.tgz\"," <> "\"" <> key <> "\":" <> value <> "}")) | key <- ["shasum", "integrity"], value <- ["null", "false", "7", "[]", "{}", "\"invalid\""]]
        <> [("field " <> toString key <> " is " <> toString value, extended ("\"" <> key <> "\":" <> value)) | key <- ["deprecated", "hasInstallScript", "scripts", "license", "_npmUser"], value <- ["null", "false", "true", "0", "1.5", "\"\"", "\"MIT\"", "[]", "{}"]]
        <> [("bad script " <> toString value, extended ("\"scripts\":{\"build\":" <> value <> ",\"install\":\"\"}")) | value <- ["null", "false", "true", "0", "1.5", "[]", "{}"]]
        <> [("licence " <> toString value, extended ("\"license\":" <> value)) | value <- ["{\"type\":null,\"url\":null}", "{\"type\":7}", "{\"url\":false}", "{\"type\":\"MIT\",\"type\":false}", "{\"type\":false,\"type\":\"MIT\"}"]]
        <> [("advisory size " <> toString value, "{\"name\":\"thing\",\"version\":\"ignored\",\"dist\":{\"tarball\":\"https://example.test/a.tgz\",\"unpackedSize\":" <> value <> "}}") | value <- ["-1", "0", "1.5", "9223372036854775807", "9223372036854775808", "1e1000", "true", "null"]]
        <> [ ("first required member wins", "{\"name\":\"thing\",\"name\":false,\"version\":\"opaque\",\"version\":null,\"dist\":{\"tarball\":\"a.tgz\",\"tarball\":null}}")
           , ("invalid first required member wins", "{\"name\":false,\"name\":\"thing\",\"version\":null,\"dist\":{}}")
           , ("multiple invalid optional fields preserve priority", extended "\"hasInstallScript\":7,\"scripts\":false,\"license\":false,\"_npmUser\":false")
           , ("false flag cannot hide empty hooks", extended "\"hasInstallScript\":false,\"scripts\":{\"postinstall\":\"\",\"install\":\"\",\"preinstall\":\"\"}")
           , ("true flag declares install code without hooks", extended "\"hasInstallScript\":true,\"scripts\":{\"build\":\"work\"}")
           , ("Unicode commands and keys", extended "\"scripts\":{\"pr\\u0065install\":\"\\ud83d\\ude00\",\"b\\u00e9\":\"\\u00e9\"},\"license\":{\"type\":\"\\u00e9\",\"url\":null}")
           , ("malformed ignored script string", extended "\"scripts\":{\"build\":\"\\x\"}")
           , ("source scripts remain unchanged", extended "\"scripts\":{\"build\":\"long command\",\"install\":\"\"},\"dependencies\":{\"other\":\"^1.0.0\"},\"bundledDependencies\":[\"other\"]")
           ]

requiredRelease :: ByteString -> ByteString -> ByteString
requiredRelease key value =
    "{" <> BS.intercalate "," ["\"" <> field <> "\":" <> if field == key then value else usual | (field, usual) <- defaults, field /= key || not (BS.null value)] <> "}"
  where
    defaults = [("name", "\"thing\""), ("version", "\"opaque\""), ("dist", "{\"tarball\":\"a.tgz\"}")]

-- Frozen base definitions use only shared domain constructors and library decoders.
data FrozenPerson = FrozenPerson
    { frozenPersonName :: Text
    , frozenPersonEmail :: Maybe Text
    , frozenPersonUrl :: Maybe Text
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON FrozenPerson where
    parseJSON = \case
        String name -> pure (FrozenPerson name Nothing Nothing)
        Object o ->
            FrozenPerson
                <$> o .:? "name" .!= ""
                <*> o .:? "email"
                <*> o .:? "url"
        other -> frozenTypeMismatchOneOf "Person (object or string)" other

data FrozenLicense
    = FrozenLicenseSpdx Text
    | FrozenLicenseObject Text (Maybe Text)
    deriving stock (Eq, Ord, Show)

instance FromJSON FrozenLicense where
    parseJSON = \case
        String spdx -> pure (FrozenLicenseSpdx spdx)
        Object o ->
            FrozenLicenseObject
                <$> o .:? "type" .!= ""
                <*> o .:? "url"
        other -> frozenTypeMismatchOneOf "License (object or string)" other

data FrozenDist = FrozenDist
    { frozenDistTarball :: Text
    , frozenDistShasum :: Maybe Text
    , frozenDistIntegrity :: Maybe Text
    , frozenDistUnpackedSize :: Maybe Int
    }
    deriving stock (Eq, Ord, Show)

instance FromJSON FrozenDist where
    parseJSON = withObject "Dist" $ \o ->
        FrozenDist
            <$> o .: "tarball"
            <*> o .:? "shasum"
            <*> o .:? "integrity"
            <*> frozenLenientOptional o "unpackedSize"

data FrozenVersionManifest = FrozenVersionManifest
    { frozenVmName :: Text
    , frozenVmVersion :: Text
    , frozenVmDist :: FrozenDist
    , frozenVmAvailability :: Availability
    , frozenVmHasInstallScript :: Maybe Bool
    , frozenVmScripts :: Map Text Text
    , frozenVmLicense :: Maybe FrozenLicense
    }
    deriving stock (Eq, Show)

instance FromJSON FrozenVersionManifest where
    parseJSON = withObject "VersionManifest" $ \o ->
        FrozenVersionManifest
            <$> o .: "name"
            <*> o .: "version"
            <*> o .: "dist"
            <*> (frozenAvailability <$> o .:? "deprecated")
            <*> o .:? "hasInstallScript"
            <*> o .:? "scripts" .!= mempty
            <*> o .:? "license"

frozenAvailability :: Maybe Value -> Availability
frozenAvailability = \case
    Just (String "") -> Available
    Just (String _) -> Deprecated
    Just (Bool True) -> Deprecated
    _ -> Available

newtype FrozenVersionEntry = FrozenVersionEntry FrozenVersionManifest

instance FromJSON FrozenVersionEntry where
    parseJSON v =
        withObject "npm version object" (\o -> FrozenVersionEntry <$> parseJSON v <* (o .:? "_npmUser" :: Parser (Maybe FrozenPerson))) v

frozenProjectVersionEntryResult :: PackageName -> Version -> Maybe UTCTime -> Value -> Either String PackageDetails
frozenProjectVersionEntryResult name version publishedAt value =
    frozenProjectDetails name version publishedAt <$> parseEither parseJSON value

frozenProjectDetails :: PackageName -> Version -> Maybe UTCTime -> FrozenVersionEntry -> PackageDetails
frozenProjectDetails name version publishedAt (FrozenVersionEntry vm) =
    PackageDetails
        { pkgName = name
        , pkgVersion = version
        , pkgPublishedAt = publishedAt
        , pkgInstallCode = frozenInstallCode vm
        , pkgAvailability = frozenVmAvailability vm
        , pkgArtifacts = (:| []) $! frozenProjectArtifact version (frozenVmDist vm)
        }

frozenInstallCode :: FrozenVersionManifest -> CodeExecSignal
frozenInstallCode vm
    | not (null hooks) =
        RunsCodeOnInstall ("declares install script(s): " <> T.intercalate ", " hooks)
    | frozenVmHasInstallScript vm == Just True =
        RunsCodeOnInstall "declares an install script (hasInstallScript)"
    | otherwise = NoCodeOnInstall
  where
    hooks = filter (`Map.member` frozenVmScripts vm) frozenInstallHooks

frozenInstallHooks :: [Text]
frozenInstallHooks = ["preinstall", "install", "postinstall"]

frozenProjectArtifact :: Version -> FrozenDist -> Artifact
frozenProjectArtifact version dist =
    Artifact
        { artEntryKey = ObjectEntry (renderVersion version)
        , artFilename = frozenTarballFilename (frozenDistTarball dist) version
        , artUrl = frozenDistTarball dist
        , artHashes = strictElements (sriHashes <> maybeToList sha1Hash)
        , artSize = frozenDistUnpackedSize dist
        }
  where
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg

    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (frozenDistIntegrity dist)
    sha1Hash = frozenDistShasum dist >>= toHash SHA1

frozenTarballFilename :: Text -> Version -> Text
frozenTarballFilename url version =
    fromMaybe (renderVersion version <> ".tgz") (urlFilename url)

frozenLenientOptional :: (FromJSON a, NFData a) => Object -> Key -> Parser (Maybe a)
frozenLenientOptional = frozenLenientOptionalWith parseJSON

frozenLenientOptionalWith :: (NFData a) => (Value -> Parser a) -> Object -> Key -> Parser (Maybe a)
frozenLenientOptionalWith decode o k = do
    mv <- o .:? k

    pure $ case mv >>= parseMaybe decode of
        Just value -> Just $!! value
        Nothing -> Nothing

frozenTypeMismatchOneOf :: String -> Value -> Parser a
frozenTypeMismatchOneOf expected actual =
    fail ("expected " <> expected <> ", but encountered " <> frozenValueKind actual)

frozenValueKind :: Value -> String
frozenValueKind = \case
    Object{} -> "an object"
    String{} -> "a string"
    Array{} -> "an array"
    Number{} -> "a number"
    Bool{} -> "a boolean"
    Null -> "null"
