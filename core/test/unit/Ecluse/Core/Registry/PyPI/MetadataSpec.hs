-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI protocol, artifact identity and release-age projection parity.
module Ecluse.Core.Registry.PyPI.MetadataSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Core.Package (
    Artifact (artEntryKey, artFilename),
    PackageDetails (pkgArtifacts, pkgPublishedAt),
    PackageInfo (infoName, infoVersions),
    renderPackageName,
 )
import Ecluse.Core.Package.Admission (ArtifactAdmission (AdmissionAdmit, AdmissionDenied))
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.Metadata (
    Manifest (manifestRetainedEntries),
    MetadataError (MetadataBoundExceeded, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Metadata.EntryFloor (RetainedEntries (RetainedEntries), entryFloorBytes, entryFloorCharge)
import Ecluse.Core.Registry.Publish (PublishCodec (pcTargetRead))
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIStream, pypiChargeFactors)
import Ecluse.Core.Registry.PyPI.Publish (pypiPublishCodec)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext), Rule (AllowByIdentity, AllowIfOlderThan), completeEvidence)
import Ecluse.Core.Security (
    LimitError (TooManyVersions),
    Limits (maxVersionCount),
    defaultLimits,
 )
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfKeptPermille))
import Ecluse.Test.Contract.Metadata.Projection (MetadataFraming (SingleDocument), ProjectionDriver (..), metadataProjectionContract)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), pypiCorpusPackages)
import Ecluse.Test.Json (encodeStrict)
import Ecluse.Test.Package (defaultMinIntegrity, pypiVersion, requestsName, unsafeFilename, unscopedPyPI)
import Ecluse.Test.Package.Admission (admitArtifactWithEvidence)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks, heldWorkspacePayments, heldWorkspacePeak)
import Ecluse.Test.Registry.PyPI (denseIndex, filesNamed, minimalIndex, simpleFile, simpleIndex, simpleIndexWith, withFileKeys)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIChunks, projectPyPIIndex, projectPyPIVersion, simpleValue)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, evalRules, inertRuleDeps)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    metadataProjectionContract "pypi" projectionDriver
    describe "worker selected envelope" $
        it "retains supported project fields without widening ordinary selected reads" $ do
            let body = encodeStrict (simpleIndexWith "requests" [("project-status", object ["status" .= ("active" :: Text)]), ("alternate-locations", Array (fromList [String "https://other.test/simple/requests/"]))] (filesNamed ["requests-2.34.2.tar.gz"]))
                readMode mode = projectPyPIChunks defaultLimits requestsName mode [body] >>= projectPyPIStream defaultLimits requestsName
            (_, worker) <- expectRight (readMode (MirrorRead requestsName "2.34.2"))
            (_, ordinary) <- expectRight (readMode (SelectedRead requestsName "2.34.2"))
            KeyMap.member "project-status" (simpleEnvelope worker) `shouldBe` True
            KeyMap.member "alternate-locations" (simpleEnvelope worker) `shouldBe` True
            KeyMap.member "project-status" (simpleEnvelope ordinary) `shouldBe` False
            KeyMap.member "alternate-locations" (simpleEnvelope ordinary) `shouldBe` False
    indexSpec
    versionSpec
    paritySpec
    protocolSpec
    releaseAgeSpec
    entryFloorSpec

-- Files pay the release floor, and dropped version entries and alternate-locations and tracks items the
-- entry floor, that their bytes leave uncovered, through the wiring serve and publication use.
entryFloorSpec :: Spec
entryFloorSpec = describe "the full-read entry floor" $ do
    it "charges the workspace for files that name no release and dropped versions beyond their per-byte charge" $ do
        (result, peak) <- heldWorkspacePeak adapterRead pypiBase requestsName [denseBody]
        void result `shouldSatisfy` isRight
        peak `shouldBe` denseShortfall

    it "charges the workspace for minimal-body files at the release floor" $ do
        let body = minimalIndex 300
        (result, peak) <- heldWorkspacePeak adapterRead pypiBase (unscopedPyPI "thing") [body]
        void result `shouldSatisfy` isRight
        peak `shouldBe` releaseFloor * 300 - scaleCharge (cfFullReadPermille pypiChargeFactors) (BS.length body)

    it "counts every file, dropped version entry and envelope item, which the finished document keeps" $ do
        (dense, _) <- heldWorkspacePeak adapterRead pypiBase requestsName [denseBody]
        manifestRetainedEntries <$> expectRight dense `shouldReturn` RetainedEntries files drops
        let items = BS.intercalate "," (replicate 30 "0")
            body = "{\"meta\":{\"api-version\":\"1.1\",\"tracks\":[" <> items <> "]},\"name\":\"requests\",\"alternate-locations\":[" <> items <> "],\"files\":[]}"
        (envelope, _) <- heldWorkspacePeak adapterRead pypiBase requestsName [body]
        manifestRetainedEntries <$> expectRight envelope `shouldReturn` RetainedEntries 0 60

    it "charges no workspace for files whose source bytes cover the floor" $ do
        (result, peak) <- heldWorkspacePeak adapterRead pypiBase requestsName [indexOf ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz"]]
        void result `shouldSatisfy` isRight
        peak `shouldBe` 0

    it "charges a publication target's full read the same floor" $ do
        (result, peak) <- heldWorkspacePeak (pcTargetRead (pypiPublishCodec nativePresentation)) pypiBase requestsName [denseBody]
        void result `shouldSatisfy` isRight
        peak `shouldBe` denseShortfall

    it "charges the workspace for the alternate-locations and tracks items a dense listing keeps" $ do
        let locations = 300
            tracks = 200
            items count = BS.intercalate "," (replicate count "0")
            body = "{\"meta\":{\"api-version\":\"1.1\",\"tracks\":[" <> items tracks <> "]},\"name\":\"requests\",\"alternate-locations\":[" <> items locations <> "],\"alternate-locations\":[" <> items 100 <> "],\"files\":[]}"
        (result, peak) <- heldWorkspacePeak adapterRead pypiBase requestsName [body]
        void result `shouldSatisfy` isRight
        peak `shouldBe` entryFloorBytes * (locations + tracks) - scaleCharge (cfFullReadPermille pypiChargeFactors) (BS.length body)

    it "charges the alternate-locations and tracks items read before their array closes, and nothing for a repeat" $ do
        let items count = BS.intercalate "," (replicate count "\"0\"")
            pieces =
                [ "{\"meta\":{\"api-version\":\"1.1\",\"tracks\":[" <> items 200 <> ","
                , items 100 <> "],\"tracks\":[" <> items 50 <> "]},\"name\":\"requests\",\"alternate-locations\":[" <> items 300 <> ","
                , items 50 <> "],\"meta\":{\"tracks\":[" <> items 50 <> "]},\"files\":[]}"
                ]
            shortfall retained parsed = entryFloorBytes * retained - scaleCharge (cfFullReadPermille pypiChargeFactors) (sum (map BS.length parsed))
        (result, payments) <- heldWorkspacePayments adapterRead pypiBase requestsName pieces
        void result `shouldSatisfy` isRight
        payments `shouldBe` [shortfall 200 (take 1 pieces), shortfall 600 (take 2 pieces), shortfall 650 pieces]

    forM_ pypiCorpusPackages $ \package ->
        it ("keeps only the per-byte kept charge once it reads the capture " <> cpPath package) $ do
            bytes <- readFileBS (cpPath package)
            (result, _) <- heldWorkspacePeak adapterRead pypiBase (cpPackage package) (captureChunks bytes)
            manifest <- expectRight result
            entryFloorCharge (manifestRetainedEntries manifest) `shouldSatisfy` (<= scaleCharge (cfKeptPermille pypiChargeFactors) (BS.length bytes))
  where
    adapterRead = metadataRead (adapterMetadata pypiAdapter)
    pypiBase = "https://pypi.org/simple"
    files = 150
    drops = 400
    denseBody = denseIndex files drops
    denseShortfall = releaseFloor * files + entryFloorBytes * drops - scaleCharge (cfFullReadPermille pypiChargeFactors) (BS.length denseBody)
    -- The floor each file pays, which the release floor's measurement sets.
    releaseFloor = 1152

indexSpec :: Spec
indexSpec = describe "projectPyPIIndex" $ do
    it "projects a well-formed index into the manifest paired with its raw document" $
        case projectPyPIIndex defaultLimits requestsName (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz"]) of
            Right (info, raw) -> do
                renderPackageName (infoName info) `shouldBe` "requests"
                Map.keys (infoVersions info) `shouldBe` ["2.34.1", "2.34.2"]
                simpleValue raw `shouldBe` simpleIndex "requests" (filesNamed ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz"])
            other -> expectationFailure ("expected a projection, got: " <> show other)

    it "counts every file position of one release toward the version ceiling on full reads" $ do
        let body = indexOf ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"]
        projectPyPIIndex defaultLimits{maxVersionCount = 1} requestsName body
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))
        projectPyPIIndex defaultLimits{maxVersionCount = 2} requestsName body `shouldSatisfy` isRight

    it "counts files that name no release toward the version ceiling" $ do
        let body = encodeStrict (simpleIndex "requests" (filesNamed ["requests-2.34.2.tar.gz"] <> [simpleFile "not-a-distribution"]))
        projectPyPIIndex defaultLimits{maxVersionCount = 1} requestsName body
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

versionSpec :: Spec
versionSpec = describe "projectPyPIVersion" $ do
    it "projects one release's files out of an index carrying several" $
        artifactNames (projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2") (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl", "requests-2.34.1.tar.gz"]))
            `shouldBe` Right (Just ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"])

    it "counts unselected and unusable files toward the version ceiling on selected reads" $ do
        let body = encodeStrict (simpleIndex "requests" [Number 7, simpleFile "requests-2.0.tar.gz", simpleFile "requests-1.0.tar.gz", simpleFile "requests-1.0-py3-none-any.whl"])
        projectPyPIVersion defaultLimits{maxVersionCount = 3} requestsName (pypiVersion "1") body
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 4 3))

paritySpec :: Spec
paritySpec = describe "the two decode paths agree on what they serve" $ do
    it "retains original entry positions across skipped releases and malformed entries" $ do
        let filename = "requests-1.0.0.tar.gz"
            body = encodeStrict (simpleIndex "requests" [simpleFile "requests-2.0.0.tar.gz", Number 1, object ["filename" .= filename], simpleFile filename, simpleFile filename])
        (info, _) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
        selected <- expectRight (projectPyPIVersion defaultLimits requestsName (pypiVersion "1") body)
        selected `shouldBe` Map.lookup "1" (infoVersions info)
        (map artEntryKey . toList . pkgArtifacts <$> selected) `shouldBe` Just [ArrayEntry 3, ArrayEntry 4]

    for_ ["2.34.2", "2.34", "1.0.post1"] $ \version ->
        it ("resolves " <> toString version <> " to the same files as the whole-index path") $ do
            let body = indexOf ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl", "requests-2.34.tar.gz", "requests-1.0-1.tar.gz"]
            artifactNames (projectPyPIVersion defaultLimits requestsName (pypiVersion version) body)
                `shouldBe` Right (wholeIndexArtifacts version body)

protocolSpec :: Spec
protocolSpec = describe "protocol envelope parity" $ do
    for_ [String "2.0", String "0.9", String "broken", String "", Number 1, Bool True, Array mempty, object []] $ \declared ->
        it ("refuses the API declaration " <> show declared) $
            assertProtocolRefusal (protocolIndex (Just (object ["api-version" .= declared])))

    for_ [String "1.0", String "1.4", String "1", Null] $ \declared ->
        it ("accepts the API declaration " <> show declared) $
            assertProtocolAcceptance (protocolIndex (Just (object ["api-version" .= declared])))

    for_ [Nothing, Just Null, Just (object [])] $ \meta ->
        it ("accepts an absent declaration in " <> show meta) $
            assertProtocolAcceptance (protocolIndex meta)

    for_ [String "1.0", Number 1, Bool True, Array mempty] $ \meta ->
        it ("refuses a non-object meta value " <> show meta) $
            assertProtocolRefusal (protocolIndex (Just meta))

    it "checks the protocol before the name mismatch, and refuses a file past the ceiling before both" $ do
        let body = encodeStrict (simpleIndexWith "urllib3" ["meta" .= object ["api-version" .= ("2.0" :: Text)]] [simpleFile "urllib3-2.34.2.tar.gz"])
            limits = defaultLimits{maxVersionCount = 0}
            refused = Left (MetadataBoundExceeded (TooManyVersions 1 0))
        projectPyPIIndex defaultLimits requestsName body `shouldBe` Left MetadataUndecodable
        projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2") body `shouldBe` Left MetadataUndecodable
        projectPyPIIndex limits requestsName body `shouldBe` refused
        projectPyPIVersion limits requestsName (pypiVersion "2.34.2") body `shouldBe` refused

    for_ [("{\"api-version\":\"1.0\",\"api-version\":\"2.0\"}", True), ("{\"api-version\":\"2.0\",\"api-version\":\"1.0\"}", False), ("null,\"meta\":{\"api-version\":\"2.0\"}", True), ("{\"api-version\":\"2.0\"},\"meta\":null", False)] $ \(meta, accepted) ->
        it ("keeps the first protocol keys in " <> show meta) $ do
            let body = "{\"name\":\"requests\",\"meta\":" <> meta <> "}"
                expected :: Either MetadataError (Maybe PackageDetails)
                expected = if accepted then Right Nothing else Left MetadataUndecodable
            (Nothing <$ projectPyPIIndex defaultLimits requestsName body) `shouldBe` expected
            projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2") body `shouldBe` expected

    it "refuses an unsupported protocol even when the requested release is absent" $
        projectPyPIVersion defaultLimits requestsName (pypiVersion "9.9.9") (protocolIndex (Just (object ["api-version" .= ("2.0" :: Text)])))
            `shouldBe` Left MetadataUndecodable

    it "blocks cold artifact admission for bytes declaring an unsupported protocol" $ do
        rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
        let evalContext = EvalContext (UTCTime (fromGregorian 2026 9 7) 0) Nothing
            admit body = traverse (traverse (fmap fst . admitArtifactWithEvidence evalContext rules defaultMinIntegrity (unsafeFilename "requests-2.34.2.tar.gz"))) (projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2") body)
        admitted <- admit (protocolIndex (Just (object ["api-version" .= ("1.0" :: Text)])))
        case admitted of
            Right (Just AdmissionAdmit{}) -> pass
            other -> expectationFailure ("expected supported protocol admission, got: " <> show other)
        refused <- admit (protocolIndex (Just (object ["api-version" .= ("2.0" :: Text)])))
        case refused of
            Left MetadataUndecodable -> pass
            other -> expectationFailure ("expected protocol refusal before admission, got: " <> show other)

assertProtocolRefusal :: ByteString -> Expectation
assertProtocolRefusal body = do
    projectPyPIIndex defaultLimits requestsName body `shouldBe` Left MetadataUndecodable
    projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2") body `shouldBe` Left MetadataUndecodable

assertProtocolAcceptance :: ByteString -> Expectation
assertProtocolAcceptance body = do
    (info, _) <- expectRight (projectPyPIIndex defaultLimits requestsName body)
    let selected = Map.lookup "2.34.2" (infoVersions info)
    -- Name the release the fixture declares, so the parity line below cannot agree on a
    -- projection both paths got wrong.
    artifactNames (Right selected) `shouldBe` Right (Just ["requests-2.34.2.tar.gz"])
    projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2") body `shouldBe` Right selected

protocolIndex :: Maybe Value -> ByteString
protocolIndex meta = encodeStrict (object (["name" .= ("requests" :: Text), "files" .= [simpleFile "requests-2.34.2.tar.gz"]] <> maybe [] (\value -> ["meta" .= value]) meta))

releaseAgeSpec :: Spec
releaseAgeSpec = describe "release age and artifact admission" $
    for_ projections $ \(projectionName, project) -> describe projectionName $
        for_ timestampCases $ \(caseName, timestamps, expectedTime) ->
            it caseName $ do
                let names = ["requests-2.34.2-py3-none-any.whl", "requests-2.34.2-1-py3-none-any.whl"]
                    files = zipWith timestampedFile names timestamps
                    body = encodeStrict (simpleIndex "requests" files)
                    ctx = EvalContext (UTCTime (fromGregorian 2026 9 7) 0) Nothing
                projected <- expectRight (project body)
                case projected of
                    Just details -> do
                        map artFilename (toList (pkgArtifacts details)) `shouldBe` names
                        pkgPublishedAt details `shouldBe` expectedTime
                        ageRules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
                        decision <- evalRules ctx ageRules (completeEvidence details)
                        admittedBy decision `shouldBe` ("AllowIfOlderThan" <$ expectedTime)
                        exceptionRules <- prepare inertRuleDeps (map atDefaultPrecedence [AllowByIdentity "requests@2.34.2", AllowIfOlderThan (7 * nominalDay)])
                        for_ names $ \name -> do
                            (admission, _) <- admitArtifactWithEvidence ctx ageRules defaultMinIntegrity (unsafeFilename name) details
                            case (expectedTime, admission) of
                                (Nothing, AdmissionDenied{}) -> pass
                                (Just _, AdmissionAdmit{}) -> pass
                                other -> expectationFailure ("unexpected age admission: " <> show other)
                            (exception, _) <- admitArtifactWithEvidence ctx exceptionRules defaultMinIntegrity (unsafeFilename name) details
                            case exception of
                                AdmissionAdmit{} -> pass
                                other -> expectationFailure ("expected identity exception, got: " <> show other)
                    Nothing -> expectationFailure "expected both retained files"
  where
    projections =
        [ ("full projection", fmap (Map.lookup "2.34.2" . infoVersions . fst) . projectPyPIIndex defaultLimits requestsName)
        , ("selected projection", projectPyPIVersion defaultLimits requestsName (pypiVersion "2.34.2"))
        ]
    timestampCases =
        [ ("refuses a known and absent timestamp", [Just old, Nothing], Nothing)
        , ("refuses an absent and known timestamp", [Nothing, Just old], Nothing)
        , ("refuses a known and malformed timestamp", [Just old, Just "not-a-date"], Nothing)
        , ("refuses all unknown timestamps", [Nothing, Just "not-a-date"], Nothing)
        , ("uses the newest complete timestamp", [Just old, Just newer], Just newest)
        , ("uses the newest timestamp regardless of file order", [Just newer, Just old], Just newest)
        ]
    old = "2026-05-14T19:25:26Z"
    newer = "2026-05-15T00:00:00Z"
    newest = UTCTime (fromGregorian 2026 5 15) 0

timestampedFile :: Text -> Maybe Text -> Value
timestampedFile name = \case
    Just timestamp -> withFileKeys [("upload-time", String timestamp)] (simpleFile name)
    Nothing -> case simpleFile name of
        Object fields -> Object (KeyMap.delete "upload-time" fields)
        other -> other

artifactNames :: Either MetadataError (Maybe PackageDetails) -> Either MetadataError (Maybe [Text])
artifactNames = fmap (fmap (map artFilename . toList . pkgArtifacts))

wholeIndexArtifacts :: Text -> ByteString -> Maybe [Text]
wholeIndexArtifacts version body = case projectPyPIIndex defaultLimits requestsName body of
    Right (info, _) -> map artFilename . toList . pkgArtifacts <$> Map.lookup version (infoVersions info)
    Left _ -> Nothing

indexOf :: [Text] -> ByteString
indexOf = indexNamed "requests"

indexNamed :: Text -> [Text] -> ByteString
indexNamed name = encodeStrict . simpleIndex name . filesNamed

projectionDriver :: ProjectionDriver
projectionDriver =
    ProjectionDriver
        { projectFull = \limits bytes -> pure (fst <$> projectPyPIIndex limits requestsName bytes)
        , projectSelected = \limits version bytes -> pure (projectPyPIVersion limits requestsName (pypiVersion version) bytes)
        , projectionFraming = SingleDocument
        , projectionDocument = \name versions -> indexNamed name [name <> "-" <> version <> ".tar.gz" | version <- versions]
        , projectionPackageName = "requests"
        , projectionOtherName = "urllib3"
        , projectionVersions = ["2.34.2", "2.34.1"]
        , projectionMissingName = encodeStrict (object ["files" .= ([] :: [Value])])
        }
