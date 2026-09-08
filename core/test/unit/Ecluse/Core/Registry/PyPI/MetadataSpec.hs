-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI projection parity and the release metadata used by artifact admission.
module Ecluse.Core.Registry.PyPI.MetadataSpec (spec) where

import Data.Aeson (Value (Number, Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (
    Artifact (artEntryKey, artFilename),
    PackageDetails (pkgArtifacts, pkgPublishedAt),
    PackageInfo (infoName, infoVersions),
    PackageName,
    mkPackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Admission (ArtifactAdmission (AdmissionAdmit, AdmissionDenied), admitArtifact)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIIndex, projectPyPIVersion)
import Ecluse.Core.Rules (evalRules, prepare)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext), Rule (AllowByIdentity, AllowIfOlderThan))
import Ecluse.Core.Security (
    LimitError (TooManyVersions),
    Limits (maxVersionCount),
    defaultLimits,
 )
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (defaultMinIntegrity, unsafeFilename)
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    indexSpec
    versionSpec
    paritySpec
    releaseAgeSpec

indexSpec :: Spec
indexSpec = describe "projectPyPIIndex" $ do
    it "projects a well-formed index into the manifest paired with its raw document" $
        case projectPyPIIndex defaultLimits requests (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz"]) of
            Right (info, raw) -> do
                renderPackageName (infoName info) `shouldBe` "requests"
                Map.keys (infoVersions info) `shouldBe` ["2.34.1", "2.34.2"]
                raw `shouldSatisfy` isObject
            other -> expectationFailure ("expected a projection, got: " <> show other)

    it "reports an undecodable body" $
        projectPyPIIndex defaultLimits requests "{not json" `shouldBe` Left MetadataUndecodable

    it "reports an absent top-level name as undecodable" $
        projectPyPIIndex defaultLimits requests (bytes (object ["files" .= ([] :: [Value])]))
            `shouldBe` Left MetadataUndecodable

    it "reports an index self-reporting another project as a name mismatch, not a decode failure" $
        -- The anti-shadowing distinction: a responding upstream serving the wrong project is a
        -- `502`, never a `404` that would read as the project not existing.
        projectPyPIIndex defaultLimits requests (indexNamed "urllib3" [])
            `shouldBe` Left (MetadataNameMismatch "urllib3")

    it "reports a release count past the bound as a bound breach" $
        projectPyPIIndex defaultLimits{maxVersionCount = 1} requests (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz"])
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

versionSpec :: Spec
versionSpec = describe "projectPyPIVersion" $ do
    it "projects one release's files out of an index carrying several" $
        artifactNames (projectPyPIVersion defaultLimits requests (release "2.34.2") (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl", "requests-2.34.1.tar.gz"]))
            `shouldBe` Right (Just ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl"])

    it "yields nothing for a release a sound index does not carry, a forwarded miss" $
        artifactNames (projectPyPIVersion defaultLimits requests (release "9.9.9") (indexOf ["requests-2.34.2.tar.gz"]))
            `shouldBe` Right Nothing

    it "reports an undecodable body" $
        artifactNames (projectPyPIVersion defaultLimits requests (release "2.34.2") "{not json")
            `shouldBe` Left MetadataUndecodable

    it "reports an index self-reporting another project as a name mismatch" $
        artifactNames (projectPyPIVersion defaultLimits requests (release "2.34.2") (indexNamed "urllib3" ["urllib3-2.34.2.tar.gz"]))
            `shouldBe` Left (MetadataNameMismatch "urllib3")

    it "reports a file count past the bound as a bound breach" $
        artifactNames (projectPyPIVersion defaultLimits{maxVersionCount = 1} requests (release "2.34.2") (indexOf ["requests-2.34.2.tar.gz", "requests-2.34.1.tar.gz"]))
            `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

paritySpec :: Spec
paritySpec = describe "the two decode paths agree on what they serve" $ do
    it "retains original entry positions across skipped releases and malformed entries" $ do
        let filename = "requests-1.0.0.tar.gz"
            body = bytes (object ["name" .= ("requests" :: Text), "files" .= [simpleFile "requests-2.0.0.tar.gz", Number 1, object ["filename" .= filename], simpleFile filename, simpleFile filename]])
        (info, _) <- expectRight (projectPyPIIndex defaultLimits requests body)
        selected <- expectRight (projectPyPIVersion defaultLimits requests (release "1") body)
        selected `shouldBe` Map.lookup "1" (infoVersions info)
        (map artEntryKey . toList . pkgArtifacts <$> selected) `shouldBe` Just [ArrayEntry 3, ArrayEntry 4]

    for_ ["2.34.2", "2.34", "1.0.post1"] $ \version ->
        it ("resolves " <> toString version <> " to the same files as the whole-index path") $ do
            let body = indexOf ["requests-2.34.2.tar.gz", "requests-2.34.2-py3-none-any.whl", "requests-2.34.tar.gz", "requests-1.0-1.tar.gz"]
            artifactNames (projectPyPIVersion defaultLimits requests (release version) body)
                `shouldBe` Right (wholeIndexArtifacts version body)

releaseAgeSpec :: Spec
releaseAgeSpec = describe "release age and artifact admission" $
    for_ projections $ \(projectionName, project) -> describe projectionName $
        for_ timestampCases $ \(caseName, timestamps, expectedTime) ->
            it caseName $ do
                let names = ["requests-2.34.2-py3-none-any.whl", "requests-2.34.2-1-py3-none-any.whl"]
                    files = zipWith timestampedFile names timestamps
                    body = bytes (object ["name" .= ("requests" :: Text), "files" .= files])
                    ctx = EvalContext (UTCTime (fromGregorian 2026 9 7) 0) Nothing
                projected <- expectRight (project body)
                case projected of
                    Just details -> do
                        map artFilename (toList (pkgArtifacts details)) `shouldBe` names
                        pkgPublishedAt details `shouldBe` expectedTime
                        ageRules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
                        decision <- evalRules ctx ageRules details
                        admittedBy decision `shouldBe` ("AllowIfOlderThan" <$ expectedTime)
                        exceptionRules <- prepare inertRuleDeps (map atDefaultPrecedence [AllowByIdentity "requests@2.34.2", AllowIfOlderThan (7 * nominalDay)])
                        for_ names $ \name -> do
                            admission <- admitArtifact ctx ageRules defaultMinIntegrity (unsafeFilename name) details
                            case (expectedTime, admission) of
                                (Nothing, AdmissionDenied{}) -> pass
                                (Just _, AdmissionAdmit{}) -> pass
                                other -> expectationFailure ("unexpected age admission: " <> show other)
                            exception <- admitArtifact ctx exceptionRules defaultMinIntegrity (unsafeFilename name) details
                            case exception of
                                AdmissionAdmit{} -> pass
                                other -> expectationFailure ("expected identity exception, got: " <> show other)
                    Nothing -> expectationFailure "expected both retained files"
  where
    projections =
        [ ("full projection", fmap (Map.lookup "2.34.2" . infoVersions . fst) . projectPyPIIndex defaultLimits requests)
        , ("selected projection", projectPyPIVersion defaultLimits requests (release "2.34.2"))
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

requests :: PackageName
requests = mkPackageName PyPI Nothing "requests"

release :: Text -> Version
release = mkVersion PyPI

artifactNames :: Either MetadataError (Maybe PackageDetails) -> Either MetadataError (Maybe [Text])
artifactNames = fmap (fmap (map artFilename . toList . pkgArtifacts))

wholeIndexArtifacts :: Text -> ByteString -> Maybe [Text]
wholeIndexArtifacts version body = case projectPyPIIndex defaultLimits requests body of
    Right (info, _) -> map artFilename . toList . pkgArtifacts <$> Map.lookup version (infoVersions info)
    Left _ -> Nothing

indexOf :: [Text] -> ByteString
indexOf = indexNamed "requests"

indexNamed :: Text -> [Text] -> ByteString
indexNamed name files = bytes (object ["name" .= name, "files" .= map simpleFile files])

bytes :: Value -> ByteString
bytes = BL.toStrict . encode

isObject :: Value -> Bool
isObject = \case
    Object _ -> True
    _ -> False
