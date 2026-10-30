-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm publication through a recording transport stub preserves verified bytes and checked upload claims.
module Ecluse.Core.Registry.Npm.PublishSpec (spec) where

import Data.Aeson (Object, Value (Bool, Null, Number, Object, String), encode, object, toJSON, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types.Status (status200, status404, status409, status500)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportFault (tfDetail))
import Ecluse.Core.Package (Artifact (artFilename, artHashes, artSize), HashAlg (SHA1, SHA256, SRI), PackageDetails (pkgArtifacts, pkgName, pkgVersion), PackageInfo (..), mkHash, mkSriHashes)
import Ecluse.Core.Package.Integrity (IntegrityResult (IntegrityVerified), verifyIntegrity)
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
    MirrorArtifact (maHashes, maSize),
    ParseError (ParseError),
    PublishBodyFault (PublishMalformed, PublishUnsupported),
    PublishError (publishErrorMessage),
    PublishFault (PublishFetch, PublishIntegrityMismatch, PublishRejected, PublishSourceUnavailable),
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails, vdRaw))
import Ecluse.Core.Registry.MirrorUpload (inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Registry.Npm.Publish (npmNewReleaseOnly, npmPublishCodec, npmPublishDocument, prepareNpmSource, prepareNpmUpload, uploadClaims)
import Ecluse.Core.Registry.Publish (
    MirrorPublish,
    PublishCodec (pcNativeComplete),
    PublishPlan (PublishPlan, ppLatest),
 )
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (hexSha1Of, hexSha256Of, pypiVersion, sriSha256Of, sriSha512Of, unsafeHash, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Registry.Npm (dummyArtifact, isOdd, isOddVersionDoc, mirrorVersionDoc)
import Ecluse.Test.Registry.Npm.Publish (npmDependentRelease, npmDeprecateDocument, npmExternalDependencyRefusal, npmMisfiledVersion, npmProvenanceRelease, npmReleaseDocument, npmRepeatedAttachments, npmStarDocument, npmTaggedRelease)
import Ecluse.Test.Registry.Publish (anonymousPublishAt, checkedNpmUpload, isMalformedBody, publishOnlyFile)
import Ecluse.Test.Registry.PyPI.Metadata (documentFromValue)
import Ecluse.Test.Support (decodeJsonOrFail, expectRight)

import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Stub (
    Stub,
    allCaptured,
    capBody,
    capMethod,
    capPath,
    headerValue,
    lastCaptured,
    stubBaseUrl,
    withStub,
 )

-- | npm publication outcomes and the integrity carried with attached bytes.
spec :: Spec
spec = do
    publishSpec
    integritySpec
    fieldRewriteSpec
    checkedUploadDataSpec
    nativeCompletionSpec
    uploadGuardSpec

publishSpec :: Spec
publishSpec = describe "the npm mirror write (codec over the shared transport)" $ do
    it "PUTs the publish document to the package path" $
        withStub status200 "{}" $ \stub -> withSweeper $ \deadlines -> do
            publish <- stubPublish deadlines stub
            _ <- publishFixture publish isOddVersionDoc planV1 sizedArtifact dummyTarballBytes
            cap <- lastCaptured stub
            capMethod cap `shouldBe` "PUT"
            capPath cap `shouldBe` "/is-odd"
            publishDoc `shouldReturn` capBody cap
            headerValue "content-type" cap `shouldBe` Just "application/json"

    it "declares the plan's latest, not the version it publishes" $ do
        let plan = planV1{ppLatest = mkVersion Npm "2.0.0"}
        document <- decodeJsonOrFail =<< expectRight (documentOf isOddVersionDoc plan sizedArtifact) :: IO Object
        tags <- expectRight (parseEither (.: "dist-tags") document)
        versions <- expectRight (parseEither (.: "versions") document) :: IO Object
        KeyMap.lookup "latest" tags `shouldBe` Just (String "2.0.0")
        KeyMap.keys versions `shouldBe` ["1.0.0"]

    it "treats a 2xx as success" $
        withStub status200 "{}" $ \stub -> withSweeper $ \deadlines -> do
            publish <- stubPublish deadlines stub
            publishFixture publish isOddVersionDoc planV1 sizedArtifact dummyTarballBytes `shouldReturn` Right ()

    it "treats a 409 Conflict as idempotent success (the immutable version is already present)" $
        withStub status409 "{\"error\":\"version already exists\"}" $ \stub -> withSweeper $ \deadlines -> do
            publish <- stubPublish deadlines stub
            publishFixture publish isOddVersionDoc planV1 sizedArtifact dummyTarballBytes `shouldReturn` Right ()

    it "reports a 404 as a publish error naming the status (so the mirror job is retried)" $
        withStub status404 "{\"error\":\"Not found\"}" $ \stub -> withSweeper $ \deadlines -> do
            publish <- stubPublish deadlines stub
            outcome <- publishFixture publish isOddVersionDoc planV1 sizedArtifact dummyTarballBytes
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> withSweeper $ \deadlines -> do
            publish <- stubPublish deadlines stub
            outcome <- publishFixture publish isOddVersionDoc planV1 sizedArtifact dummyTarballBytes
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "500")

    it "reports a transport failure as a PublishFetch value, never thrown" $ withSweeper $ \deadlines -> do
        publish <- publishAt deadlines "http://127.0.0.1:1"
        outcome <- publishFixture publish isOddVersionDoc planV1 sizedArtifact dummyTarballBytes
        outcome `shouldSatisfy` isTransport

integritySpec :: Spec
integritySpec = describe "verified integrity in the published document" $ do
    let matching = sriSha512Of dummyTarballBytes
        nonmatching = sriSha512Of "other tarball bytes"
        weaker = sriSha256Of dummyTarballBytes
    for_
        [ ("keeps a nonmatching alternative before a matching alternative", [nonmatching, matching], Just (nonmatching <> " " <> matching))
        , ("keeps a matching alternative before a nonmatching alternative", [matching, nonmatching], Just (matching <> " " <> nonmatching))
        , ("drops a weaker first token and keeps every strongest alternative", [weaker, nonmatching, matching], Just (nonmatching <> " " <> matching))
        , ("preserves a single SRI token", [matching], Just matching)
        , ("preserves a SHA1-only artifact without adding SRI", [], Nothing)
        ]
        $ \(label, tokens, expectedIntegrity) ->
            it label (assertPublishedIntegrity tokens expectedIntegrity)

assertPublishedIntegrity :: [Text] -> Maybe Text -> Expectation
assertPublishedIntegrity tokens expectedIntegrity =
    withStub status200 "{}" $ \stub -> withSweeper $ \deadlines -> do
        let shasum = T.toUpper (hexSha1Of dummyTarballBytes)
            hashes = unsafeHash SHA1 shasum :| map (unsafeHash SRI) tokens
            artifact = sizedArtifact{maHashes = hashes}
        verifyIntegrity hashes dummyTarballBytes `shouldBe` IntegrityVerified
        publish <- stubPublish deadlines stub
        publishFixture publish isOddVersionDoc planV1 artifact dummyTarballBytes `shouldReturn` Right ()
        cap <- lastCaptured stub
        document <- decodeJsonOrFail (capBody cap) :: IO Object
        manifest <- expectRight (parseEither (\o -> o .: "versions" >>= (.: "1.0.0")) document)
        dist <- expectRight (parseEither (.: "dist") manifest)
        attachment <- expectRight (parseEither (\o -> o .: "_attachments" >>= (.: "is-odd-1.0.0.tgz")) document)
        tags <- expectRight (parseEither (.: "dist-tags") document)
        KeyMap.lookup "_id" document `shouldBe` Just (String "is-odd")
        KeyMap.lookup "name" document `shouldBe` Just (String "is-odd")
        KeyMap.lookup "latest" tags `shouldBe` Just (String "1.0.0")
        KeyMap.lookup "name" manifest `shouldBe` Just (String "is-odd")
        KeyMap.lookup "version" manifest `shouldBe` Just (String "1.0.0")
        KeyMap.lookup "tarball" dist `shouldBe` Just (String "is-odd-1.0.0.tgz")
        KeyMap.lookup "integrity" dist `shouldBe` (String <$> expectedIntegrity)
        KeyMap.lookup "shasum" dist `shouldBe` Just (String shasum)
        KeyMap.lookup "content_type" attachment `shouldBe` Just (String "application/octet-stream")
        KeyMap.lookup "length" attachment `shouldBe` Just (toJSON (BS.length dummyTarballBytes))
        encoded <- expectRight (parseEither (.: "data") attachment) :: IO Text
        bytes <- expectRight (convertFromBase Base64 (encodeUtf8 encoded :: ByteString))
        bytes `shouldBe` dummyTarballBytes
        integrity <- expectRight (parseEither (.:? "integrity") dist)
        case integrity of
            Just carrier -> do
                publishedHashes <- expectRight (mkSriHashes carrier)
                verifyIntegrity publishedHashes bytes `shouldBe` IntegrityVerified
            Nothing -> do
                raw <- expectRight (parseEither (.: "shasum") dist)
                publishedHash <- expectRight (mkHash SHA1 raw)
                verifyIntegrity (publishedHash :| []) bytes `shouldBe` IntegrityVerified

fieldRewriteSpec :: Spec
fieldRewriteSpec = describe "the field-rewrite contract on the published version object" $ do
    it "keeps what the author wrote: dependencies, executables, policy inputs, and an unknown field" $ do
        manifest <- publishedManifest
        forM_ ["dependencies", "bin", "scripts", "engines", "license", "gitHead"] $ \field ->
            KeyMap.lookup field manifest `shouldBe` KeyMap.lookup field sourceObject

    it "keeps a deprecation notice verbatim" $ do
        manifest <- publishedManifest
        KeyMap.lookup "deprecated" manifest `shouldBe` Just (String "use is-even instead")

    it "rewrites only the validated name and version under local authority" $ do
        manifest <- publishedManifest
        KeyMap.lookup "name" manifest `shouldBe` Just (String "is-odd")
        KeyMap.lookup "version" manifest `shouldBe` Just (String "1.0.0")

    it "replaces the dist location and digests with the verified ones and keeps the rest of dist" $ do
        dist <- distOf <$> publishedManifest
        KeyMap.lookup "tarball" dist `shouldBe` Just (String "is-odd-1.0.0.tgz")
        KeyMap.lookup "integrity" dist `shouldBe` Just (String verifiedSri)
        KeyMap.lookup "shasum" dist `shouldBe` Just (String (hexSha1Of dummyTarballBytes))
        KeyMap.lookup "unpackedSize" dist `shouldBe` Just (Number 4096)
        KeyMap.lookup "fileCount" dist `shouldBe` Just (Number 3)

    it "strips dist.signatures and dist.attestations, which reference the public registry's own keys" $ do
        dist <- distOf <$> publishedManifest
        KeyMap.lookup "signatures" dist `shouldBe` Nothing
        KeyMap.lookup "attestations" dist `shouldBe` Nothing

    it "keeps the shrinkwrap installation marker while stripping registry bookkeeping" $ do
        manifest <- publishedManifest
        filter (T.isPrefixOf "_" . Key.toText) (KeyMap.keys manifest) `shouldBe` ["_hasShrinkwrap"]
        KeyMap.lookup "_hasShrinkwrap" manifest `shouldBe` Just (Bool False)

    it "never lets an unverified source digest survive the absence of a verified one" $ do
        document <- decodeJsonOrFail =<< expectRight (documentOf (fst npmCached sourceVersion) planV1 sizedArtifact{maHashes = unsafeHash SHA256 (hexSha256Of dummyTarballBytes) :| []}) :: IO Object
        dist <- distOf <$> (expectRight (parseEither (\o -> o .: "versions" >>= (.: "1.0.0")) document) :: IO Object)
        KeyMap.lookup "integrity" dist `shouldBe` Nothing
        KeyMap.lookup "shasum" dist `shouldBe` Nothing
        KeyMap.lookup "tarball" dist `shouldBe` Just (String "is-odd-1.0.0.tgz")

    it "refuses, as a value, a version object another ecosystem injected" $
        checked (fst pypiSimpleCached (documentFromValue sourceVersion)) `shouldSatisfy` isSourceRefusal

    it "refuses, as a value, a carried version object that is not a JSON object" $
        checked (fst npmCached (String "not an object")) `shouldSatisfy` isSourceRefusal

    it "writes nothing to the mirror target for a refused version object" $
        withStub status200 "{}" $ \stub -> withSweeper $ \deadlines -> do
            publish <- stubPublish deadlines stub
            outcome <- publishFixture publish (fst npmCached (String "not an object")) planV1 sizedArtifact dummyTarballBytes
            outcome `shouldSatisfy` isSourceRefusal
            allCaptured stub `shouldReturn` []
  where
    publishedManifest :: IO Object
    publishedManifest = do
        document <- decodeJsonOrFail =<< expectRight (documentOf (fst npmCached sourceVersion) planV1 verifiedArtifact) :: IO Object
        expectRight (parseEither (\o -> o .: "versions" >>= (.: "1.0.0")) document)
    checked raw = prepareNpmSource isOdd v1_0_0 (mirrorVersionDoc isOdd v1_0_0 raw verifiedArtifact)
    verifiedArtifact = sizedArtifact{maHashes = unsafeHash SHA1 (hexSha1Of dummyTarballBytes) :| [unsafeHash SRI verifiedSri]}
    distOf :: Object -> Object
    distOf manifest = case KeyMap.lookup "dist" manifest of
        Just (Object dist) -> dist
        _ -> mempty
    verifiedSri = sriSha512Of dummyTarballBytes

-- The version object as the public registry serves it: author fields, registry bookkeeping, and a
-- name, version, and dist that must not reach the mirror as written.
sourceVersion :: Value
sourceVersion = Object sourceObject

sourceObject :: Object
sourceObject =
    KeyMap.fromList
        [ "name" .= ("shadowed-name" :: Text)
        , "version" .= ("9.9.9" :: Text)
        , "dist"
            .= object
                [ "tarball" .= ("https://registry.npmjs.org/is-odd/-/is-odd-1.0.0.tgz" :: Text)
                , "integrity" .= ("sha512-forged" :: Text)
                , "shasum" .= ("0000000000000000000000000000000000000000" :: Text)
                , "signatures" .= [object ["keyid" .= ("SHA256:npm" :: Text), "sig" .= ("MEUC" :: Text)]]
                , "attestations" .= object ["url" .= ("https://registry.npmjs.org/-/npm/v1/attestations/is-odd@1.0.0" :: Text)]
                , "unpackedSize" .= (4096 :: Int)
                , "fileCount" .= (3 :: Int)
                ]
        , "dependencies" .= object ["is-number" .= ("^6.0.0" :: Text)]
        , "bin" .= object ["is-odd" .= ("cli.js" :: Text)]
        , "scripts" .= object ["test" .= ("jest" :: Text)]
        , "engines" .= object ["node" .= (">=18" :: Text)]
        , "license" .= ("MIT" :: Text)
        , "deprecated" .= ("use is-even instead" :: Text)
        , "gitHead" .= ("0123456789abcdef0123456789abcdef01234567" :: Text)
        , "_id" .= ("is-odd@1.0.0" :: Text)
        , "_npmUser" .= object ["name" .= ("publisher" :: Text)]
        , "_nodeVersion" .= ("20.11.0" :: Text)
        , "_hasShrinkwrap" .= Bool False
        ]

stubPublish :: Deadlines -> Stub -> IO MirrorPublish
stubPublish deadlines stub = publishAt deadlines (stubBaseUrl stub)

publishAt :: Deadlines -> Text -> IO MirrorPublish
publishAt deadlines = anonymousPublishAt deadlines npmPublishCodec . loopbackRegistryUrl

sizedArtifact :: MirrorArtifact
sizedArtifact = dummyArtifact{maSize = Just 1234, maHashes = unsafeHash SHA1 (hexSha1Of dummyTarballBytes) :| []}

dummyTarballBytes :: ByteString
dummyTarballBytes = "tarball-bytes"

planV1 :: PublishPlan
planV1 = PublishPlan{ppLatest = v1_0_0}

publishDoc :: IO ByteString
publishDoc = expectRight (documentOf isOddVersionDoc planV1 sizedArtifact)

documentOf :: CachedDoc -> PublishPlan -> MirrorArtifact -> Either PublishFault ByteString
documentOf raw plan artifact =
    npmPublishDocument plan <$> checkedNpmUpload (mirrorVersionDoc isOdd v1_0_0 raw artifact) dummyTarballBytes

publishFixture :: MirrorPublish -> CachedDoc -> PublishPlan -> MirrorArtifact -> ByteString -> IO (Either PublishFault ())
publishFixture publish raw plan artifact =
    publishOnlyFile publish (mirrorVersionDoc isOdd v1_0_0 raw artifact) plan

checkedUploadDataSpec :: Spec
checkedUploadDataSpec = describe "checked mirror upload data" $ do
    let doc = mirrorVersionDoc isOdd v1_0_0 isOddVersionDoc sizedArtifact
        details = vdDetails doc
        changed update = doc{vdDetails = update details}
        check = prepareNpmSource isOdd v1_0_0
    it "refuses fields from another snapshot of the same package and version" $ do
        (inventory, _) <- expectRight (check doc)
        (_, otherMetadata) <- expectRight (check doc{vdRaw = Just (fst npmCached sourceVersion)})
        let file :| _ = inventoryFiles inventory
        verified <- expectRight (verifyMirrorBytes file dummyTarballBytes)
        prepareNpmUpload inventory otherMetadata file verified `shouldSatisfy` isSourceRefusal
    it "refuses another ecosystem even when its snapshot identity agrees" $ do
        let name = unscopedPyPI "is-odd"
            version = pypiVersion "1.0.0"
        prepareNpmSource name version (mirrorVersionDoc name version isOddVersionDoc sizedArtifact) `shouldSatisfy` isSourceRefusal
    it "refuses missing source metadata" $
        check doc{vdRaw = Nothing} `shouldSatisfy` isSourceRefusal
    it "refuses a snapshot belonging to another package" $
        check (changed (\d -> d{pkgName = unscopedNpm "other"})) `shouldSatisfy` isSourceRefusal
    it "refuses a snapshot belonging to another version" $
        check (changed (\d -> d{pkgVersion = mkVersion Npm "2.0.0"})) `shouldSatisfy` isSourceRefusal
    it "refuses to publish one selected artifact from a larger inventory" $
        check (changed (\d -> d{pkgArtifacts = fmap (\a -> a{artFilename = "another.tgz"}) (pkgArtifacts d) <> pkgArtifacts d})) `shouldSatisfy` isSourceRefusal

nativeCompletionSpec :: Spec
nativeCompletionSpec = describe "native atomic npm completion" $ do
    let details = vdDetails (mirrorVersionDoc isOdd v1_0_0 isOddVersionDoc sizedArtifact)
        inventory = PackageInfo isOdd (Map.singleton "1.0.0" details) mempty []
        complete = pcNativeComplete npmPublishCodec isOdd v1_0_0
        changed update = inventory{infoVersions = Map.singleton "1.0.0" (update details)}
    it "proves one supported native artifact without a source snapshot" $
        complete inventory `shouldBe` True
    it "refuses missing versions and mismatched package identity" $ do
        complete inventory{infoVersions = mempty} `shouldBe` False
        complete inventory{infoName = unscopedNpm "other"} `shouldBe` False
    it "refuses multiple artifacts and mismatched version facts" $ do
        complete (changed (\d -> d{pkgArtifacts = pkgArtifacts d <> pkgArtifacts d})) `shouldBe` False
        complete (changed (\d -> d{pkgVersion = mkVersion Npm "2.0.0"})) `shouldBe` False
    it "refuses malformed filenames, absent digests and negative sizes" $
        for_ [\a -> a{artFilename = "other-1.0.0.tgz"}, \a -> a{artHashes = []}, \a -> a{artSize = Just (-1)}] $ \update ->
            complete (changed (\d -> d{pkgArtifacts = fmap update (pkgArtifacts d)})) `shouldBe` False

uploadGuardSpec :: Spec
uploadGuardSpec = describe "uploadClaims (the first-party new-release guard)" $ do
    it "reads every declared name of a real publish, with and without a provenance attachment" $ do
        uploadClaims (toStrict (npmReleaseDocument widget widget widget)) `shouldBe` Right [widget, widget, widget]
        uploadClaims (toStrict npmProvenanceRelease) `shouldBe` Right [widget, widget, widget]
    it "never reads the attachment data" $ do
        body <- editedRelease (KeyMap.insert "_attachments" (object ["widget.tgz" .= object ["data" .= ("not base64" :: Text)]]))
        uploadClaims body `shouldBe` Right [widget, widget, widget]
    for_ [("deprecate", npmDeprecateDocument "use 1.1.0"), ("undeprecate", npmDeprecateDocument ""), ("star", npmStarDocument)] $ \(command, body) ->
        it ("refuses the body of npm " <> command <> " as a write it does not relay") $
            uploadClaims (toStrict body) `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
    it "refuses a release with two versions or none, or with no attachment" $ do
        twoVersions <- editedRelease (KeyMap.insertWith withVersions "versions" (object ["1.0.0" .= object ["name" .= widget, "version" .= ("1.0.0" :: Text)]]))
        uploadClaims twoVersions `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
        noVersions <- editedRelease (KeyMap.insert "versions" (object []))
        uploadClaims noVersions `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
        bare <- editedRelease (KeyMap.insert "_attachments" (object []))
        uploadClaims bare `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
    it "refuses an undecodable body as malformed" $ do
        uploadClaims "{\"_id\":" `shouldSatisfy` isMalformedBody
        uploadClaims "[]" `shouldSatisfy` isMalformedBody
        uploadClaims (toStrict (npmReleaseDocument widget widget widget) <> " {}") `shouldSatisfy` isMalformedBody
    it "refuses a body that repeats a key, at the top or inside a version, as malformed" $ do
        uploadClaims (toStrict npmRepeatedAttachments) `shouldSatisfy` isMalformedBody
        uploadClaims repeatedVersionName `shouldSatisfy` isMalformedBody
        uploadClaims escapedRepeat `shouldSatisfy` isMalformedBody
    it "refuses a release whose tag points at another version or is null" $ do
        uploadClaims (toStrict (npmTaggedRelease (object ["latest" .= ("1.0.0" :: Text)]))) `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
        uploadClaims (toStrict (npmTaggedRelease (object ["latest" .= ("2.0.0" :: Text), "beta" .= Null]))) `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
        uploadClaims (toStrict (npmTaggedRelease (String "2.0.0"))) `shouldBe` Left (PublishUnsupported npmNewReleaseOnly)
    it "refuses a version filed under another key, or without a string version field, as malformed" $ do
        uploadClaims (toStrict npmMisfiledVersion) `shouldSatisfy` isMalformedBody
        unversioned <- editedRelease (KeyMap.insert "versions" (object ["2.0.0" .= object ["name" .= widget]]))
        uploadClaims unversioned `shouldSatisfy` isMalformedBody
        numbered <- editedRelease (KeyMap.insert "versions" (object ["2.0.0" .= object ["name" .= widget, "version" .= (2 :: Int)]]))
        uploadClaims numbered `shouldSatisfy` isMalformedBody
    it "refuses a version document that is not an object as malformed" $ do
        bare <- editedRelease (KeyMap.insert "versions" (object ["2.0.0" .= ("2.0.0" :: Text)]))
        uploadClaims bare `shouldSatisfy` isMalformedBody
    it "relays a release under another tag, or with no dist-tags member" $ do
        uploadClaims (toStrict (npmTaggedRelease (object ["beta" .= ("2.0.0" :: Text)]))) `shouldBe` Right [widget, widget, widget]
        untagged <- editedRelease (KeyMap.delete "dist-tags")
        uploadClaims untagged `shouldBe` Right [widget, widget, widget]
    it "refuses an installable dependency that names a git source or a remote URL, naming the dependency" $
        for_ installableFields $ \field ->
            for_ externalSpecs $ \(written, kind) ->
                uploadClaims (toStrict (npmDependentRelease [Key.fromText field .= object ["left-pad" .= written]]))
                    `shouldBe` Left (PublishMalformed (ParseError (npmExternalDependencyRefusal "left-pad" field kind)))
    it "relays registry, alias and local specs, and a git source only in devDependencies" $ do
        for_ installableFields $ \field ->
            for_ inRegistrySpecs $ \written ->
                uploadClaims (toStrict (npmDependentRelease [Key.fromText field .= object ["left-pad" .= written]])) `shouldBe` Right [widget, widget, widget]
        uploadClaims (toStrict (npmDependentRelease ["devDependencies" .= object ["left-pad" .= ("github:acme/left-pad" :: Text)]])) `shouldBe` Right [widget, widget, widget]
    it "refuses the first external dependency by field order, then by name" $ do
        let git = "github:acme/left-pad" :: Text
            release =
                npmDependentRelease
                    [ "peerDependencies" .= object ["beta" .= git]
                    , "dependencies" .= object ["safe" .= ("^1.0.0" :: Text)]
                    , "optionalDependencies" .= object ["zeta" .= git, "alpha" .= ("https://example.test/alpha.tgz" :: Text)]
                    ]
        uploadClaims (toStrict release) `shouldBe` Left (PublishMalformed (ParseError (npmExternalDependencyRefusal "alpha" "optionalDependencies" "a remote URL")))
    it "ignores a dependency map that is not an object and a spec that is not a string"
        $ for_
            [ toJSON ["github:acme/left-pad" :: Text]
            , String "github:acme/left-pad"
            , Null
            , object ["left-pad" .= (1 :: Int)]
            , object ["left-pad" .= ["github:acme/left-pad" :: Text]]
            , object ["left-pad" .= object ["url" .= ("github:acme/left-pad" :: Text)]]
            ]
        $ \dependencies ->
            uploadClaims (toStrict (npmDependentRelease ["dependencies" .= dependencies])) `shouldBe` Right [widget, widget, widget]
  where
    installableFields = ["dependencies", "optionalDependencies", "peerDependencies"]
    externalSpecs :: [(Text, Text)]
    externalSpecs =
        [ (written, "a git source")
        | written <-
            [ "git+https://github.com/acme/left-pad.git"
            , "git://github.com/acme/left-pad.git"
            , "git+ssh://git@github.com:acme/left-pad.git"
            , "ssh://git@github.com/acme/left-pad.git"
            , "git@github.com:acme/left-pad.git"
            , "git@github.com/acme/left-pad"
            , "github:acme/left-pad"
            , "gitlab:acme/left-pad"
            , "bitbucket:acme/left-pad"
            , "gist:11081aaa"
            , "sourcehut:~acme/left-pad"
            , "acme/left-pad"
            , "acme/left-pad#v1.0.0"
            , "GIT+HTTPS://github.com/acme/left-pad.git"
            , " github:acme/left-pad"
            , "git\t+https://github.com/acme/left-pad.git"
            , "user:secret@github.com:acme/left-pad"
            ]
        ]
            <> [(written, "a remote URL") | written <- ["https://example.test/left-pad-1.0.0.tgz", "http://example.test/left-pad-1.0.0.tgz", "HTTPS://example.test/left-pad.tgz"]]
    inRegistrySpecs :: [Text]
    inRegistrySpecs =
        [ "^1.2.3"
        , "1.x"
        , "latest"
        , "*"
        , ""
        , ">=1.0.0 <2.0.0"
        , "npm:left-pad@^1"
        , "NPM:left-pad@^1"
        , "file:../left-pad"
        , "./left-pad"
        , "../left-pad"
        , "/opt/left-pad"
        , "~/left-pad"
        , "C:\\left-pad"
        , "link:../left-pad"
        , "workspace:*"
        , "workspace:left-pad@*"
        , "a/b/c"
        , "packages/@acme/widget"
        , "@acme/widget"
        ]
    widget :: Text
    widget = "@acme/widget"
    -- A last-wins store reads this version's name as the second, out-of-scope copy.
    repeatedVersionName :: ByteString
    repeatedVersionName = "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"2.0.0\":{\"name\":\"@acme/widget\",\"name\":\"@victim/target\",\"version\":\"2.0.0\"}},\"_attachments\":{\"widget-2.0.0.tgz\":{\"data\":\"AA==\"}}}"
    -- The second key spells _attachments with an escape, so only the decoded keys repeat.
    escapedRepeat :: ByteString
    escapedRepeat = "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"2.0.0\":{\"name\":\"@acme/widget\",\"version\":\"2.0.0\"}},\"_attachments\":{\"widget-2.0.0.tgz\":{\"data\":\"AA==\"}},\"\\u005fattachments\":{}}"
    -- Each refused shape differs from a real publish in one place.
    editedRelease edit = toStrict . encode . edit <$> decodeJsonOrFail @Object (toStrict (npmReleaseDocument widget widget widget))
    withVersions extra own = case (extra, own) of
        (Object added, Object versions) -> Object (added <> versions)
        _ -> extra

isSourceRefusal :: Either PublishFault a -> Bool
isSourceRefusal = \case
    Left (PublishSourceUnavailable _) -> True
    _ -> False

leftMessage :: Either PublishFault a -> Maybe Text
leftMessage outcome = case outcome of
    Left (PublishIntegrityMismatch detail) -> Just detail
    Left (PublishRejected err) -> Just (publishErrorMessage err)
    Left (PublishFetch (FetchTransport fault)) -> Just (tfDetail fault)
    Left (PublishFetch _) -> Nothing
    Left (PublishSourceUnavailable detail) -> Just detail
    Right _ -> Nothing

isTransport :: Either PublishFault a -> Bool
isTransport = \case
    Left (PublishFetch (FetchTransport _)) -> True
    _ -> False
