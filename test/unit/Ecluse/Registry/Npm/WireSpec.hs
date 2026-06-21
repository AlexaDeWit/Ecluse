module Ecluse.Registry.Npm.WireSpec (spec) where

import Data.Aeson (FromJSON, eitherDecode, eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Test.Hspec

import Ecluse.Registry.Npm.Wire

{- | Decoding tests for the npm wire types. Every fixture under
@test\/unit\/fixtures\/npm\/@ is a body derived from the real captures documented
in @docs\/research\/reverse-engineering\/npm.md@ (§4 full packument, §5
abbreviated, §6 manifest, §3 errors). The suite is pure and offline — it never
touches the network; protocol drift is surfaced separately by the non-gating
smoke suite.

The cases pin down the two things the decoders promise: __faithful__ capture of
the rule-decisive fields (@hasInstallScript@, @deprecated@, the @dist@ integrity
triple, the @time@ map, the @scripts@ map) and __lenient__ input handling
(string-or-object @license@\/@person@\/@bugs@\/@repository@, the bare-string 404,
and ignored unknown keys).
-}
spec :: Spec
spec = do
    abbreviatedPackumentSpec
    fullPackumentSpec
    versionManifestSpec
    distSpec
    lenientScalarSpec
    errorResponseSpec

-- ── abbreviated packument ────────────────────────────────────────────────────

abbreviatedPackumentSpec :: Spec
abbreviatedPackumentSpec = describe "AbbreviatedPackument" $ do
    it "decodes the is-odd abbreviated packument" $ do
        pk <- decodeFixture @AbbreviatedPackument "is-odd.abbreviated.json"
        apkmtName pk `shouldBe` "is-odd"
        Map.lookup "latest" (apkmtDistTags pk) `shouldBe` Just "3.0.1"
        Map.keys (apkmtVersions pk) `shouldBe` ["3.0.1"]

    it "captures the abbreviated-only hasInstallScript flag (core-js)" $ do
        pk <- decodeFixture @AbbreviatedPackument "core-js.abbreviated.json"
        vmHasInstallScript <$> Map.lookup "3.49.0" (apkmtVersions pk)
            `shouldBe` Just (Just True)

    it "leaves hasInstallScript absent when not declared (is-odd)" $ do
        pk <- decodeFixture @AbbreviatedPackument "is-odd.abbreviated.json"
        vmHasInstallScript <$> Map.lookup "3.0.1" (apkmtVersions pk)
            `shouldBe` Just Nothing

    it "decodes a scoped package name verbatim (@babel/code-frame)" $ do
        pk <- decodeFixture @AbbreviatedPackument "babel-code-frame.abbreviated.json"
        apkmtName pk `shouldBe` "@babel/code-frame"
        vmName <$> Map.lookup "7.0.0" (apkmtVersions pk)
            `shouldBe` Just "@babel/code-frame"

    it "parses the top-level modified timestamp" $ do
        pk <- decodeFixture @AbbreviatedPackument "is-odd.abbreviated.json"
        expected <- readUTC "2026-04-14T14:26:11.557Z"
        apkmtModified pk `shouldBe` expected

-- ── full packument ───────────────────────────────────────────────────────────

fullPackumentSpec :: Spec
fullPackumentSpec = describe "Packument (full)" $ do
    it "decodes the is-odd full packument" $ do
        pk <- decodeFixture @Packument "is-odd.full.json"
        pkmtName pk `shouldBe` "is-odd"
        Map.lookup "latest" (pkmtDistTags pk) `shouldBe` Just "3.0.1"
        Map.keys (pkmtVersions pk) `shouldBe` ["3.0.1"]

    it "captures the time map including per-version publish timestamps" $ do
        pk <- decodeFixture @Packument "is-odd.full.json"
        -- The per-version timestamp is the source of truth for publish age.
        published <- readUTC "2018-05-31T20:04:53.306Z"
        created <- readUTC "2015-02-24T05:53:13.392Z"
        Map.lookup "3.0.1" (pkmtTime pk) `shouldBe` Just published
        Map.lookup "created" (pkmtTime pk) `shouldBe` Just created

    it "ignores unknown top-level and manifest keys" $ do
        -- The fixture carries readme/readmeFilename/_rev at the top level and a
        -- `verb` tool-config block plus `gitHead` inside the manifest. A lenient
        -- decoder must not choke on any of them.
        pk <- decodeFixture @Packument "is-odd.full.json"
        pkmtName pk `shouldBe` "is-odd"

    it "captures the scripts map from an embedded full manifest" $ do
        pk <- decodeFixture @Packument "is-odd.full.json"
        vmScripts <$> Map.lookup "3.0.1" (pkmtVersions pk)
            `shouldBe` Just (Map.singleton "test" "mocha")

    it "reads a string license at the package level" $ do
        pk <- decodeFixture @Packument "is-odd.full.json"
        pkmtLicense pk `shouldBe` Just (LicenseSpdx "MIT")

-- ── version manifest ─────────────────────────────────────────────────────────

versionManifestSpec :: Spec
versionManifestSpec = describe "VersionManifest" $ do
    it "decodes a standalone full manifest (core-js)" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        vmName vm `shouldBe` "core-js"
        vmVersion vm `shouldBe` "3.49.0"

    it "captures the full-form scripts map (no hasInstallScript key)" $ do
        -- core-js full manifest has scripts.postinstall but NO hasInstallScript;
        -- a later slice derives install-script presence from this map.
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        vmHasInstallScript vm `shouldBe` Nothing
        Map.keys (vmScripts vm) `shouldBe` ["postinstall"]

    it "captures the deprecation notice (request)" $ do
        vm <- decodeFixture @VersionManifest "request.manifest.json"
        vmDeprecated vm
            `shouldBe` Just
                "request has been deprecated, see https://github.com/request/request/issues/3142"

    it "captures runtime dependencies as raw ranges" $ do
        vm <- decodeFixture @VersionManifest "request.manifest.json"
        Map.lookup "form-data" (vmDependencies vm) `shouldBe` Just "~2.3.2"

-- ── dist ─────────────────────────────────────────────────────────────────────

distSpec :: Spec
distSpec = describe "Dist" $ do
    it "captures the integrity triple (tarball, shasum, integrity)" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        let d = vmDist vm
        distTarball d `shouldBe` "https://registry.npmjs.org/core-js/-/core-js-3.49.0.tgz"
        distShasum d `shouldBe` Just "aaaabbbbccccddddeeeeffff0000111122223333"
        distIntegrity d
            `shouldBe` Just "sha512-AAAABBBBCCCCDDDDEEEEFFFF00001111222233334444555566667777888899=="

    it "captures fileCount, unpackedSize, and signatures when present" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        let d = vmDist vm
        distFileCount d `shouldBe` Just 1455
        distUnpackedSize d `shouldBe` Just 6789012
        sigKeyid <$> distSignatures d `shouldBe` ["SHA256:jl3bwswu80"]

    it "tolerates a dist with only the required tarball field" $
        decodesTo @Dist
            "{\"tarball\":\"https://example.test/x.tgz\"}"
            ( Dist
                { distTarball = "https://example.test/x.tgz"
                , distShasum = Nothing
                , distIntegrity = Nothing
                , distFileCount = Nothing
                , distUnpackedSize = Nothing
                , distSignatures = []
                }
            )

-- ── lenient string-or-object scalars ─────────────────────────────────────────

lenientScalarSpec :: Spec
lenientScalarSpec = describe "lenient string-or-object scalars" $ do
    describe "License" $ do
        it "accepts a bare SPDX string" $
            decodesTo @License "\"MIT\"" (LicenseSpdx "MIT")
        it "accepts the legacy object form" $
            decodesTo @License
                "{\"type\":\"Apache-2.0\",\"url\":\"https://apache.org/l\"}"
                (LicenseObject "Apache-2.0" (Just "https://apache.org/l"))
        it "reads the legacy object license from the request manifest" $ do
            vm <- decodeFixture @VersionManifest "request.manifest.json"
            vmLicense vm
                `shouldBe` Just (LicenseObject "Apache-2.0" (Just "https://www.apache.org/licenses/LICENSE-2.0"))

    describe "Person" $ do
        it "accepts a packed string and keeps it verbatim in personName" $
            decodesTo @Person
                "\"Mikeal Rogers <mikeal.rogers@gmail.com>\""
                (Person "Mikeal Rogers <mikeal.rogers@gmail.com>" Nothing Nothing)
        it "accepts the object form" $
            decodesTo @Person
                "{\"name\":\"mikeal\",\"email\":\"m@example.com\"}"
                (Person "mikeal" (Just "m@example.com") Nothing)
        it "accepts a maintainers list mixing object and string forms (request)" $ do
            vm <- decodeFixture @VersionManifest "request.manifest.json"
            vmMaintainers vm
                `shouldBe` [ Person "mikeal" (Just "mikeal.rogers@gmail.com") Nothing
                           , Person "simov <simeonvelichkov@gmail.com>" Nothing Nothing
                           ]

    describe "Repository" $ do
        it "accepts a bare shorthand string" $
            decodesTo @Repository
                "\"github:user/repo\""
                (Repository Nothing "github:user/repo")
        it "accepts the object form" $
            decodesTo @Repository
                "{\"type\":\"git\",\"url\":\"git+https://x/r.git\"}"
                (Repository (Just "git") "git+https://x/r.git")

    describe "Bugs" $ do
        it "accepts a bare URL string" $
            decodesTo @Bugs
                "\"https://github.com/x/issues\""
                (Bugs (Just "https://github.com/x/issues") Nothing)
        it "accepts the object form" $
            decodesTo @Bugs
                "{\"url\":\"https://x/issues\",\"email\":\"b@x\"}"
                (Bugs (Just "https://x/issues") (Just "b@x"))

-- ── error responses ──────────────────────────────────────────────────────────

errorResponseSpec :: Spec
errorResponseSpec = describe "ErrorResponse" $ do
    it "tolerates the bare-string per-version 404 body" $ do
        -- npm's per-version 404 is a bare JSON string, not an object (§3).
        err <- decodeFixture @ErrorResponse "error-version-not-found.json"
        err `shouldBe` ErrorString "version not found: ^3.0.0"
        errorMessage err `shouldBe` Just "version not found: ^3.0.0"

    it "decodes the {error} object form (unknown package)" $ do
        err <- decodeFixture @ErrorResponse "error-not-found.json"
        err `shouldBe` ErrorObject ErrorBody{errMessage = Nothing, errError = Just "Not found"}
        errorMessage err `shouldBe` Just "Not found"

    it "decodes the {message} object form and prefers message over error" $ do
        err <- decodeFixture @ErrorResponse "error-method-not-allowed.json"
        err `shouldBe` ErrorObject ErrorBody{errMessage = Just "GET is not allowed", errError = Nothing}
        errorMessage err `shouldBe` Just "GET is not allowed"

-- ── helpers ──────────────────────────────────────────────────────────────────

{- | Decode a committed fixture by file name (under @test\/unit\/fixtures\/npm\/@,
a path relative to the package root that Cabal runs tests from), failing the
example with the aeson error on a decode failure.
-}
decodeFixture :: forall a. (FromJSON a) => FilePath -> IO a
decodeFixture name = do
    bytes <- readFileBS ("test/unit/fixtures/npm/" <> name)
    case eitherDecodeStrict bytes of
        Right a -> pure a
        Left e -> fail ("failed to decode " <> name <> ": " <> e)

{- | Assert that a JSON literal decodes to an expected value. Keeps the inline
lenient-form cases to a single readable line.
-}
decodesTo :: forall a. (FromJSON a, Eq a, Show a) => LByteString -> a -> Expectation
decodesTo json expected = eitherDecode json `shouldBe` Right expected

{- | Parse an ISO-8601 timestamp for an expectation. Runs in the example\'s own
'MonadFail' (here @IO@), so an unparseable literal fails the test rather than
crashing — keeping the suite total (no partial @error@; see STYLE.md §10).
-}
readUTC :: (MonadFail m) => Text -> m UTCTime
readUTC = iso8601ParseM . toString
