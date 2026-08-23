-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- The totality properties below are polymorphic in the decoded type @a@, which appears
-- only under a type application at each call site (e.g. @valueDecodeIsTotal \@Person@).
-- That is what AllowAmbiguousTypes is for.
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ecluse.Registry.Npm.WireSpec (spec) where

import Data.Aeson (
    FromJSON,
    Result (Error, Success),
    Value (String),
    eitherDecode,
    eitherDecodeStrict,
    fromJSON,
 )
import Data.Map.Strict qualified as Map
import Hedgehog (PropertyT, annotateShow, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Registry.Npm.Wire
import Ecluse.Test.Json (genValue)
import Ecluse.Test.Support (decodeJsonOrFail)

{- | Decoding tests for the npm wire types, pure and offline over the fixtures in
@core\/test\/unit\/fixtures\/npm\/@, live captures of @registry.npmjs.org@.
They pin faithful capture of the rule-decisive fields and lenient string-or-object handling.
-}
spec :: Spec
spec = do
    versionManifestSpec
    distSpec
    advisoryFieldLeniencySpec
    lenientScalarSpec
    jsonListSpec
    totalitySpec

versionManifestSpec :: Spec
versionManifestSpec = describe "VersionManifest" $ do
    it "decodes a standalone full manifest (core-js)" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        vmName vm `shouldBe` "core-js"
        vmVersion vm `shouldBe` "3.49.0"

    it "captures the full-form scripts map (no hasInstallScript key)" $ do
        -- The core-js full manifest has scripts.postinstall but NO hasInstallScript.
        -- The projection derives install-script presence from this map.
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        vmHasInstallScript vm `shouldBe` Nothing
        Map.keys (vmScripts vm) `shouldBe` ["postinstall"]

    it "captures the deprecation notice (request)" $ do
        vm <- decodeFixture @VersionManifest "request.manifest.json"
        vmDeprecated vm
            `shouldBe` Just
                "request has been deprecated, see https://github.com/request/request/issues/3142"

    it "reads a boolean deprecated=false as not deprecated (npm's wire variant)" $ do
        vm <-
            decodeJsonOrFail @VersionManifest
                "{\"name\":\"x\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://e.test/x.tgz\"},\"deprecated\":false}"
        vmDeprecated vm `shouldBe` Nothing

    it "reads a boolean deprecated=true as deprecated with an empty message" $ do
        vm <-
            decodeJsonOrFail @VersionManifest
                "{\"name\":\"x\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://e.test/x.tgz\"},\"deprecated\":true}"
        vmDeprecated vm `shouldBe` Just ""

    it "still reads a string deprecated as the message (inline, not just the fixture)" $ do
        vm <-
            decodeJsonOrFail @VersionManifest
                "{\"name\":\"x\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://e.test/x.tgz\"},\"deprecated\":\"gone\"}"
        vmDeprecated vm `shouldBe` Just "gone"

    it "decodes the same manifest bytes to equal whole records" $ do
        -- Compare two decodes of the same bytes as whole records: a determinism
        -- check that exercises the derived Eq over every field, not one selector.
        a <- decodeFixture @VersionManifest "core-js.manifest.json"
        b <- decodeFixture @VersionManifest "core-js.manifest.json"
        a `shouldBe` b

distSpec :: Spec
distSpec = describe "Dist" $ do
    it "captures the integrity triple (tarball, shasum, integrity)" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        let d = vmDist vm
        distTarball d `shouldBe` "https://registry.npmjs.org/core-js/-/core-js-3.49.0.tgz"
        distShasum d `shouldBe` Just "aaaabbbbccccddddeeeeffff0000111122223333"
        distIntegrity d
            `shouldBe` Just "sha512-AAAABBBBCCCCDDDDEEEEFFFF00001111222233334444555566667777888899=="

    it "captures unpackedSize when present" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        distUnpackedSize (vmDist vm) `shouldBe` Just 6789012

    it "tolerates a dist with only the required tarball field" $
        decodesTo @Dist
            "{\"tarball\":\"https://example.test/x.tgz\"}"
            ( Dist
                { distTarball = "https://example.test/x.tgz"
                , distShasum = Nothing
                , distIntegrity = Nothing
                , distUnpackedSize = Nothing
                }
            )

{- | A regression guard on advisory-field leniency. The __advisory__ @unpackedSize@ field
decides no rule and no serve. A single hostile value must degrade that field alone, never
failing the 'Dist' decode. The load-bearing integrity fields (@tarball@, @integrity@) stay
strict and intact. Whole-packument survival across such a version is the projection layer's
concern, which @ProjectSpec@ pins on the live decoder.
-}
advisoryFieldLeniencySpec :: Spec
advisoryFieldLeniencySpec =
    describe "an undecodable advisory number degrades to Nothing rather than failing the Dist" $ do
        it "maps an out-of-range unpackedSize (1e400) to Nothing" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":1e400}"
                (bareDist "https://e.test/x.tgz")
        it "maps an Int-overflowing unpackedSize to Nothing" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":99999999999999999999}"
                (bareDist "https://e.test/x.tgz")
        it "maps a fractional unpackedSize to Nothing" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":1.5}"
                (bareDist "https://e.test/x.tgz")
        it "maps a wrong-typed unpackedSize (a string) to Nothing" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":\"big\"}"
                (bareDist "https://e.test/x.tgz")
        it "still reads a well-formed unpackedSize" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":4096}"
                (bareDist "https://e.test/x.tgz"){distUnpackedSize = Just 4096}

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
        it "reads name, email, and url from the full object form" $ do
            -- Exercise each selector directly, beyond the structural equality above.
            p <-
                decodeJsonOrFail @Person
                    "{\"name\":\"Sindre\",\"email\":\"s@example.com\",\"url\":\"https://sindresorhus.com\"}"
            personName p `shouldBe` "Sindre"
            personEmail p `shouldBe` Just "s@example.com"
            personUrl p `shouldBe` Just "https://sindresorhus.com"

    -- A string-or-object scalar must reject any other JSON kind rather than mis-parsing it.
    -- Asserting the full message, not `isLeft`, pins the error text that names both the accepted
    -- shapes and the JSON kind found. Ecluse.Json.LenientSpec pins every kind's rendering.
    describe "rejecting the wrong JSON kind" $ do
        it "rejects a number for License, naming the number kind" $
            (eitherDecode "42" :: Either String License)
                `shouldBe` Left "Error in $: expected License (object or string), but encountered a number"
        it "rejects an array for Person, naming the array kind" $
            (eitherDecode "[\"a\",\"b\"]" :: Either String Person)
                `shouldBe` Left "Error in $: expected Person (object or string), but encountered an array"

{- | The wire types appear inside registry arrays and objects, so each must also decode as a
list element. These cases drive every decoder's list path, which HPC tracks as a distinct
@parseJSONList@ box.
-}
jsonListSpec :: Spec
jsonListSpec = describe "decoding JSON arrays of the wire types" $ do
    it "decodes a list of licenses, mixing string and object forms" $
        decodesTo @[License]
            "[\"MIT\", {\"type\":\"BSD-3-Clause\",\"url\":\"https://x/l\"}]"
            [LicenseSpdx "MIT", LicenseObject "BSD-3-Clause" (Just "https://x/l")]

    it "decodes a list of people, mixing packed-string and object forms" $
        decodesTo @[Person]
            "[\"Mikeal Rogers\", {\"name\":\"sindre\",\"email\":\"s@example.com\"}]"
            [ Person "Mikeal Rogers" Nothing Nothing
            , Person "sindre" (Just "s@example.com") Nothing
            ]

    it "decodes a list of dist objects" $
        decodesTo @[Dist]
            "[{\"tarball\":\"https://x/a.tgz\"},{\"tarball\":\"https://x/b.tgz\",\"shasum\":\"abc\"}]"
            [ Dist "https://x/a.tgz" Nothing Nothing Nothing
            , Dist "https://x/b.tgz" (Just "abc") Nothing Nothing
            ]

    it "decodes a list of version manifests" $ do
        vms <-
            decodeJsonOrFail @[VersionManifest]
                "[{\"name\":\"a\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://x/a.tgz\"}}\
                \,{\"name\":\"b\",\"version\":\"2.0.0\",\"dist\":{\"tarball\":\"https://x/b.tgz\"}}]"
        map vmName vms `shouldBe` ["a", "b"]
        map vmVersion vms `shouldBe` ["1.0.0", "2.0.0"]

{- | The wire decoders eat __untrusted__ upstream JSON, so each must be __total__: no input
may make one bottom, only a typed 'Success'\/'Error' or 'Right'\/'Left'. The companion
projection-layer properties live in "Ecluse.Registry.Npm.ProjectSpec".
-}
totalitySpec :: Spec
totalitySpec = describe "decoder totality (arbitrary input never bottoms)" $ do
    describe "every wire decoder is total over an arbitrary Value" $ do
        it "Person" $ hedgehog (valueDecodeIsTotal @Person)
        it "License" $ hedgehog (valueDecodeIsTotal @License)
        it "Dist" $ hedgehog (valueDecodeIsTotal @Dist)
        it "VersionManifest" $ hedgehog (valueDecodeIsTotal @VersionManifest)

    -- The bytes-level entry ('eitherDecodeStrict') must be total over arbitrary
    -- bytes too: garbage decodes to a typed 'Left', never a crash.
    describe "every bytes-level decode is total over arbitrary bytes" $ do
        it "Person" $ hedgehog (bytesDecodeIsTotal @Person)
        it "Dist" $ hedgehog (bytesDecodeIsTotal @Dist)
        it "VersionManifest" $ hedgehog (bytesDecodeIsTotal @VersionManifest)

    describe "the Value generator reaches both arms of a permissive decoder" $ do
        it "Person decodes both ways" $ hedgehog (valueDecodeCoversBothArms @Person)
        it "License decodes both ways" $ hedgehog (valueDecodeCoversBothArms @License)

    -- A generated success must be a genuine round-trip, not a coincidental accept.
    it "a String that decodes as a Person is captured verbatim in personName" $
        hedgehog $ do
            s <- forAll (Gen.text (Range.linear 0 12) Gen.unicode)
            case fromJSON (String s) :: Result Person of
                Success p -> personName p H.=== s
                other -> annotateShow other >> H.failure

{- | Assert a 'FromJSON' decoder is __total__ over an arbitrary 'Value'. Forcing the 'Show'
rendering walks the whole decoded structure, so a bottom past the outermost constructor
surfaces as a caught failure rather than a pass.
-}
valueDecodeIsTotal :: forall a. (FromJSON a, Show a) => PropertyT IO ()
valueDecodeIsTotal = do
    v <- forAll (genValue wireKeys)
    annotateShow v
    _ <- H.eval (resultRendering (fromJSON v :: Result a))
    H.success

{- | Assert a bytes-level decode ('eitherDecodeStrict') is __total__ over arbitrary bytes:
random, mostly non-JSON bytes must yield a typed 'Left', never a crash.
-}
bytesDecodeIsTotal :: forall a. (FromJSON a, Show a) => PropertyT IO ()
bytesDecodeIsTotal = do
    bytes <- forAll (Gen.bytes (Range.linear 0 64))
    _ <- H.eval (length (show (eitherDecodeStrict bytes :: Either String a) :: String))
    H.success

{- | Confirm the 'Value' generator reaches __both__ the success and failure arms
of a permissive decoder, so 'valueDecodeIsTotal' is not vacuously all-failures.
'H.cover' fails the property when either arm is under-represented.
-}
valueDecodeCoversBothArms :: forall a. (FromJSON a, Show a) => PropertyT IO ()
valueDecodeCoversBothArms = do
    v <- forAll (genValue wireKeys)
    let decoded = fromJSON v :: Result a
    annotateShow v
    _ <- H.eval (resultRendering decoded)
    H.cover 1 "decodes (Success)" (isSuccess decoded)
    H.cover 1 "rejects (Error)" (not (isSuccess decoded))

-- | Force a decoded 'Result' through its 'Show' rendering, returning its length.
resultRendering :: (Show a) => Result a -> Int
resultRendering = \case
    Success a -> length (show a :: String)
    Error e -> length e

-- | Whether a decode 'Result' is the 'Success' arm.
isSuccess :: Result a -> Bool
isSuccess = \case
    Success{} -> True
    Error{} -> False

{- | The object-key pool the generated documents draw from. Without the bias toward the real
wire field names, almost every object would miss @.: \"name\"@ and the success arm would go unsampled.
-}
wireKeys :: [Text]
wireKeys =
    [ "name"
    , "version"
    , "modified"
    , "dist"
    , "dist-tags"
    , "versions"
    , "time"
    , "tarball"
    , "shasum"
    , "integrity"
    , "unpackedSize"
    , "scripts"
    , "license"
    , "type"
    , "url"
    , "email"
    , "maintainers"
    , "dependencies"
    , "deprecated"
    , "hasInstallScript"
    ]

{- | Decode a committed fixture by file name under @core\/test\/unit\/fixtures\/npm\/@, a path
relative to the package root that Cabal runs tests from.
-}
decodeFixture :: forall a. (FromJSON a) => FilePath -> IO a
decodeFixture name = do
    bytes <- readFileBS ("core/test/unit/fixtures/npm/" <> name)
    case eitherDecodeStrict bytes of
        Right a -> pure a
        Left e -> fail ("failed to decode " <> name <> ": " <> e)

-- | Assert that a JSON literal decodes to an expected value.
decodesTo :: forall a. (FromJSON a, Eq a, Show a) => LByteString -> a -> Expectation
decodesTo json expected = eitherDecode json `shouldBe` Right expected

{- | A 'Dist' carrying only its required tarball, its advisory field at the absent
default. This is the expected shape when a poisoned advisory field degrades.
-}
bareDist :: Text -> Dist
bareDist tarball =
    Dist
        { distTarball = tarball
        , distShasum = Nothing
        , distIntegrity = Nothing
        , distUnpackedSize = Nothing
        }
