-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- The totality properties below are polymorphic in the decoded type @a@, which
-- appears only under a type application at each call site (e.g.
-- @valueDecodeIsTotal \@Person@); that is exactly what AllowAmbiguousTypes is for.
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ecluse.Registry.Npm.WireSpec (spec) where

import Data.Aeson (
    FromJSON,
    Result (Error, Success),
    Value (Array, Bool, Null, Number, Object, String),
    eitherDecode,
    eitherDecodeStrict,
    fromJSON,
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, scientific)
import Data.Vector qualified as V
import Hedgehog (PropertyT, annotateShow, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Registry.Npm.Wire

{- | Decoding tests for the npm wire types. Every fixture under
@core\/test\/unit\/fixtures\/npm\/@ is a body derived from the real captures documented
in @docs\/research\/reverse-engineering\/npm.md@ (§6 manifest, §7 @dist@, §3 errors).
The suite is pure and offline -- it never touches the network; protocol drift is
surfaced separately by the non-gating smoke suite.

The cases pin down the two things the decoders promise: __faithful__ capture of
the rule-decisive fields (@hasInstallScript@, @deprecated@, the @dist@ integrity
triple, the @scripts@ map) and __lenient__ input handling (string-or-object
@license@\/@person@\/@bugs@\/@repository@, the bare-string 404, and ignored unknown
keys).
-}
spec :: Spec
spec = do
    versionManifestSpec
    distSpec
    advisoryFieldLeniencySpec
    lenientScalarSpec
    errorResponseSpec
    jsonListSpec
    totalitySpec

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

    it "reads a boolean deprecated=false as not deprecated (npm's wire variant)" $ do
        vm <-
            decodeOrFail @VersionManifest
                "{\"name\":\"x\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://e.test/x.tgz\"},\"deprecated\":false}"
        vmDeprecated vm `shouldBe` Nothing

    it "reads a boolean deprecated=true as deprecated with an empty message" $ do
        vm <-
            decodeOrFail @VersionManifest
                "{\"name\":\"x\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://e.test/x.tgz\"},\"deprecated\":true}"
        vmDeprecated vm `shouldBe` Just ""

    it "still reads a string deprecated as the message (inline, not just the fixture)" $ do
        vm <-
            decodeOrFail @VersionManifest
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

    it "captures fileCount, unpackedSize, and signatures when present" $ do
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        let d = vmDist vm
        distFileCount d `shouldBe` Just 1455
        distUnpackedSize d `shouldBe` Just 6789012
        -- Pin the whole signature object so both fields (sig and keyid) are
        -- asserted, not just the keyid.
        distSignatures d
            `shouldBe` [Signature{sigSig = "MEQCIH", sigKeyid = "SHA256:jl3bwswu80"}]

    it "reads the sig and keyid selectors off a decoded Signature" $ do
        -- Exercise the named selectors directly (the structural shouldBe above
        -- goes through derived Eq, not the selector functions).
        vm <- decodeFixture @VersionManifest "core-js.manifest.json"
        case distSignatures (vmDist vm) of
            [sig] -> do
                sigSig sig `shouldBe` "MEQCIH"
                sigKeyid sig `shouldBe` "SHA256:jl3bwswu80"
            sigs -> fail ("expected exactly one signature, got " <> show (length sigs))

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

{- | A regression guard for the advisory-field denial defect: the __advisory__
@dist@ sub-fields (@unpackedSize@, @fileCount@, @signatures@) decide no rule and
no serve, so a single hostile value in one must degrade that field alone, never
failing the 'Dist' decode. The load-bearing integrity fields (@tarball@,
@integrity@) stay strict and intact. Whole-packument survival across such a version
is the projection layer's concern, pinned on the live decoder by @ProjectSpec@.
-}
advisoryFieldLeniencySpec :: Spec
advisoryFieldLeniencySpec = describe "advisory dist-field leniency" $ do
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
        it "maps a fractional fileCount to Nothing" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"fileCount\":1.5}"
                (bareDist "https://e.test/x.tgz")
        it "maps a wrong-typed unpackedSize (a string) to Nothing" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":\"big\"}"
                (bareDist "https://e.test/x.tgz")
        it "still reads a well-formed unpackedSize" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"unpackedSize\":4096}"
                (bareDist "https://e.test/x.tgz"){distUnpackedSize = Just 4096}

    describe "signatures degrade element-wise rather than failing the array" $ do
        it "skips a signature element missing its keyid" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"signatures\":[{\"sig\":\"x\"}]}"
                (bareDist "https://e.test/x.tgz")
        it "treats a non-array signatures value (5) as empty" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"signatures\":5}"
                (bareDist "https://e.test/x.tgz")
        it "keeps the well-formed elements and drops only the junk ones" $
            decodesTo @Dist
                "{\"tarball\":\"https://e.test/x.tgz\",\"signatures\":\
                \[{\"sig\":\"a\",\"keyid\":\"k1\"},{\"sig\":\"x\"},{\"sig\":\"b\",\"keyid\":\"k2\"}]}"
                (bareDist "https://e.test/x.tgz")
                    { distSignatures = [Signature "a" "k1", Signature "b" "k2"]
                    }

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
            -- Exercise each selector, not just the structural equality above.
            p <-
                decodeOrFail @Person
                    "{\"name\":\"Sindre\",\"email\":\"s@example.com\",\"url\":\"https://sindresorhus.com\"}"
            personName p `shouldBe` "Sindre"
            personEmail p `shouldBe` Just "s@example.com"
            personUrl p `shouldBe` Just "https://sindresorhus.com"
    describe "Repository" $ do
        it "accepts a bare shorthand string" $
            decodesTo @Repository
                "\"github:user/repo\""
                (Repository Nothing "github:user/repo")
        it "accepts the object form" $
            decodesTo @Repository
                "{\"type\":\"git\",\"url\":\"git+https://x/r.git\"}"
                (Repository (Just "git") "git+https://x/r.git")
        it "reads type and url selectors from the object form" $ do
            r <- decodeOrFail @Repository "{\"type\":\"git\",\"url\":\"git+https://x/r.git\"}"
            repoType r `shouldBe` Just "git"
            repoUrl r `shouldBe` "git+https://x/r.git"

    describe "Bugs" $ do
        it "accepts a bare URL string" $
            decodesTo @Bugs
                "\"https://github.com/x/issues\""
                (Bugs (Just "https://github.com/x/issues") Nothing)
        it "accepts the object form" $
            decodesTo @Bugs
                "{\"url\":\"https://x/issues\",\"email\":\"b@x\"}"
                (Bugs (Just "https://x/issues") (Just "b@x"))
        it "reads url and email selectors from the object form" $ do
            b <- decodeOrFail @Bugs "{\"url\":\"https://x/issues\",\"email\":\"b@x\"}"
            bugsUrl b `shouldBe` Just "https://x/issues"
            bugsEmail b `shouldBe` Just "b@x"

    -- A string-or-object scalar must reject any other JSON kind rather than
    -- silently mis-parsing it. We spread the four rejected kinds (number, array,
    -- boolean, null) across the lenient decoders so each is proven to refuse a
    -- shape it was never meant to accept. Asserting the full message (not just
    -- `isLeft`) pins the descriptive error, naming both the accepted shapes and
    -- the JSON kind actually found.
    describe "rejecting the wrong JSON kind" $ do
        it "rejects a number for License, naming the number kind" $
            (eitherDecode "42" :: Either String License)
                `shouldBe` Left "Error in $: expected License (object or string), but encountered a number"
        it "rejects an array for Person, naming the array kind" $
            (eitherDecode "[\"a\",\"b\"]" :: Either String Person)
                `shouldBe` Left "Error in $: expected Person (object or string), but encountered an array"
        it "rejects a boolean for Repository, naming the boolean kind" $
            (eitherDecode "true" :: Either String Repository)
                `shouldBe` Left "Error in $: expected Repository (object or string), but encountered a boolean"
        it "rejects null for Bugs, naming the null kind" $
            (eitherDecode "null" :: Either String Bugs)
                `shouldBe` Left "Error in $: expected Bugs (object or string), but encountered null"
        it "rejects an array for an error body, naming the array kind" $
            (eitherDecode "[\"oops\"]" :: Either String ErrorResponse)
                `shouldBe` Left "Error in $: expected ErrorResponse (object or string), but encountered an array"

errorResponseSpec :: Spec
errorResponseSpec = describe "ErrorResponse" $ do
    it "tolerates the bare-string per-version 404 body" $ do
        -- npm's per-version 404 is a bare JSON string, not an object (§3).
        err <- decodeFixture @ErrorResponse "error-version-not-found.json"
        err `shouldBe` ErrorString "version not found: ^3.0.0"

    it "decodes the {error} object form (unknown package)" $ do
        err <- decodeFixture @ErrorResponse "error-not-found.json"
        err `shouldBe` ErrorObject ErrorBody{errMessage = Nothing, errError = Just "Not found"}

    it "decodes the {message} object form" $ do
        err <- decodeFixture @ErrorResponse "error-method-not-allowed.json"
        err `shouldBe` ErrorObject ErrorBody{errMessage = Just "GET is not allowed", errError = Nothing}

    it "decodes an object carrying both message and error fields" $ do
        err <- decodeFixture @ErrorResponse "error-both-fields.json"
        err
            `shouldBe` ErrorObject
                ErrorBody{errMessage = Just "you must be logged in", errError = Just "Unauthorized"}

{- | The wire types appear inside JSON arrays on the registry (maintainer and
signature lists, and @versions@\/@dist-tags@ are objects of them), so each must
decode as an element of a homogeneous list too. These cases decode a JSON array
of each type and assert the decoded elements, exercising the list path of every
decoder (HPC tracks it as a distinct @parseJSONList@ box) with real values.
-}
jsonListSpec :: Spec
jsonListSpec = describe "decoding JSON arrays of the wire types" $ do
    it "decodes a list of licenses, mixing string and object forms" $
        decodesTo @[License]
            "[\"MIT\", {\"type\":\"BSD-3-Clause\",\"url\":\"https://x/l\"}]"
            [LicenseSpdx "MIT", LicenseObject "BSD-3-Clause" (Just "https://x/l")]

    it "decodes a list of bug trackers, mixing string and object forms" $
        decodesTo @[Bugs]
            "[\"https://a/issues\", {\"url\":\"https://b/issues\",\"email\":\"b@x\"}]"
            [Bugs (Just "https://a/issues") Nothing, Bugs (Just "https://b/issues") (Just "b@x")]

    it "decodes a list of repositories, mixing shorthand and object forms" $
        decodesTo @[Repository]
            "[\"github:u/r\", {\"type\":\"git\",\"url\":\"git+https://x/r.git\"}]"
            [Repository Nothing "github:u/r", Repository (Just "git") "git+https://x/r.git"]

    it "decodes a list of dist objects" $
        decodesTo @[Dist]
            "[{\"tarball\":\"https://x/a.tgz\"},{\"tarball\":\"https://x/b.tgz\",\"shasum\":\"abc\"}]"
            [ Dist "https://x/a.tgz" Nothing Nothing Nothing Nothing []
            , Dist "https://x/b.tgz" (Just "abc") Nothing Nothing Nothing []
            ]

    it "decodes a list of version manifests" $ do
        vms <-
            decodeOrFail @[VersionManifest]
                "[{\"name\":\"a\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://x/a.tgz\"}}\
                \,{\"name\":\"b\",\"version\":\"2.0.0\",\"dist\":{\"tarball\":\"https://x/b.tgz\"}}]"
        map vmName vms `shouldBe` ["a", "b"]
        map vmVersion vms `shouldBe` ["1.0.0", "2.0.0"]

    it "decodes a list of error responses, mixing string and object forms" $ do
        errs <-
            decodeOrFail @[ErrorResponse]
                "[\"version not found\", {\"error\":\"Not found\"}, {\"message\":\"nope\"}]"
        errs
            `shouldBe` [ ErrorString "version not found"
                       , ErrorObject ErrorBody{errMessage = Nothing, errError = Just "Not found"}
                       , ErrorObject ErrorBody{errMessage = Just "nope", errError = Nothing}
                       ]

{- | The wire decoders eat __untrusted__ upstream JSON, so every one must be
__total__: an arbitrary input may never make a decoder bottom (throw, or hit a
partial function); it must always return a typed 'Success'\/'Error' (for a
'Value') or a 'Right'\/'Left' (for raw bytes). These generative properties feed
each 'FromJSON' instance a bounded-but-arbitrary 'Value' and a run of arbitrary
bytes and assert the result is fully evaluable without an exception -- the
totality half of /parse, don't validate/ that the fixture suite above only spot-
checks. (The companion projection-layer properties live in
"Ecluse.Registry.Npm.ProjectSpec".)
-}
totalitySpec :: Spec
totalitySpec = describe "decoder totality (arbitrary input never bottoms)" $ do
    -- Each decoder must be total over an arbitrary 'Value': the result is a
    -- typed Success or a typed Error, never ⊥. We force the whole decoded
    -- structure (via its 'Show' rendering) so a partial function anywhere in it
    -- would surface as a caught exception rather than slipping past in a thunk.
    describe "every wire decoder is total over an arbitrary Value" $ do
        it "Person" $ hedgehog (valueDecodeIsTotal @Person)
        it "Repository" $ hedgehog (valueDecodeIsTotal @Repository)
        it "Bugs" $ hedgehog (valueDecodeIsTotal @Bugs)
        it "License" $ hedgehog (valueDecodeIsTotal @License)
        it "Signature" $ hedgehog (valueDecodeIsTotal @Signature)
        it "Dist" $ hedgehog (valueDecodeIsTotal @Dist)
        it "VersionManifest" $ hedgehog (valueDecodeIsTotal @VersionManifest)
        it "ErrorResponse" $ hedgehog (valueDecodeIsTotal @ErrorResponse)

    -- The bytes-level entry ('eitherDecodeStrict') must be total over arbitrary
    -- bytes too: garbage decodes to a typed 'Left', never a crash.
    describe "every bytes-level decode is total over arbitrary bytes" $ do
        it "Person" $ hedgehog (bytesDecodeIsTotal @Person)
        it "Dist" $ hedgehog (bytesDecodeIsTotal @Dist)
        it "VersionManifest" $ hedgehog (bytesDecodeIsTotal @VersionManifest)
        it "ErrorResponse" $ hedgehog (bytesDecodeIsTotal @ErrorResponse)

    -- For the permissive string-or-object scalars the generator must reach BOTH
    -- the success and the failure arm, so the totality check above is not
    -- vacuously all-failures. 'H.cover' fails the property if either arm is
    -- under-sampled.
    describe "the Value generator reaches both arms of a permissive decoder" $ do
        it "Person decodes both ways" $ hedgehog (valueDecodeCoversBothArms @Person)
        it "Repository decodes both ways" $ hedgehog (valueDecodeCoversBothArms @Repository)
        it "Bugs decodes both ways" $ hedgehog (valueDecodeCoversBothArms @Bugs)
        it "License decodes both ways" $ hedgehog (valueDecodeCoversBothArms @License)
        it "ErrorResponse decodes both ways" $ hedgehog (valueDecodeCoversBothArms @ErrorResponse)

    -- A raw-fidelity invariant the fixture suite implies: a successfully decoded
    -- 'ErrorString' carries the generated string __verbatim__. This proves a
    -- generated success is a genuine round-trip, not a coincidental accept.
    it "a String that decodes as an ErrorResponse is captured verbatim" $
        hedgehog $ do
            s <- forAll (Gen.text (Range.linear 0 12) Gen.unicode)
            case fromJSON (String s) :: Result ErrorResponse of
                Success (ErrorString captured) -> captured H.=== s
                other -> annotateShow other >> H.failure

{- | Assert a 'FromJSON' decoder is __total__ over an arbitrary 'Value': feed it a
bounded-but-arbitrary value and fully evaluate the typed 'Result', so a bottom
anywhere in the decoded structure surfaces as a caught exception ('H.eval' runs
the forcing in pure code and turns any thrown bottom into a test failure) rather
than a pass. Forcing the 'Show' rendering walks the whole structure, not just its
outermost constructor.
-}
valueDecodeIsTotal :: forall a. (FromJSON a, Show a) => PropertyT IO ()
valueDecodeIsTotal = do
    v <- forAll genValue
    annotateShow v
    _ <- H.eval (resultRendering (fromJSON v :: Result a))
    H.success

{- | Assert a bytes-level decode ('eitherDecodeStrict') is __total__ over
arbitrary bytes: random (mostly non-JSON) bytes must yield a typed 'Left', never
a crash. As above, the whole 'Either' is forced through its rendering.
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
    v <- forAll genValue
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

{- | A recursive, depth- and breadth-__bounded__ arbitrary 'Aeson.Value': the
five scalar kinds (null\/bool\/number\/string) plus small arrays and objects of
recursively-generated values. 'Gen.recursive' shrinks toward the scalar
(non-recursive) cases and the small ranges keep it terminating, so it covers the
JSON shapes a registry might send (and many it never would) without diverging.
The object keys lean on a small alphabet that includes the real wire field names,
so generated objects routinely land on a decoder's expected keys.
-}
genValue :: H.Gen Value
genValue =
    Gen.recursive
        Gen.choice
        -- non-recursive (leaf) generators -- also the shrink targets
        [ pure Null
        , Bool <$> Gen.bool
        , Number <$> genNumber
        , String <$> genJsonText
        ]
        -- recursive generators -- small fan-out so the tree stays bounded
        [ Array . V.fromList <$> Gen.list (Range.linear 0 4) genValue
        , Object . KeyMap.fromList
            <$> Gen.list (Range.linear 0 4) ((,) <$> genKey <*> genValue)
        ]

{- | A small arbitrary integer to seed a JSON number (kept in a modest range so
'Show' is cheap; 'fromInteger' lifts it into aeson's 'Number' 'Scientific').
-}
genInteger :: H.Gen Integer
genInteger = Gen.integral (Range.linearFrom 0 (-100000) 100000)

{- | A small arbitrary JSON number. Most draws are modest integers (cheap to
'Show'), but a deliberate minority are hostile to a strict 'Int' decode --
fractional or far outside 'Int' range (built from a bounded coefficient and a
wide base-10 exponent, so the magnitude is astronomical yet the value stays cheap
to render). This reaches the fractional\/huge\/overflowing shapes a plain integer
generator never produces, so the totality fuzz exercises the lenient numeric
@dist@ decoders' graceful degradation, not merely the absence of a crash.
-}
genNumber :: H.Gen Scientific
genNumber =
    Gen.frequency
        [ (3, fromInteger <$> genInteger)
        , (1, scientific <$> genInteger <*> Gen.int (Range.linearFrom 0 (-20) 400))
        ]

-- | A short arbitrary JSON string value (unicode, to probe text handling).
genJsonText :: H.Gen Text
genJsonText = Gen.text (Range.linear 0 8) Gen.unicode

{- | An object key drawn from a pool biased toward the real wire field names
(@name@, @version@, @dist@, @tarball@, …) so generated objects frequently satisfy
a decoder's required\/optional keys -- otherwise almost every object would miss
@.: \"name\"@ and the success arm would go unsampled.
-}
genKey :: H.Gen Key.Key
genKey = Key.fromText <$> Gen.choice [Gen.element wireKeys, genJsonText]
  where
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
        , "signatures"
        , "sig"
        , "keyid"
        , "scripts"
        , "license"
        , "type"
        , "url"
        , "email"
        , "message"
        , "error"
        , "maintainers"
        , "dependencies"
        , "deprecated"
        , "hasInstallScript"
        ]

{- | Decode a committed fixture by file name (under @core\/test\/unit\/fixtures\/npm\/@,
a path relative to the package root that Cabal runs tests from), failing the
example with the aeson error on a decode failure.
-}
decodeFixture :: forall a. (FromJSON a) => FilePath -> IO a
decodeFixture name = do
    bytes <- readFileBS ("core/test/unit/fixtures/npm/" <> name)
    case eitherDecodeStrict bytes of
        Right a -> pure a
        Left e -> fail ("failed to decode " <> name <> ": " <> e)

{- | Assert that a JSON literal decodes to an expected value. Keeps the inline
lenient-form cases to a single readable line.
-}
decodesTo :: forall a. (FromJSON a, Eq a, Show a) => LByteString -> a -> Expectation
decodesTo json expected = eitherDecode json `shouldBe` Right expected

{- | A 'Dist' carrying only its required tarball, every advisory field at its
absent\/empty default. The expected shape when a poisoned advisory field has
degraded; record-update one selector for the cases that keep a good value.
-}
bareDist :: Text -> Dist
bareDist tarball =
    Dist
        { distTarball = tarball
        , distShasum = Nothing
        , distIntegrity = Nothing
        , distFileCount = Nothing
        , distUnpackedSize = Nothing
        , distSignatures = []
        }

{- | Decode a JSON literal, failing the example with the aeson error rather than
returning an 'Either', so an example can go on to read selectors off the value.
-}
decodeOrFail :: forall a. (FromJSON a) => LByteString -> IO a
decodeOrFail json = case eitherDecode json of
    Right a -> pure a
    Left e -> fail ("expected a successful decode, got: " <> e)
