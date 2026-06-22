module Ecluse.Registry.Npm.ProjectSpec (spec) where

import Data.Aeson (
    Value (Array, Bool, Null, Number, Object, String),
    encode,
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Hedgehog (PropertyT, annotateShow, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotSatisfy, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (
    Artifact (artFilename, artHashes, artInterpreter, artKind, artProvenance, artSize, artUrl, artYanked),
    ArtifactKind (Tarball),
    Availability (Available, Deprecated),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    DepKind (Dev, Optional, Peer, Runtime),
    Dependency (depConstraint, depKind, depMarker, depName),
    Hash (Hash),
    HashAlg (SHA1, SRI),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Person (Person),
    Trust (TrustUnknown),
    pkgCanonical,
    pkgNamespace,
    renderScope,
 )
import Ecluse.Registry (ParseError, RegistryResponse (RegistryResponse))
import Ecluse.Registry.Npm.Project (
    parsePackageInfo,
    parseVersionDetails,
    parseVersionList,
 )
import Ecluse.Version (Version, mkVersion, renderVersion, unVersion)

{- | Projection tests for the npm adapter. They assert the __domain__ values a
fetched packument projects into — the second half of the boundary that
"Ecluse.Registry.Npm.WireSpec" tests the decode half of. The fixtures under
@test\/unit\/fixtures\/npm\/@ are the same captures the wire suite uses; a few
edge cases (a full-form install-script derivation, a missing @time@ entry, a
malformed body) are inline JSON literals.

The cases pin down the signal-mapping table: install-script presence (flagged,
derived, and absent) onto 'CodeExecSignal'; @deprecated@ onto 'Availability';
the @dist@ integrity pair onto __both__ a 'SHA1' and an 'SRI' 'Hash'; @_npmUser@
onto 'pkgPublisher'; @time[version]@ onto 'pkgPublishedAt'; and scoped names onto
'Ecluse.Package.Scope'.
-}
spec :: Spec
spec = do
    packageInfoSpec
    signalMappingSpec
    integritySpec
    versionDetailsSpec
    versionListSpec
    failureSpec
    totalitySpec

-- ── packument-level projection ───────────────────────────────────────────────

packageInfoSpec :: Spec
packageInfoSpec = describe "parsePackageInfo" $ do
    it "projects the package name, versions, and dist-tags (is-odd)" $ do
        info <- projectFixture "is-odd.full.json"
        renderName (infoName info) `shouldBe` "is-odd"
        Map.keys (infoVersions info) `shouldBe` ["3.0.1"]
        fmap renderVersion (Map.lookup "latest" (infoDistTags info)) `shouldBe` Just "3.0.1"

    it "keys the publish-time map by raw version, dropping created/modified" $ do
        -- The packument `time` map also carries `created`/`modified`; only the
        -- per-version entries are publish times, so those bookkeeping keys must
        -- not leak into the projected map.
        info <- projectFixture "is-odd.full.json"
        published <- readUTC "2018-05-31T20:04:53.306Z"
        infoPublishedAt info `shouldBe` Map.singleton "3.0.1" published

    it "splits a scoped name into scope and bare name (@babel/code-frame)" $ do
        info <- projectFixture "babel-code-frame.abbreviated.json"
        fmap renderScope (pkgNamespace (infoName info)) `shouldBe` Just "@babel"
        pkgCanonical (infoName info) `shouldBe` "@babel/code-frame"

    it "leaves the publish-time map empty for an abbreviated document (no time)" $ do
        -- The abbreviated form omits the `time` map entirely, so every version's
        -- publish time is unknown.
        info <- projectFixture "core-js.abbreviated.json"
        infoPublishedAt info `shouldBe` Map.empty

-- ── the signal-mapping table ─────────────────────────────────────────────────

signalMappingSpec :: Spec
signalMappingSpec = describe "signal mapping" $ do
    describe "install-script presence → CodeExecSignal" $ do
        it "maps an abbreviated hasInstallScript:true to RunsCodeOnInstall (core-js)" $ do
            d <- projectVersion "core-js.abbreviated.json" (mkVersion Npm "3.49.0")
            pkgInstallCode d `shouldSatisfy` runsCode

        it "derives RunsCodeOnInstall from a full-form postinstall script" $ do
            -- The full manifest has NO hasInstallScript key; presence is derived
            -- from `scripts` having one of preinstall/install/postinstall.
            d <- projectVersionOf fullPostinstallPackument (mkVersion Npm "1.0.0")
            pkgInstallCode d `shouldSatisfy` runsCode

        it "maps no install-script signal to NoCodeOnInstall (is-odd has only `test`)" $ do
            -- is-odd's only script is `test`, which does not run on install.
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            pkgInstallCode d `shouldBe` NoCodeOnInstall
            pkgInstallCode d `shouldNotSatisfy` runsCode

        it "maps an explicit hasInstallScript:false to NoCodeOnInstall" $ do
            -- The abbreviated flag, when present and false, is a determination
            -- that installation runs no code.
            d <- projectVersionOf noInstallScriptPackument (mkVersion Npm "1.0.0")
            pkgInstallCode d `shouldBe` NoCodeOnInstall

    describe "deprecated → Availability" $ do
        it "maps a deprecation notice to Deprecated carrying the message (request)" $ do
            d <- projectVersion "request.full.json" (mkVersion Npm "2.88.2")
            pkgAvailability d
                `shouldBe` Deprecated
                    "request has been deprecated, see https://github.com/request/request/issues/3142"

        it "maps the absence of a notice to Available (is-odd)" $ do
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            pkgAvailability d `shouldBe` Available

    describe "_npmUser → pkgPublisher" $ do
        it "projects the publisher from the version object (is-odd)" $ do
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            pkgPublisher d `shouldBe` Just (Person "jonschlinkert" (Just "github@sellside.com") Nothing)

        it "leaves the publisher absent when _npmUser is missing (request)" $ do
            d <- projectVersion "request.full.json" (mkVersion Npm "2.88.2")
            pkgPublisher d `shouldBe` Nothing

    describe "maintainers → pkgMaintainers" $ do
        it "projects the version's maintainers, distinct from the publisher" $ do
            d <- projectVersionOf maintainersPackument (mkVersion Npm "1.0.0")
            pkgMaintainers d `shouldBe` [Person "carol" (Just "carol@example.test") Nothing]

        it "leaves maintainers empty when the version declares none (is-odd)" $ do
            -- The is-odd packument carries maintainers at the package level only;
            -- its version manifest has none.
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            pkgMaintainers d `shouldBe` []

    describe "time[version] → pkgPublishedAt" $ do
        it "fills the publish time from the packument time map (is-odd)" $ do
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            published <- readUTC "2018-05-31T20:04:53.306Z"
            pkgPublishedAt d `shouldBe` Just published

        it "leaves the publish time Nothing when no time entry exists (abbreviated)" $ do
            d <- projectVersion "core-js.abbreviated.json" (mkVersion Npm "3.49.0")
            pkgPublishedAt d `shouldBe` Nothing

    describe "unfetched trust → TrustUnknown" $
        it "leaves trust unknown (pure projection performs no signature check)" $ do
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            pkgTrust d `shouldBe` TrustUnknown

    describe "license → pkgLicenses" $ do
        it "projects a bare SPDX string license (is-odd → MIT)" $ do
            d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
            pkgLicenses d `shouldBe` ["MIT"]

        it "projects the legacy object license to its name (request → Apache-2.0)" $ do
            d <- projectVersion "request.full.json" (mkVersion Npm "2.88.2")
            pkgLicenses d `shouldBe` ["Apache-2.0"]

    describe "dependencies → pkgDependencies (kept raw, tagged by kind)" $ do
        it "keeps the constraint raw and carries no marker (npm has no PEP 508)" $ do
            d <- projectVersionOf allDepKindsPackument (mkVersion Npm "1.0.0")
            let runtimeDep = find ((== "runtime-dep") . depName) (pkgDependencies d)
            fmap (\x -> (depConstraint x, depMarker x)) runtimeDep `shouldBe` Just ("^1.0.0", Nothing)

        it "tags runtime, dev, peer, and optional dependencies across all four maps" $ do
            d <- projectVersionOf allDepKindsPackument (mkVersion Npm "1.0.0")
            let kindOf n = depKind <$> find ((== n) . depName) (pkgDependencies d)
            kindOf "runtime-dep" `shouldBe` Just Runtime
            kindOf "dev-dep" `shouldBe` Just Dev
            kindOf "peer-dep" `shouldBe` Just Peer
            kindOf "optional-dep" `shouldBe` Just Optional

-- ── integrity: BOTH digests survive ──────────────────────────────────────────

integritySpec :: Spec
integritySpec = describe "dist → Artifact integrity" $ do
    it "carries BOTH the SHA-1 shasum and the SRI integrity (is-odd)" $ do
        -- Neither digest may be dropped: a cross-upstream merge compares both to
        -- detect a same-version integrity divergence.
        d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
        artHashes (soleArtifact d)
            `shouldBe` [ Hash SRI "sha512-CQpnWPrDwmP1+SMHXZhtLtJv90yiyVfluGsX5iNCVkrhQtU3TQHsUWPG9wkdk9Lgd5yNpAg9jQEo90CBaXgWMA=="
                       , Hash SHA1 "65101baf3727d728b66fa62f50cda7f2d3989601"
                       ]

    it "projects exactly one tarball artifact with the dist URL (is-odd)" $ do
        d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
        length (pkgArtifacts d) `shouldBe` 1
        artKind (soleArtifact d) `shouldBe` Tarball
        artUrl (soleArtifact d) `shouldBe` "https://registry.npmjs.org/is-odd/-/is-odd-3.0.1.tgz"

    it "derives the artifact filename from the URL's last path segment (is-odd)" $ do
        d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
        artFilename (soleArtifact d) `shouldBe` "is-odd-3.0.1.tgz"

    it "leaves npm-irrelevant artifact fields at their explicit defaults (is-odd)" $ do
        -- npm has no per-file yank, interpreter constraint, or separate
        -- provenance URL on the artifact; these stay at their unknown/false
        -- defaults rather than being fabricated.
        d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
        let art = soleArtifact d
        artInterpreter art `shouldBe` Nothing
        artYanked art `shouldBe` False
        artProvenance art `shouldBe` Nothing

    it "carries the unpacked size as the artifact size (inline dist)" $ do
        d <- projectVersionOf sizedPackument (mkVersion Npm "1.0.0")
        artSize (soleArtifact d) `shouldBe` Just 6510

    it "falls back to <version>.tgz when the tarball URL has no filename segment" $ do
        -- A dist URL ending in a slash has no last segment, so the filename
        -- falls back to the conventional <version>.tgz form.
        d <- projectVersionOf trailingSlashPackument (mkVersion Npm "1.0.0")
        artFilename (soleArtifact d) `shouldBe` "1.0.0.tgz"

    it "keeps the SRI integrity even when the shasum is absent (inline dist)" $ do
        -- An integrity-only dist still yields the SRI hash (and only it).
        d <- projectVersionOf integrityOnlyPackument (mkVersion Npm "1.0.0")
        artHashes (soleArtifact d) `shouldBe` [Hash SRI "sha512-onlyintegrity=="]

    it "keeps the SHA-1 shasum even when the integrity is absent (inline dist)" $ do
        d <- projectVersionOf shasumOnlyPackument (mkVersion Npm "1.0.0")
        artHashes (soleArtifact d) `shouldBe` [Hash SHA1 "deadbeef"]

-- ── parseVersionDetails ──────────────────────────────────────────────────────

versionDetailsSpec :: Spec
versionDetailsSpec = describe "parseVersionDetails" $ do
    it "projects the requested version's details (is-odd@3.0.1)" $ do
        d <- projectVersion "is-odd.full.json" (mkVersion Npm "3.0.1")
        renderVersion (pkgVersion d) `shouldBe` "3.0.1"
        renderName (pkgName d) `shouldBe` "is-odd"

    it "fails when the requested version is absent from the packument" $ do
        body <- readFixture "is-odd.full.json"
        parseVersionDetails (RegistryResponse body) (mkVersion Npm "99.99.99")
            `shouldSatisfy` isLeft

-- ── parseVersionList ─────────────────────────────────────────────────────────

versionListSpec :: Spec
versionListSpec = describe "parseVersionList" $ do
    it "lists the packument's versions, preserving the raw strings (is-odd)" $ do
        body <- readFixture "is-odd.full.json"
        fmap (map unVersion) (parseVersionList (RegistryResponse body)) `shouldBe` Right ["3.0.1"]

    it "lists every key for a multi-version inline packument, in key order" $ do
        vs <- orFailParse (parseVersionList (RegistryResponse multiVersionPackument))
        map unVersion vs `shouldBe` ["1.0.0", "1.2.0", "2.0.0"]

-- ── failure handling ─────────────────────────────────────────────────────────

failureSpec :: Spec
failureSpec = describe "malformed input" $ do
    it "reports a ParseError on a body that is not JSON" $
        parsePackageInfo (RegistryResponse "this is not json") `shouldSatisfy` isLeft

    it "reports a ParseError on an empty package name" $
        -- A document whose `name` is the empty string cannot yield a PackageName.
        parsePackageInfo (RegistryResponse "{\"name\":\"\"}") `shouldSatisfy` isLeft

    it "reports a ParseError on a JSON value that is not a packument object" $
        -- Valid JSON of the wrong shape (here an array) is reported, not crashed.
        parsePackageInfo (RegistryResponse "[1,2,3]") `shouldSatisfy` isLeft

    it "reports a ParseError when a versions entry is not an object" $
        -- Each version must be an object; a scalar there is a parse failure, not
        -- a dropped version.
        parsePackageInfo (RegistryResponse "{\"name\":\"x\",\"versions\":{\"1.0.0\":42}}")
            `shouldSatisfy` isLeft

    it "fails parseVersionList on a non-JSON body too" $
        parseVersionList (RegistryResponse "nope") `shouldSatisfy` isLeft

-- ── projection totality (the fuzz target) ────────────────────────────────────

{- | The projection eats __untrusted__ upstream JSON (it decodes the response
body internally with 'eitherDecodeStrict' and then walks the wire shape into the
domain model), so every @parse*@ entry must be __total__: an arbitrary body may
never make it bottom; it must always return a typed 'Right' or a typed
@ParseError@ 'Left', never ⊥. These generative properties feed each entry point a
bounded-but-arbitrary 'Value' (encoded to a body) and a run of arbitrary bytes,
then fully evaluate the result so a partial function anywhere in the projection
surfaces as a caught exception rather than a pass. They are the projection-layer
companion to the wire-decoder totality properties in
"Ecluse.Registry.Npm.WireSpec".
-}
totalitySpec :: Spec
totalitySpec = describe "projection totality (arbitrary input never bottoms)" $ do
    describe "every projection entry is total over an arbitrary Value body" $ do
        it "parsePackageInfo" $
            hedgehog (projectionIsTotal (showResult . parsePackageInfo))
        it "parseVersionList" $
            hedgehog (projectionIsTotal (showResult . parseVersionList))
        it "parseVersionDetails" $
            hedgehog
                ( projectionIsTotal
                    (\r -> showResult (parseVersionDetails r (mkVersion Npm "1.0.0")))
                )

    describe "every projection entry is total over arbitrary bytes" $ do
        it "parsePackageInfo" $
            hedgehog (projectionBytesIsTotal (showResult . parsePackageInfo))
        it "parseVersionList" $
            hedgehog (projectionBytesIsTotal (showResult . parseVersionList))
        it "parseVersionDetails" $
            hedgehog
                ( projectionBytesIsTotal
                    (\r -> showResult (parseVersionDetails r (mkVersion Npm "1.0.0")))
                )

    it "the body generator reaches both a decodable packument and a rejected body" $
        hedgehog $ do
            v <- forAll genBody
            let resp = RegistryResponse (encodeToBody v)
                decoded = parsePackageInfo resp
            annotateShow v
            _ <- H.eval (showResult decoded)
            -- Non-vacuity: 'genBody' must reach both the projects-to-domain arm
            -- (the packument-shaped half) and the rejected-body arm (the
            -- arbitrary half), so the totality checks above are not all-failures.
            H.cover 5 "projects (Right)" (isRight decoded)
            H.cover 5 "rejects (Left)" (isLeft decoded)

-- ── totality helpers ─────────────────────────────────────────────────────────

{- | Assert a projection entry is __total__ over an arbitrary 'Value' body: encode
a 'genBody' value (which mixes fully-arbitrary JSON with packument-shaped objects,
so the __success__ path of the projection is exercised too, not just rejection)
into a response body and fully evaluate the entry's result (the @render@ argument
forces it to a 'String'), so 'H.eval' turns any bottom inside the projection into
a caught test failure rather than a pass.
-}
projectionIsTotal :: (RegistryResponse -> String) -> PropertyT IO ()
projectionIsTotal render = do
    v <- forAll genBody
    annotateShow v
    _ <- H.eval (length (render (RegistryResponse (encodeToBody v))))
    H.success

{- | Assert a projection entry is total over arbitrary bytes: a garbage body must
yield a typed 'ParseError' 'Left', never a crash.
-}
projectionBytesIsTotal :: (RegistryResponse -> String) -> PropertyT IO ()
projectionBytesIsTotal render = do
    bytes <- forAll (Gen.bytes (Range.linear 0 64))
    _ <- H.eval (length (render (RegistryResponse bytes)))
    H.success

-- | Force a projection result fully by rendering both arms to a 'String'.
showResult :: (Show a) => Either ParseError a -> String
showResult = \case
    Left e -> show e :: String
    Right a -> show a :: String

-- | Encode a generated 'Value' into a strict response body.
encodeToBody :: Value -> ByteString
encodeToBody = BL.toStrict . encode

{- | A recursive, depth- and breadth-__bounded__ arbitrary 'Aeson.Value' (the same
shape as "Ecluse.Registry.Npm.WireSpec"'s generator, kept in-file to avoid a new
module): the JSON scalar kinds plus small arrays and objects of recursively-
generated values, shrinking toward the scalars so it terminates. Object keys are
biased toward the real packument field names (@name@, @versions@, @dist-tags@, …)
so a generated object routinely reaches the projection's success arm.
-}
genValue :: H.Gen Value
genValue =
    Gen.recursive
        Gen.choice
        [ pure Null
        , Bool <$> Gen.bool
        , Number . fromInteger <$> genInteger
        , String <$> genJsonText
        ]
        [ Array . V.fromList <$> Gen.list (Range.linear 0 4) genValue
        , Object . KeyMap.fromList
            <$> Gen.list (Range.linear 0 4) ((,) <$> genKey <*> genValue)
        ]

{- | A response-body generator that mixes fully-arbitrary JSON ('genValue') with
packument-__shaped__ objects ('genPackumentish'), so a property driving a
projection reaches __both__ arms: arbitrary JSON almost always rejects (a 'Left'),
while a packument-shaped object usually projects (a 'Right'). The wire decoders
are lenient, so even the shaped half carries arbitrary values in its fields — it
is a /shape/ bias, not a valid-document oracle, which keeps the fuzzing honest.
-}
genBody :: H.Gen Value
genBody = Gen.frequency [(1, genValue), (1, genPackumentish)]

{- | A top-level object shaped like an npm packument: a (usually non-empty) string
@name@, a @versions@ map keyed by @1.0.0@ (the version 'parseVersionDetails' asks
for) whose entries are arbitrary objects carrying a @dist@ object, plus an
arbitrary @time@\/@dist-tags@. The values inside are still arbitrary, so this only
biases the /shape/ toward the projection's success arm; it does not hand-build a
known-valid document.
-}
genPackumentish :: H.Gen Value
genPackumentish = do
    name <- Gen.text (Range.linear 1 8) Gen.alphaNum
    versionObj <- genVersionish
    extra <- Gen.list (Range.linear 0 3) ((,) <$> genKey <*> genValue)
    pure . Object . KeyMap.fromList $
        [ (Key.fromText "name", String name)
        , (Key.fromText "versions", Object (KeyMap.singleton (Key.fromText "1.0.0") versionObj))
        ]
            <> extra

{- | A version-object-shaped 'Value': @name@\/@version@ strings and a @dist@ with a
@tarball@ URL string, plus a few arbitrary keys — enough that the wire manifest
decodes and the artifact projection has a tarball to read.
-}
genVersionish :: H.Gen Value
genVersionish = do
    tarball <- genJsonText
    extra <- Gen.list (Range.linear 0 3) ((,) <$> genKey <*> genValue)
    pure . Object . KeyMap.fromList $
        [ (Key.fromText "name", String "pkg")
        , (Key.fromText "version", String "1.0.0")
        , (Key.fromText "dist", Object (KeyMap.singleton (Key.fromText "tarball") (String tarball)))
        ]
            <> extra

{- | A small arbitrary integer to seed a JSON number (kept in a modest range;
'fromInteger' lifts it into aeson's 'Number' 'Scientific').
-}
genInteger :: H.Gen Integer
genInteger = Gen.integral (Range.linearFrom 0 (-100000) 100000)

-- | A short arbitrary JSON string value (unicode, to probe text handling).
genJsonText :: H.Gen Text
genJsonText = Gen.text (Range.linear 0 8) Gen.unicode

{- | An object key drawn from a pool biased toward the packument field names the
projection reads, so generated objects frequently satisfy them (otherwise almost
every object would miss @name@\/@versions@ and the success arm would go
unsampled). @1.0.0@ is included so a generated @versions@ map can be keyed by the
version the @parseVersionDetails@ property requests.
-}
genKey :: H.Gen Key.Key
genKey = Key.fromText <$> Gen.choice [Gen.element packumentKeys, genJsonText]
  where
    packumentKeys =
        [ "name"
        , "version"
        , "dist-tags"
        , "versions"
        , "time"
        , "dist"
        , "tarball"
        , "shasum"
        , "integrity"
        , "scripts"
        , "license"
        , "deprecated"
        , "hasInstallScript"
        , "_npmUser"
        , "maintainers"
        , "dependencies"
        , "1.0.0"
        , "latest"
        ]

-- ── inline packument fixtures ────────────────────────────────────────────────

{- | A full-form packument whose single version declares a @postinstall@ script
and __no__ @hasInstallScript@ key, so install-script presence must be derived.
-}
fullPostinstallPackument :: ByteString
fullPostinstallPackument =
    "{\"name\":\"derived\",\"dist-tags\":{\"latest\":\"1.0.0\"},\"versions\":{\"1.0.0\":\
    \{\"name\":\"derived\",\"version\":\"1.0.0\",\"scripts\":{\"postinstall\":\"node x.js\"},\
    \\"dist\":{\"tarball\":\"https://r/derived/-/derived-1.0.0.tgz\"}}}}"

{- | A packument exercising all four npm dependency maps on one version, so each
'DepKind' is covered.
-}
allDepKindsPackument :: ByteString
allDepKindsPackument =
    "{\"name\":\"deps\",\"versions\":{\"1.0.0\":{\"name\":\"deps\",\"version\":\"1.0.0\",\
    \\"dependencies\":{\"runtime-dep\":\"^1.0.0\"},\"devDependencies\":{\"dev-dep\":\"^2.0.0\"},\
    \\"peerDependencies\":{\"peer-dep\":\"^3.0.0\"},\"optionalDependencies\":{\"optional-dep\":\"^4.0.0\"},\
    \\"dist\":{\"tarball\":\"https://r/deps/-/deps-1.0.0.tgz\"}}}}"

{- | A full-form packument whose single version sets @hasInstallScript:false@
explicitly, so install presence is a determination rather than a derivation.
-}
noInstallScriptPackument :: ByteString
noInstallScriptPackument =
    "{\"name\":\"noscript\",\"versions\":{\"1.0.0\":{\"name\":\"noscript\",\"version\":\"1.0.0\",\
    \\"hasInstallScript\":false,\"dist\":{\"tarball\":\"https://r/noscript/-/noscript-1.0.0.tgz\"}}}}"

-- | A packument whose single version declares per-version @maintainers@.
maintainersPackument :: ByteString
maintainersPackument =
    "{\"name\":\"maint\",\"versions\":{\"1.0.0\":{\"name\":\"maint\",\"version\":\"1.0.0\",\
    \\"maintainers\":[{\"name\":\"carol\",\"email\":\"carol@example.test\"}],\
    \\"dist\":{\"tarball\":\"https://r/maint/-/maint-1.0.0.tgz\"}}}}"

-- | A packument whose version's @dist@ reports an @unpackedSize@.
sizedPackument :: ByteString
sizedPackument =
    "{\"name\":\"sized\",\"versions\":{\"1.0.0\":{\"name\":\"sized\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/sized/-/sized-1.0.0.tgz\",\"unpackedSize\":6510}}}}"

-- | A packument whose tarball URL ends in a slash (no filename segment).
trailingSlashPackument :: ByteString
trailingSlashPackument =
    "{\"name\":\"slash\",\"versions\":{\"1.0.0\":{\"name\":\"slash\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/slash/\"}}}}"

-- | A packument whose version's @dist@ carries only the SRI @integrity@.
integrityOnlyPackument :: ByteString
integrityOnlyPackument =
    "{\"name\":\"intg\",\"versions\":{\"1.0.0\":{\"name\":\"intg\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/intg/-/intg-1.0.0.tgz\",\"integrity\":\"sha512-onlyintegrity==\"}}}}"

-- | A packument whose version's @dist@ carries only the legacy SHA-1 @shasum@.
shasumOnlyPackument :: ByteString
shasumOnlyPackument =
    "{\"name\":\"sha\",\"versions\":{\"1.0.0\":{\"name\":\"sha\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/sha/-/sha-1.0.0.tgz\",\"shasum\":\"deadbeef\"}}}}"

-- | A packument with three versions, to check version-list extraction.
multiVersionPackument :: ByteString
multiVersionPackument =
    "{\"name\":\"multi\",\"versions\":{\
    \\"1.0.0\":{\"name\":\"multi\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://r/a.tgz\"}},\
    \\"1.2.0\":{\"name\":\"multi\",\"version\":\"1.2.0\",\"dist\":{\"tarball\":\"https://r/b.tgz\"}},\
    \\"2.0.0\":{\"name\":\"multi\",\"version\":\"2.0.0\",\"dist\":{\"tarball\":\"https://r/c.tgz\"}}}}"

-- ── helpers ──────────────────────────────────────────────────────────────────

-- | The canonical key of a 'PackageName' (verbatim for npm).
renderName :: PackageName -> Text
renderName = pkgCanonical

{- | The first projected 'Artifact' of a version. npm projects exactly one
artifact per version, so this is the whole of @pkgArtifacts@; taking the head of
the 'NonEmpty' is total.
-}
soleArtifact :: PackageDetails -> Artifact
soleArtifact d = let (art :| _) = pkgArtifacts d in art

-- | Whether a 'CodeExecSignal' is one of the @RunsCodeOnInstall@ determinations.
runsCode :: CodeExecSignal -> Bool
runsCode = \case
    RunsCodeOnInstall _ -> True
    _ -> False

{- | Read a committed fixture body by name (under @test\/unit\/fixtures\/npm\/@,
the path Cabal runs tests from).
-}
readFixture :: FilePath -> IO ByteString
readFixture name = readFileBS ("test/unit/fixtures/npm/" <> name)

{- | Project a fixture into a 'PackageInfo', failing the example with the
'ParseError' message on a projection failure.
-}
projectFixture :: FilePath -> IO PackageInfo
projectFixture name = do
    body <- readFixture name
    orFailParse (parsePackageInfo (RegistryResponse body))

-- | Project one version of a fixture into its 'PackageDetails'.
projectVersion :: FilePath -> Version -> IO PackageDetails
projectVersion name version = do
    body <- readFixture name
    orFailParse (parseVersionDetails (RegistryResponse body) version)

-- | Project one version of an inline packument body into its 'PackageDetails'.
projectVersionOf :: ByteString -> Version -> IO PackageDetails
projectVersionOf body version =
    orFailParse (parseVersionDetails (RegistryResponse body) version)

{- | Unwrap a projection result, failing the example with the 'ParseError'
message rather than crashing — keeping the suite total (no partial @error@).
-}
orFailParse :: Either ParseError a -> IO a
orFailParse = either expectationFailureWith pure

-- | Fail the running example with a 'ParseError' message.
expectationFailureWith :: ParseError -> IO a
expectationFailureWith e = fail ("unexpected ParseError: " <> show e)

{- | Parse an ISO-8601 timestamp for an expectation, in the example's own
'MonadFail' so an unparseable literal fails the test rather than crashing.
-}
readUTC :: (MonadFail m) => Text -> m UTCTime
readUTC = iso8601ParseM . toString
