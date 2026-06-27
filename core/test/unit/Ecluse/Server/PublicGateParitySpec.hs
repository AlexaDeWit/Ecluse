{- | Gate-level parity for the __public tarball fallback__
("Ecluse.Core.Server.Pipeline.gatePublicVersion").

The public fallback now gates the requested version off a __dedicated, uncached,
one-version projection__ ('Ecluse.Core.Registry.Npm.Project.parseVersionDetailsFromValue')
rather than projecting the whole 'Ecluse.Core.Package.PackageInfo' and looking the version
up. That gate is a __security boundary__, so this suite is the proof the narrowing is
behaviour-preserving: for the same packument @Value@, the version-targeted projection fed
through the gate ('Ecluse.Core.Server.Pipeline.gateVersion') yields the __identical__
'Ecluse.Core.Server.Pipeline.PublicArtifactGate' — the same admit\/refuse decision and the
same selected 'Ecluse.Core.Package.Artifact' — that projecting the whole packument and
gating the looked-up version does.

The cases walk the security-decisive dimensions the gate turns on: the publish-age
quarantine ('Ecluse.Core.Rules.Types.AllowIfOlderThan') deciding over @time[version]@
(aged → admit vs too-young → deny-by-default), the install-time-execution deny
('Ecluse.Core.Rules.Types.DenyInstallTimeExecution'), the integrity floor (at-floor admit
vs a below-floor SHA-1-only and a hashless refusal), and the filename selection (a match
admits the right artifact, a miss is a forwarded absence). Every case asserts both the
__parity__ (full-projection gate ≡ targeted gate) and the __expected__ outcome, so the
suite is at once a parity proof and a cross-ruleset correctness check of the gate.

The projector-level parity (the targeted projection equals the full projection at the
'Ecluse.Core.Package.PackageDetails' level, across present\/absent\/below-floor\/hashless\/
non-conventional-URL shapes) is proven separately in
"Ecluse.Registry.Npm.ProjectSpec"; this suite drives the verdict the rest of the way,
through the real 'Ecluse.Core.Server.Pipeline.gateVersion', under each ruleset.
-}
module Ecluse.Server.PublicGateParitySpec (spec) where

import Crypto.Hash (Digest, SHA512, hash)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (artFilename),
    PackageDetails,
    PackageInfo (infoVersions),
    PackageName,
    mkPackageName,
 )
import Ecluse.Core.Package.Integrity (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Core.Registry.Npm.Project (
    Projection (Projected),
    VersionProjection (VersionProjected),
    parsePackageInfoFromValue,
    parseVersionDetailsFromValue,
 )
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (
    EvalContext (EvalContext),
    PrecededRule,
    Rule (AllowIfOlderThan, DenyInstallTimeExecution),
    atDefaultPrecedence,
 )
import Ecluse.Core.Security (TarballHostPolicy (SameHostAsPackument), defaultLimits, lowerCaseHosts)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Pipeline (PublicArtifactGate (Admitted, Refused), gateVersion)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

spec :: Spec
spec = describe "Ecluse.Core.Server.Pipeline public-fallback gate (one-version vs full-projection parity)" $ do
    it "admits an aged, policy-clean, at-floor version — same Artifact as the full projection" $ do
        (full, targeted) <- gateBoth quarantine matchFilename (agedClean atFloorIntegrity)
        full `shouldBe` targeted
        admittedFilename full `shouldBe` Just matchFilename

    it "refuses a too-young version under the publish-age quarantine (AllowIfOlderThan over time[version])" $ do
        (full, targeted) <- gateBoth quarantine matchFilename (tooYoung atFloorIntegrity)
        full `shouldBe` targeted
        full `shouldSatisfy` isRefused

    it "refuses a version that runs an install script (DenyInstallTimeExecution), even aged and at-floor" $ do
        (full, targeted) <- gateBoth quarantineAndDeny matchFilename installScriptVersion
        full `shouldBe` targeted
        full `shouldSatisfy` isRefused

    it "refuses a below-floor (SHA-1-only) version the rules would otherwise admit" $ do
        (full, targeted) <- gateBoth quarantine matchFilename (agedClean sha1OnlyIntegrity)
        full `shouldBe` targeted
        full `shouldSatisfy` isRefused

    it "refuses a hashless version the rules would otherwise admit" $ do
        (full, targeted) <- gateBoth quarantine matchFilename (agedClean noIntegrity)
        full `shouldBe` targeted
        full `shouldSatisfy` isRefused

    it "is a forwarded miss when the requested filename matches no artifact" $ do
        (full, targeted) <- gateBoth quarantine "nope.tgz" (agedClean atFloorIntegrity)
        full `shouldBe` targeted
        full `shouldSatisfy` isRefused

-- ── driving both projection paths through the gate ──────────────────────────────

{- | Gate the requested version of a packument @Value@ two ways and return both
outcomes: through the whole-packument projection ('parsePackageInfoFromValue' then a
@versions@ lookup) and through the version-targeted projection
('parseVersionDetailsFromValue'), each fed to the /same/ 'gateVersion'. A present version
projects to @Just details@ on both arms; an absent one to 'Nothing'. The two must agree —
that equality is the security-boundary contract this suite asserts.
-}
gateBoth :: [PrecededRule] -> Text -> Value -> IO (Maybe PublicArtifactGate, Maybe PublicArtifactGate)
gateBoth rules file value = do
    deps <- gateDeps rules
    full <- traverse (gateVersion evalCtx deps file) (fullDetails value)
    targeted <- traverse (gateVersion evalCtx deps file) (targetedDetails value)
    pure (full, targeted)

-- | The 'PackageDetails' the whole-packument projection holds at the requested version.
fullDetails :: Value -> Maybe PackageDetails
fullDetails value = case parsePackageInfoFromValue route value of
    Right (Projected info) -> Map.lookup (renderVersion version) (infoVersions info)
    _ -> Nothing

-- | The 'PackageDetails' the version-targeted projection yields for the requested version.
targetedDetails :: Value -> Maybe PackageDetails
targetedDetails value = case parseVersionDetailsFromValue route value version of
    Right (VersionProjected _ details) -> details
    _ -> Nothing

-- | The artifact filename a gate admit selected, if it admitted.
admittedFilename :: Maybe PublicArtifactGate -> Maybe Text
admittedFilename = \case
    Just (Admitted artifact) -> Just (artFilename artifact)
    _ -> Nothing

-- | Whether the gate refused (a policy denial, an integrity-floor drop, or a miss).
isRefused :: Maybe PublicArtifactGate -> Bool
isRefused = \case
    Just (Refused _) -> True
    _ -> False

-- ── the gate's inputs ──────────────────────────────────────────────────────────

-- | The requested package and version every case gates.
route :: PackageName
route = mkPackageName Npm Nothing "pkg"

version :: Version
version = mkVersion Npm "1.0.0"

-- | A fixed wall clock the aged fixtures read as well past the quarantine and the
-- too-young fixture reads as inside it.
evalCtx :: EvalContext
evalCtx = EvalContext (UTCTime (fromGregorian 2020 1 1) 0)

-- | The publish-age quarantine alone (the shipped @min-age@ default).
quarantine :: [PrecededRule]
quarantine = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

-- | The quarantine plus the install-time-execution deny.
quarantineAndDeny :: [PrecededRule]
quarantineAndDeny =
    [ atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))
    , atDefaultPrecedence DenyInstallTimeExecution
    ]

-- | Serve dependencies carrying the given ruleset and the hard integrity floor; the gate
-- reads only 'pdRules' and 'pdMinIntegrity', the rest are inert placeholders.
gateDeps :: [PrecededRule] -> IO PackumentDeps
gateDeps rules = do
    prepared <- prepare rules
    pure
        PackumentDeps
            { pdPrivateBaseUrl = "http://private.invalid"
            , pdPublicBaseUrl = "http://public.invalid"
            , pdMountBaseUrl = "http://proxy.test"
            , pdMirrorTarget = "http://mirror.test"
            , pdRules = prepared
            , pdTarballHostPolicy = SameHostAsPackument
            , pdAllowedInternalHosts = lowerCaseHosts mempty
            , pdLimits = defaultLimits
            , pdInboundToken = Nothing
            , pdNow = pure (UTCTime (fromGregorian 2020 1 1) 0)
            , pdHelp = Nothing
            , pdMinIntegrity = defaultMinIntegrity
            , pdMinTrustedIntegrity = defaultMinTrustedIntegrity
            }

-- ── packument fixtures ──────────────────────────────────────────────────────────

-- | The filename the fixtures' @dist.tarball@ resolves to (its last path segment).
matchFilename :: Text
matchFilename = "pkg-1.0.0.tgz"

{- | A single-version packument published well before the quarantine window (so
'AllowIfOlderThan' admits it), with no install script, carrying the given @dist@
integrity fields.
-}
agedClean :: [Pair] -> Value
agedClean integrity = packument "2019-01-01T00:00:00.000Z" integrity []

{- | A single-version packument published two days before @now@ — inside the 7-day
quarantine, so 'AllowIfOlderThan' yields no decision and the version is denied by default.
-}
tooYoung :: [Pair] -> Value
tooYoung integrity = packument "2019-12-30T00:00:00.000Z" integrity []

-- | An aged, at-floor version that declares an install script (the deny trigger).
installScriptVersion :: Value
installScriptVersion =
    packument
        "2019-01-01T00:00:00.000Z"
        atFloorIntegrity
        [ "hasInstallScript" .= True
        , "scripts" .= object ["install" .= ("node -e \"\"" :: Text)]
        ]

-- | A @dist.integrity@ meeting the floor: a well-formed SHA-512 SRI.
atFloorIntegrity :: [Pair]
atFloorIntegrity = ["integrity" .= sha512Sri "pkg-1.0.0 artifact bytes"]

-- | A @dist.shasum@ only — a SHA-1 digest, below the default SHA-256 floor.
sha1OnlyIntegrity :: [Pair]
sha1OnlyIntegrity = ["shasum" .= ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :: Text)]

-- | No digest of any kind — a hashless artifact (classified @NoIntegrity@).
noIntegrity :: [Pair]
noIntegrity = []

{- | A one-version packument for @pkg@, with the given publish time, @dist@ integrity
fields, and extra version-object fields (the install-script signals). The @dist.tarball@
self-hosts so its filename is 'matchFilename'.
-}
packument :: Text -> [Pair] -> [Pair] -> Value
packument publishTime integrity extraVersionFields =
    object
        [ "name" .= ("pkg" :: Text)
        , "dist-tags" .= object ["latest" .= ("1.0.0" :: Text)]
        , "versions"
            .= object
                [ "1.0.0"
                    .= object
                        ( [ "name" .= ("pkg" :: Text)
                          , "version" .= ("1.0.0" :: Text)
                          , "dist" .= object (["tarball" .= ("http://public.invalid/pkg/-/pkg-1.0.0.tgz" :: Text)] <> integrity)
                          ]
                            <> extraVersionFields
                        )
                ]
        , "time" .= object ["1.0.0" .= publishTime]
        ]

-- | The Subresource-Integrity @sha512-<base64>@ string over the given bytes.
sha512Sri :: ByteString -> Text
sha512Sri bytes = "sha512-" <> decodeUtf8 (convertToBase Base64 (hash bytes :: Digest SHA512) :: ByteString)
