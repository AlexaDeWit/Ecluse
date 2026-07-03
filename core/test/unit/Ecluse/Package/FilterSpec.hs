module Ecluse.Package.FilterSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Hedgehog (Gen, annotateShow, assert, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Trust (Untrusted),
    mkPackageName,
 )
import Ecluse.Core.Package.Filter (FilterPlan (..), filterPlan)
import Ecluse.Core.Rules.Types (
    Decision (Admitted),
    EvalContext (EvalContext),
    PrecededRule,
    Rule (AllowIfOlderThan, DenyInstallTimeExecution),
    atDefaultPrecedence,
 )
import Ecluse.Core.Version (compareVersions, isStable, mkVersion, parseVersionKey, unVersion)

spec :: Spec
spec = do
    survivorSpec
    latestSpec
    decisionsSpec
    propertiesSpec

-- | A fixed "now" so the age-based admit/deny axis is deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

ctx :: EvalContext
ctx = EvalContext now

{- | The policy under test: a 7-day publish-age quarantine plus an install-script
deny. A version is approved iff it is at least 7 days old and declares no install
script -- so survival is controlled purely by the typed fixture, exercising the real
rules engine over the domain model (no @Value@ in sight).
-}
policy :: [PrecededRule]
policy =
    [ atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))
    , atDefaultPrecedence DenyInstallTimeExecution
    ]

name :: PackageName
name = mkPackageName Npm Nothing "thing"

-- | An instant @ageDays@ before 'now', for a version's publish time.
publishedDaysAgo :: Integer -> UTCTime
publishedDaysAgo ageDays = addUTCTime (negate (fromInteger ageDays * nominalDay)) now

{- | A per-version snapshot keyed only on what the filter reads: the parsed
version, the publish time (the age gate), and the install-code signal (the deny).
Everything else is inert.
-}
detailsAt :: Text -> Integer -> Bool -> PackageDetails
detailsAt rawVer ageDays hasInstall =
    PackageDetails
        { pkgName = name
        , pkgVersion = mkVersion Npm rawVer
        , pkgPublishedAt = Just (publishedDaysAgo ageDays)
        , pkgInstallCode = if hasInstall then RunsCodeOnInstall "postinstall" else NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = inertArtifact :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        }

inertArtifact :: Artifact
inertArtifact =
    Artifact
        { artFilename = "thing.tgz"
        , artUrl = "https://upstream.test/thing.tgz"
        , artKind = Tarball
        , artHashes = []
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | Build a 'PackageInfo' from @(rawVersion, ageDays, hasInstall)@ triples, with
@dist-tags.latest@ pointed at the given version (or none).
-}
infoOf :: Maybe Text -> [(Text, Integer, Bool)] -> PackageInfo
infoOf latest vs =
    PackageInfo
        { infoName = name
        , infoVersions = Map.fromList [(v, detailsAt v age install) | (v, age, install) <- vs]
        , infoDistTags = maybe Map.empty (Map.singleton "latest" . mkVersion Npm) latest
        , infoInvalidEntries = []
        }

survivorSpec :: Spec
survivorSpec = describe "fpSurvivors" $ do
    it "keeps only the approved versions, dropping a too-young one" $ do
        -- 1.0.0 is 30 days old (approved); 2.0.0 is 1 day old (denied by the age gate).
        plan <- filterPlan ctx policy (infoOf (Just "2.0.0") [("1.0.0", 30, False), ("2.0.0", 1, False)])
        fpSurvivors plan `shouldBe` Set.singleton "1.0.0"

    it "drops a version that declares an install script even when old enough" $ do
        -- Both old enough, but 2.0.0 runs an install script → denied by the deny rule.
        plan <- filterPlan ctx policy (infoOf (Just "1.0.0") [("1.0.0", 30, False), ("2.0.0", 30, True)])
        fpSurvivors plan `shouldBe` Set.singleton "1.0.0"

    it "is empty when nothing is approved" $ do
        plan <- filterPlan ctx policy (infoOf (Just "2.0.0") [("1.0.0", 1, False), ("2.0.0", 1, False)])
        fpSurvivors plan `shouldBe` Set.empty

latestSpec :: Spec
latestSpec = describe "fpLatest" $ do
    it "keeps a surviving upstream latest rather than promoting a higher survivor" $ do
        -- Upstream latest is 1.0.0; both survive. Keep-unless-denied: 1.0.0 stays,
        -- never promoted to the higher 2.0.0.
        plan <- filterPlan ctx policy (infoOf (Just "1.0.0") [("1.0.0", 30, False), ("2.0.0", 30, False)])
        latestRaw plan `shouldBe` Just "1.0.0"

    it "repoints latest down to a surviving version when the chosen latest is denied" $ do
        -- Upstream latest aims at the denied 2.0.0; repoint to the surviving 1.0.0.
        plan <- filterPlan ctx policy (infoOf (Just "2.0.0") [("1.0.0", 30, False), ("2.0.0", 1, False)])
        latestRaw plan `shouldBe` Just "1.0.0"

    it "prefers the highest stable survivor when repointing over a prerelease" $ do
        -- Upstream latest (3.0.0) is denied; survivors are a stable 1.0.0 and a
        -- prerelease 2.0.0-rc.1. Stable-preferring repoint chooses 1.0.0.
        plan <-
            filterPlan
                ctx
                policy
                (infoOf (Just "3.0.0") [("1.0.0", 30, False), ("2.0.0-rc.1", 30, False), ("3.0.0", 1, False)])
        latestRaw plan `shouldBe` Just "1.0.0"

    it "is Nothing when nothing survives" $ do
        plan <- filterPlan ctx policy (infoOf (Just "1.0.0") [("1.0.0", 1, False)])
        fpLatest plan `shouldBe` Nothing

decisionsSpec :: Spec
decisionsSpec = describe "fpDecisions" $ do
    it "carries one decision per version (survivors and denials alike)" $ do
        plan <- filterPlan ctx policy (infoOf (Just "2.0.0") [("1.0.0", 30, False), ("2.0.0", 1, False)])
        length (fpDecisions plan) `shouldBe` 2

    it "is all-non-approved when nothing survives" $ do
        plan <- filterPlan ctx policy (infoOf (Just "1.0.0") [("1.0.0", 1, False), ("2.0.0", 1, True)])
        length (fpDecisions plan) `shouldBe` 2
        any isApproved (fpDecisions plan) `shouldBe` False

propertiesSpec :: Spec
propertiesSpec = describe "properties" $ do
    it "survivors are exactly the approved version keys" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan ctx policy (toInfo spec'))
            fpSurvivors plan === approvedKeys spec'

    it "decisions number one per version, all non-approved when no survivor" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan ctx policy (toInfo spec'))
            length (fpDecisions plan) === length (specVersions spec')
            when (Set.null (fpSurvivors plan)) $
                assert (not (any isApproved (fpDecisions plan)))

    it "latest, when present, is always a surviving version" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan ctx policy (toInfo spec'))
            case fpLatest plan of
                Nothing -> assert (Set.null (fpSurvivors plan))
                Just v -> assert (unVersion v `Set.member` fpSurvivors plan)

    it "a surviving upstream latest is kept, never promoted to a higher survivor" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan ctx policy (toInfo spec'))
            -- When the upstream-chosen latest itself survives, keep-unless-denied
            -- holds it in place regardless of any higher survivor.
            case specLatest spec' of
                Just chosen
                    | chosen `Set.member` fpSurvivors plan ->
                        latestRaw plan === Just chosen
                _ -> H.success

    it "a repointed latest is the highest stable survivor when any survivor is stable" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan ctx policy (toInfo spec'))
            let survivors = fpSurvivors plan
                -- Repoint only happens when the chosen latest did not survive.
                chosenSurvived = maybe False (`Set.member` survivors) (specLatest spec')
                stableSurvivors = filter isStableRaw (Set.toList survivors)
            when (not chosenSurvived && not (null stableSurvivors)) $
                case latestRaw plan of
                    Just l -> do
                        annotateShow (l, stableSurvivors)
                        assert (isStableRaw l)
                        assert (all (\s -> compareVersions (mkVersion Npm s) (mkVersion Npm l) /= Just GT) stableSurvivors)
                    Nothing -> annotateShow survivors >> H.failure

{- | A generated logical packument: a chosen @latest@ target plus versions, each
with an age and an install-script flag. Survival is derived from age (≥ 7 days) and
the absence of an install script against 'policy'.
-}
data GenSpec = GenSpec
    { specLatest :: Maybe Text
    , specVersions :: [(Text, Integer, Bool)]
    }
    deriving stock (Show)

toInfo :: GenSpec -> PackageInfo
toInfo (GenSpec latest vs) = infoOf latest vs

-- | The keys 'policy' would approve: at least 7 days old and no install script.
approvedKeys :: GenSpec -> Set Text
approvedKeys =
    Set.fromList . map fst3 . filter (\(_, age, install) -> age >= 7 && not install) . specVersions
  where
    fst3 (a, _, _) = a

genSpec :: Gen GenSpec
genSpec = do
    n <- Gen.int (Range.linear 0 6)
    let versionStrings = take n versionPool
    triples <-
        forM versionStrings $ \v -> do
            age <- Gen.integral (Range.linear 0 60)
            install <- Gen.bool
            pure (v, age, install)
    latest <- case versionStrings of
        [] -> pure Nothing
        _ -> Just <$> Gen.element versionStrings
    pure (GenSpec latest triples)

-- A pool mixing stable and prerelease semver so repointing exercises both bands.
versionPool :: [Text]
versionPool = ["1.0.0", "1.1.0", "2.0.0-rc.1", "2.0.0", "3.0.0-beta", "10.0.0"]

-- | The resolved @latest@ as its raw version string, if present.
latestRaw :: FilterPlan -> Maybe Text
latestRaw = fmap unVersion . fpLatest

isApproved :: Decision -> Bool
isApproved = \case
    Admitted{} -> True
    _ -> False

-- | Whether a raw npm version string parses to a stable (non-prerelease) release.
isStableRaw :: Text -> Bool
isStableRaw raw = either (const False) isStable (parseVersionKey Npm raw)
