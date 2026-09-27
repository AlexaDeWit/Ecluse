-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm assembly preserves admitted source identities and installation fields.
module Ecluse.Core.Registry.Npm.FilterSpec (spec) where

import Control.Exception (evaluate)
import Data.Aeson (Value (Array, Object, String), eitherDecodeStrict)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, nominalDay)
import Hedgehog (Gen, annotateShow, assert, failure, forAll, success, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageInfo, mkPackageName)
import Ecluse.Core.Package.Entry (AdmittedEntry (admittedFilename, admittedKey), EntryKey (..))
import Ecluse.Core.Package.Filter (fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Merge (MergePlan (mpArtifacts, mpSurvivors), Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Npm.Filter (
    assembleMergedPackument,
    npmDocumentName,
    rewriteVersion,
 )

import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Route (tarballPath)
import Ecluse.Core.Rules.Types (
    Decision,
    EvalContext (EvalContext),
    PrecededRule,
    Rule (AllowIfOlderThan),
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Core.Text (joinUrlPath)
import Ecluse.Test.Json (asObject, fieldAt, mapAt, objectAt, textAt)
import Ecluse.Test.Registry.Npm qualified as NpmFixture
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Rules (atDefaultPrecedence, filterPlan, inertRuleDeps, isApproved)
import Ecluse.Test.Snapshot (digestOf, jsonSnapshot, projectJsonSnapshot)
import Ecluse.Test.Support (decodeJsonOrFail, expectRight)

-- | Pin plan replay, source admission and artifact URL rewriting.
spec :: Spec
spec = do
    nameGateSpec
    rewriteSpec
    filterSpec
    entryIdentitySpec
    coherenceSpec
    propertiesSpec

entryIdentitySpec :: Spec
entryIdentitySpec = describe "npm artifact-entry admission" $ do
    it "requires the admitted object key and exact upstream snapshot" $ do
        let version = NpmFixture.versionValue (NpmFixture.versionSpec "thing" "1.0.0" "https://upstream.test/thing-1.0.0.tgz")
            raw = NpmFixture.packumentValue "thing" "1.0.0" [("1.0.0", version)] [] []
        source <- projectJsonSnapshot (projectNpmManifest defaultLimits (mkPackageName Npm Nothing "thing")) raw
        plan <- expectRight (maybeToRight ("expected merge plan" :: Text) (mergePackuments [(GatedSource, fst <$> source)]))
        let rawSource = snd <$> source
            assemble bySource selection = mapAt "versions" (asObject (assembleMergedPackument base bySource selection raw))
            sources = Map.singleton 0 rawSource
            wrongKey entry = entry{admittedKey = ArrayEntry 0}
        Map.keys (assemble sources plan) `shouldBe` ["1.0.0"]
        assemble sources plan{mpArtifacts = mempty} `shouldBe` mempty
        assemble sources plan{mpArtifacts = fmap (\entries -> entries <> entries) (mpArtifacts plan)} `shouldBe` mempty
        assemble sources plan{mpArtifacts = fmap (fmap (\entry -> entry{admittedFilename = ""})) (mpArtifacts plan)} `shouldBe` mempty
        assemble sources plan{mpArtifacts = fmap (fmap wrongKey) (mpArtifacts plan)} `shouldBe` mempty
        assemble (Map.singleton 0 (Object mempty <$ rawSource)) plan `shouldBe` mempty
        assemble (Map.singleton 0 (Array mempty <$ rawSource)) plan `shouldBe` mempty
        assemble (Map.singleton 1 rawSource) plan `shouldBe` mempty
        assemble (Map.singleton 0 rawSource{snapshotDigest = digestOf "different upstream bytes"}) plan `shouldBe` mempty

-- The rewrite uses the same npm name grammar as projection and routing.
nameGateSpec :: Spec
nameGateSpec = describe "npmDocumentName -- the one npm name grammar" $
    for_ NpmFixture.npmNameVerdicts $ \(raw, valid) ->
        it (NpmFixture.nameVerdictLabel raw valid) $
            isJust (npmDocumentName (KeyMap.singleton "name" (String raw))) `shouldBe` valid

now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

ctx :: EvalContext
ctx = EvalContext now Nothing

quarantine :: [PrecededRule]
quarantine = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

publishedDaysAgo :: Integer -> Text
publishedDaysAgo = NpmFixture.publishedDaysAgo now

base :: Text
base = "https://proxy.test/npm"

thingPrefix :: Text -> Maybe Text
thingPrefix = servedUrlFor base "thing"

servedUrlFor :: Text -> Text -> Text -> Maybe Text
servedUrlFor mountBase package file = do
    name <- rightToMaybe (projectName package)
    path <- tarballPath name file
    pure (joinUrlPath mountBase path)

rewriteSpec :: Spec
rewriteSpec = describe "rewriteVersion" $ do
    it "rewrites dist.tarball to {prefix}/-/{file}" $ do
        v <- versionValue "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        versionTarball (rewriteVersion thingPrefix v)
            `shouldBe` Just "https://proxy.test/npm/thing/-/thing-1.0.0.tgz"

    it "preserves unmodelled keys on the version and dist objects" $ do
        v <- versionValue "https://upstream.test/thing/-/thing-1.0.0.tgz" [("customField", "\"kept\""), ("dist-extra-marker", "true")]
        let r = rewriteVersion thingPrefix v
        fieldAt "customField" r `shouldBe` Just (String "kept")
        bareDistKey "fileCount" r `shouldBe` Just (Aeson.Number 7)

    it "leaves a version with no dist object untouched" $ do
        v <- decodeJsonOrFail "{\"name\":\"thing\",\"version\":\"1.0.0\"}"
        rewriteVersion thingPrefix v `shouldBe` v

    it "leaves a tarball with no filename segment untouched" $ do
        v <- versionValue "https://upstream.test/thing/" []
        versionTarball (rewriteVersion thingPrefix v) `shouldBe` Just "https://upstream.test/thing/"

    it "is idempotent" $ do
        v <- versionValue "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        let once = rewriteVersion thingPrefix v
        rewriteVersion thingPrefix once `shouldBe` once

filterSpec :: Spec
filterSpec = describe "assembleMergedPackument (plan replay)" $ do
    it "removes a denied version from versions and time, keeping the approved one" $ do
        -- 2.0.0 is 1 day old, denied by the quarantine. 1.0.0 is 30 days old, approved.
        filtered <- filterTo twoVersions
        Map.keys (versionsOf filtered) `shouldBe` ["1.0.0"]
        Map.keys (timeKeysOf filtered) `shouldBe` ["1.0.0"]

    it "repoints latest down to a surviving version when the chosen latest is denied" $ do
        -- Upstream latest points at the denied 2.0.0. Keep-unless-denied repoints it
        -- down to the surviving 1.0.0.
        filtered <- filterTo twoVersions
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "keeps a surviving upstream latest rather than promoting a higher survivor" $ do
        filtered <- filterTo latestKeptBelowHigherSurvivor
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "drops a stale tag that pointed at a removed version" $ do
        -- `beta` aimed at the denied 2.0.0: dropped, not repointed.
        filtered <- filterTo twoVersionsWithBeta
        distTag "beta" filtered `shouldBe` Nothing
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "keeps a tag that points at a surviving version" $ do
        filtered <- filterTo twoVersionsStableTag
        distTag "stable" filtered `shouldBe` Just "1.0.0"

    it "preserves unmodelled keys on a surviving version and top-level" $ do
        filtered <- filterTo survivorWithExtras
        fieldAt "_id" (Object (rawObject filtered)) `shouldBe` Just (String "thing")
        versionKey "1.0.0" "customField" (Object (rawObject filtered)) `shouldBe` Just (String "kept")

    it "drops a denied version from time but keeps created/modified bookkeeping" $ do
        -- `time` carries npm's unmodelled `created`/`modified` keys alongside the
        -- per-version timestamps. Only the denied 2.0.0 entry must go.
        filtered <- filterTo twoVersionsWithTimeBookkeeping
        let t = timeKeysOf filtered
        Map.member "created" t `shouldBe` True
        Map.member "modified" t `shouldBe` True
        Map.member "1.0.0" t `shouldBe` True
        Map.member "2.0.0" t `shouldBe` False

    it "signals NoSurvivors, carrying each denied version's decision, when nothing survives" $ do
        -- Both versions are 1 day old: neither clears the quarantine.
        (info, v) <- loadPackument allYoung
        applyTo ctx quarantine info v >>= \case
            NoSurvivors decisions -> do
                length decisions `shouldBe` 2
                any isApproved decisions `shouldBe` False
            Assembled _ -> expectationFailure "expected NoSurvivors, got an assembled document"

    it "assembles onto a non-object base as an object carrying only the plan-owned keys" $ do
        -- The pipeline never hands a non-object here, because a non-object body fails projection.
        -- The assembly is total anyway, and it fabricates no keys beyond the plan-owned ones.
        (info, _) <- loadPackument oneVersionPackument
        applyTo ctx quarantine info (Array mempty) >>= \case
            NoSurvivors _ -> expectationFailure "expected an assembled document"
            Assembled out -> do
                Map.keys (mapAt "versions" (asObject out)) `shouldBe` []
                sort (map Key.toText (KeyMap.keys (asObject out))) `shouldBe` ["dist-tags", "time", "versions"]

    it "rewrites a surviving version's dist.tarball under the mount base in the assembly pass" $ do
        -- The assembly fuses in the rewrite (one pass over the versions), so the
        -- assembled document already carries {base}/{pkg}/-/{file}.
        filtered <- filterTo twoVersions
        tarballAt "1.0.0" (Object (rawObject filtered))
            `shouldBe` Just "https://proxy.test/npm/thing/-/thing-1.0.0.tgz"

    it "rewrites a scoped survivor under {base}/@scope/name/-/{file} in the assembly pass" $ do
        -- The prefix embeds the scoped @scope/name form npm uses in URLs. The scope
        -- separator must survive the component-safety gate.
        filtered <- filterTo scopedPackument
        tarballAt "1.0.0" (Object (rawObject filtered))
            `shouldBe` Just "https://proxy.test/npm/@myorg/thing/-/thing-1.0.0.tgz"

    it "ignores a trailing slash on the mount base in the assembly pass" $ do
        (info, v) <- loadPackument oneVersionPackument
        applyToAt "https://proxy.test/npm/" ctx quarantine info v >>= \case
            Assembled out ->
                tarballAt "1.0.0" out
                    `shouldBe` Just "https://proxy.test/npm/thing/-/thing-1.0.0.tgz"
            NoSurvivors _ -> expectationFailure "expected survivors, got NoSurvivors"

    it "leaves a tarball untouched when the document's name carries a traversal" $
        -- The projection refuses such a name first, so the assembly's own gate is defence in
        -- depth over the raw document. It never interpolates an unsafe name.
        tarballUnderName traversalNamePackument
            `shouldReturn` Just "https://upstream.test/thing/-/thing-1.0.0.tgz"

    it "leaves a tarball untouched when the document's name carries a control character" $
        tarballUnderName controlCharNamePackument
            `shouldReturn` Just "https://upstream.test/thing/-/thing-1.0.0.tgz"

    it "drops a version broken in a required field from the served body, keeping the healthy one" $ do
        -- 2.0.0's `dist` is a scalar, and it is 30 days old, so it would clear the quarantine if it
        -- decoded. Its absence proves the decode dropped it, not the age policy.
        filtered <- filterTo healthyPlusBroken
        Map.keys (versionsOf filtered) `shouldBe` ["1.0.0"]
        Map.keys (timeKeysOf filtered) `shouldBe` ["1.0.0"]

coherenceSpec :: Spec
coherenceSpec = describe "coherence of the filtered packument" $ do
    it "keeps latest pointing at a key that is present in versions" $ do
        filtered <- filterTo twoVersionsWithBeta
        let vs = Map.keysSet (versionsOf filtered)
        case distTag "latest" filtered of
            Just l -> Set.member l vs `shouldBe` True
            Nothing -> expectationFailure "latest must be present after filtering"

    it "keeps time entries for exactly the surviving versions" $ do
        filtered <- filterTo twoVersionsWithBeta
        Map.keysSet (timeKeysOf filtered) `shouldBe` Map.keysSet (versionsOf filtered)

    it "synthesises a minimal dist-tags.latest when upstream carried none" $ do
        filtered <- filterTo noDistTagsPackument
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "synthesises latest when dist-tags is present but null (not merely absent)" $ do
        -- The projection reads `dist-tags: null` as absent, but the raw body still carries the
        -- null. Without repair the document would ship with no resolvable latest.
        filtered <- filterTo nullDistTagsPackument
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "keeps an admitted but unparseable-version key and still resolves a present latest" $ do
        -- The admitted key cannot be ordered as semver, but latest must still resolve.
        filtered <- filterTo unparseableSurvivorPackument
        Map.member "banana" (versionsOf filtered) `shouldBe` True
        case distTag "latest" filtered of
            Just l -> Set.member l (Map.keysSet (versionsOf filtered)) `shouldBe` True
            Nothing -> expectationFailure "latest must resolve even with an unparseable survivor"

propertiesSpec :: Spec
propertiesSpec = describe "properties" $ do
    it "per-version rewriting is idempotent" $
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            v <- decodeOrFail (renderPackument spec')
            b <- forAll genBase
            let p = servedUrlFor b (specName spec')
                versions = mapAt "versions" (asObject v)
                once = fmap (rewriteVersion p) versions
            fmap (rewriteVersion p) once === once

    it "every served version's tarball is rewritten under {base}/{pkg}/-/" $
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            (info, v) <- loadOrFail (renderPackument spec')
            b <- forAll genBase
            let prefix = joinUrlPath b (specName spec') <> "/-/"
            liftIO (applyToAt b ctx quarantine info v) >>= \case
                NoSurvivors _ -> success
                Assembled out ->
                    forM_ (Map.keys (mapAt "versions" (asObject out))) $ \ver ->
                        case tarballAt ver out of
                            Just url -> H.diff prefix T.isPrefixOf url
                            Nothing -> annotateShow ver >> failure

    it "no surviving versions or tags reference a denied version" $
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            (info, v) <- loadOrFail (renderPackument spec')
            let denied = deniedVersions spec'
            liftIO (applyTo ctx quarantine info v) >>= \case
                NoSurvivors _ -> success
                Assembled out -> do
                    let o = asObject out
                        survivingKeys = Map.keysSet (mapAt "versions" o)
                        timeKeys = Map.keysSet (mapAt "time" o)
                        tagTargets = distTagValues o
                    -- no denied version survives in versions or time
                    assert (Set.null (Set.intersection survivingKeys denied))
                    assert (Set.null (Set.intersection timeKeys denied))
                    -- no dist-tag aims at a denied version
                    assert (all (`Set.notMember` denied) tagTargets)

    it "latest is always present and points at a surviving version" $
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            (info, v) <- loadOrFail (renderPackument spec')
            liftIO (applyTo ctx quarantine info v) >>= \case
                NoSurvivors _ -> success
                Assembled out -> do
                    let o = asObject out
                        survivingKeys = Map.keysSet (mapAt "versions" o)
                    case textAt "latest" (objectAt "dist-tags" o) of
                        Just l -> assert (Set.member l survivingKeys)
                        Nothing -> annotateShow out >> failure

    it "the assembled document forces deeply without bottoming (the metadataAssemble never-throws contract)" $
        -- A deferred failure would escape the request perimeter during response encoding.
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            (info, v) <- loadOrFail (renderPackument spec')
            liftIO (applyTo ctx quarantine info v) >>= \case
                NoSurvivors _ -> success
                Assembled out -> do
                    _ <- liftIO (evaluate (force out))
                    success

oneVersionPackument :: ByteString
oneVersionPackument =
    encodePackument
        "thing"
        Nothing
        [("latest", "1.0.0")]
        [versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []]
        [("1.0.0", publishedDaysAgo 30)]

scopedPackument :: ByteString
scopedPackument =
    encodePackument
        "@myorg/thing"
        Nothing
        [("latest", "1.0.0")]
        [versionLit "@myorg/thing" "1.0.0" "https://upstream.test/@myorg/thing/-/thing-1.0.0.tgz" []]
        [("1.0.0", publishedDaysAgo 30)]

twoVersions :: ByteString
twoVersions =
    encodePackument
        "thing"
        Nothing
        [("latest", "2.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "2.0.0" "https://upstream.test/thing/-/thing-2.0.0.tgz" []
        ]
        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 1)]

latestKeptBelowHigherSurvivor :: ByteString
latestKeptBelowHigherSurvivor =
    encodePackument
        "thing"
        Nothing
        [("latest", "1.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "2.0.0" "https://upstream.test/thing/-/thing-2.0.0.tgz" []
        ]
        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]

twoVersionsWithBeta :: ByteString
twoVersionsWithBeta =
    encodePackument
        "thing"
        Nothing
        [("latest", "2.0.0"), ("beta", "2.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "2.0.0" "https://upstream.test/thing/-/thing-2.0.0.tgz" []
        ]
        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 1)]

twoVersionsStableTag :: ByteString
twoVersionsStableTag =
    encodePackument
        "thing"
        Nothing
        [("latest", "2.0.0"), ("stable", "1.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "2.0.0" "https://upstream.test/thing/-/thing-2.0.0.tgz" []
        ]
        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 1)]

healthyPlusBroken :: ByteString
healthyPlusBroken =
    encodePackument
        "thing"
        Nothing
        [("latest", "1.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , ("2.0.0", "{\"name\":\"thing\", \"version\":\"2.0.0\", \"dist\":5}")
        ]
        [("1.0.0", publishedDaysAgo 30), ("2.0.0", publishedDaysAgo 30)]

allYoung :: ByteString
allYoung =
    encodePackument
        "thing"
        Nothing
        [("latest", "2.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "2.0.0" "https://upstream.test/thing/-/thing-2.0.0.tgz" []
        ]
        [("1.0.0", publishedDaysAgo 1), ("2.0.0", publishedDaysAgo 1)]

twoVersionsWithTimeBookkeeping :: ByteString
twoVersionsWithTimeBookkeeping =
    encodePackument
        "thing"
        Nothing
        [("latest", "2.0.0")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "2.0.0" "https://upstream.test/thing/-/thing-2.0.0.tgz" []
        ]
        [ ("created", publishedDaysAgo 100)
        , ("1.0.0", publishedDaysAgo 30)
        , ("2.0.0", publishedDaysAgo 1)
        , ("modified", publishedDaysAgo 0)
        ]

unparseableSurvivorPackument :: ByteString
unparseableSurvivorPackument =
    encodePackument
        "thing"
        Nothing
        [("latest", "banana")]
        [ versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        , versionLit "thing" "banana" "https://upstream.test/thing/-/thing-banana.tgz" []
        ]
        [("1.0.0", publishedDaysAgo 30), ("banana", publishedDaysAgo 30)]

survivorWithExtras :: ByteString
survivorWithExtras =
    encodePackument
        "thing"
        (Just [("_id", "\"thing\"")])
        [("latest", "1.0.0")]
        [versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" [("customField", "\"kept\""), ("dist-extra-marker", "true")]]
        [("1.0.0", publishedDaysAgo 30)]

traversalNamePackument :: ByteString
traversalNamePackument =
    encodeUtf8
        ( "{\"name\":\"../evil\",\"dist-tags\":{\"latest\":\"1.0.0\"},"
            <> "\"versions\":{\"1.0.0\":{\"name\":\"../evil\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

controlCharNamePackument :: ByteString
controlCharNamePackument =
    encodeUtf8
        ( "{\"name\":\"th\\u0001ing\",\"dist-tags\":{\"latest\":\"1.0.0\"},"
            <> "\"versions\":{\"1.0.0\":{\"name\":\"th\\u0001ing\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

noDistTagsPackument :: ByteString
noDistTagsPackument =
    encodeUtf8
        ( "{\"name\":\"thing\","
            <> "\"versions\":{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

nullDistTagsPackument :: ByteString
nullDistTagsPackument =
    encodeUtf8
        ( "{\"name\":\"thing\",\"dist-tags\":null,"
            <> "\"versions\":{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

encodePackument ::
    Text ->
    Maybe [(Text, Text)] ->
    [(Text, Text)] ->
    [(Text, Text)] ->
    [(Text, Text)] ->
    ByteString
encodePackument name extras tags versions times =
    encodeUtf8 $
        "{"
            <> field "name" (quoted name)
            <> ", \"dist-tags\":"
            <> obj tags quoted
            <> ", \"versions\":"
            <> objRaw versions
            <> ", \"time\":"
            <> obj times quoted
            <> maybe "" (\es -> ", " <> rawPairs es) extras
            <> "}"
  where
    -- a `dist-tags`/`time`-style object: string keys to a rendered value
    obj :: [(Text, Text)] -> (Text -> Text) -> Text
    obj kvs render = "{" <> T.intercalate ", " [quoted k <> ":" <> render v | (k, v) <- kvs] <> "}"
    -- a `versions`-style object whose values are pre-rendered object literals
    objRaw :: [(Text, Text)] -> Text
    objRaw kvs = "{" <> T.intercalate ", " [quoted k <> ":" <> v | (k, v) <- kvs] <> "}"
    rawPairs :: [(Text, Text)] -> Text
    rawPairs kvs = T.intercalate ", " [quoted k <> ":" <> v | (k, v) <- kvs]
    field :: Text -> Text -> Text
    field k v = quoted k <> ":" <> v

versionLit :: Text -> Text -> Text -> [(Text, Text)] -> (Text, Text)
versionLit name ver tarball extras =
    ( ver
    , "{"
        <> quoted "name"
        <> ":"
        <> quoted name
        <> ", "
        <> quoted "version"
        <> ":"
        <> quoted ver
        <> ", "
        <> quoted "dist"
        <> ":{"
        <> quoted "tarball"
        <> ":"
        <> quoted tarball
        <> distExtra
        <> "}"
        <> versionExtras
        <> "}"
    )
  where
    versionExtras =
        mconcat [", " <> quoted k <> ":" <> v | (k, v) <- extras, k /= "dist-extra-marker"]
    distExtra
        | any ((== "dist-extra-marker") . fst) extras = ", " <> quoted "fileCount" <> ":7"
        | otherwise = ""

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

data PackumentSpec = PackumentSpec
    { specName :: Text
    , specVersions :: [(Text, Integer)]
    -- ^ (version string, age in days)
    }
    deriving stock (Show)

deniedVersions :: PackumentSpec -> Set Text
deniedVersions = Set.fromList . map fst . filter ((< 7) . snd) . specVersions

genPackumentSpec :: Gen PackumentSpec
genPackumentSpec = do
    name <- Gen.element ["thing", "@myorg/thing", "left-pad", "core-js"]
    n <- Gen.int (Range.linear 0 5)
    let versionStrings = take n ["1.0.0", "1.1.0", "2.0.0", "2.1.3", "3.0.0", "10.0.0"]
    ages <- forM versionStrings (const (Gen.integral (Range.linear 0 60)))
    pure (PackumentSpec name (zip versionStrings ages))

genBase :: Gen Text
genBase =
    Gen.element
        [ "https://proxy.test/npm"
        , "https://proxy.test/npm/"
        , "https://r.internal.example.com"
        ]

renderPackument :: PackumentSpec -> ByteString
renderPackument (PackumentSpec name versions) =
    encodePackument
        name
        Nothing
        latestTag
        [versionLit name ver (upstreamTarball name ver) [] | (ver, _) <- versions]
        [(ver, publishedDaysAgo age) | (ver, age) <- versions]
  where
    -- Aim @latest@ at the first version, which may or may not survive, so the fixture
    -- drives repointing. The choice of which version does not matter.
    latestTag = case versions of
        ((ver, _) : _) -> [("latest", ver)]
        [] -> []

upstreamTarball :: Text -> Text -> Text
upstreamTarball name ver = "https://upstream.test/" <> name <> "/-/" <> baseName name <> "-" <> ver <> ".tgz"
  where
    baseName n = snd (T.breakOnEnd "/" n)

loadPackument :: ByteString -> IO (PackageInfo, Value)
loadPackument bs = do
    v <- decodeJsonOrFail bs
    info <- either (\e -> fail ("unexpected projection failure: " <> show e)) (pure . fst) (projectNpmManifest defaultLimits (NpmFixture.documentName v) bs)
    pure (info, v)

data AssembleResult
    = Assembled Value
    | NoSurvivors [Decision]
    deriving stock (Eq, Show)

applyToAt :: Text -> EvalContext -> [PrecededRule] -> PackageInfo -> Value -> IO AssembleResult
applyToAt mountBase c rules info value = do
    plan <- filterPlan inertRuleDeps c rules info
    pure $
        if Set.null (fpSurvivors plan)
            then NoSurvivors (fpDecisions plan)
            else case mergePackuments [(GatedSource, restrictToSurvivors (fpSurvivors plan) info <$ jsonSnapshot value)] of
                Just merged
                    | not (Map.null (mpSurvivors merged)) ->
                        Assembled (assembleMergedPackument mountBase (Map.singleton 0 (jsonSnapshot value)) merged value)
                _ -> NoSurvivors (fpDecisions plan)

applyTo :: EvalContext -> [PrecededRule] -> PackageInfo -> Value -> IO AssembleResult
applyTo = applyToAt base

tarballUnderName :: ByteString -> IO (Maybe Text)
tarballUnderName body = do
    (info, _) <- loadPackument oneVersionPackument
    unprojectable <- decodeJsonOrFail body
    applyTo ctx quarantine info unprojectable >>= \case
        Assembled out -> pure (tarballAt "1.0.0" out)
        NoSurvivors _ -> fail "expected survivors, got NoSurvivors"

filterTo :: ByteString -> IO FilteredPackument
filterTo bs = do
    (info, v) <- loadPackument bs
    applyTo ctx quarantine info v >>= \case
        Assembled out -> pure (FilteredPackument (asObject out))
        NoSurvivors _ -> fail "expected survivors, got NoSurvivors"

newtype FilteredPackument = FilteredPackument {rawObject :: KeyMap Value}

versionsOf :: FilteredPackument -> Map Text Value
versionsOf = mapAt "versions" . rawObject

timeKeysOf :: FilteredPackument -> Map Text Value
timeKeysOf = mapAt "time" . rawObject

distTag :: Text -> FilteredPackument -> Maybe Text
distTag tag = textAt (Key.fromText tag) . objectAt "dist-tags" . rawObject

decodeOrFail :: ByteString -> H.PropertyT IO Value
decodeOrFail bs = either (\e -> annotateShow e >> failure) pure (eitherDecodeStrict bs)

loadOrFail :: ByteString -> H.PropertyT IO (PackageInfo, Value)
loadOrFail bs = do
    v <- decodeOrFail bs
    info <- either (\e -> annotateShow e >> failure) (pure . fst) (projectNpmManifest defaultLimits (NpmFixture.documentName v) bs)
    pure (info, v)

distTagValues :: KeyMap Value -> [Text]
distTagValues o = [s | String s <- Map.elems (mapAt "dist-tags" o)]

tarballAt :: Text -> Value -> Maybe Text
tarballAt ver v = do
    Object vo <- Map.lookup ver (mapAt "versions" (asObject v))
    Object dist <- KeyMap.lookup "dist" vo
    case KeyMap.lookup "tarball" dist of
        Just (String url) -> Just url
        _ -> Nothing

versionValue :: Text -> [(Text, Text)] -> IO Value
versionValue tarball extras = decodeJsonOrFail (encodeUtf8 (snd (versionLit "thing" "1.0.0" tarball extras)))

versionTarball :: Value -> Maybe Text
versionTarball v = do
    Object dist <- KeyMap.lookup "dist" (asObject v)
    case KeyMap.lookup "tarball" dist of
        Just (String url) -> Just url
        _ -> Nothing

bareDistKey :: Key.Key -> Value -> Maybe Value
bareDistKey key v = KeyMap.lookup key (objectAt "dist" (asObject v))

versionKey :: Text -> Key.Key -> Value -> Maybe Value
versionKey ver key v = do
    Object vo <- Map.lookup ver (mapAt "versions" (asObject v))
    KeyMap.lookup key vo
