-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.Npm.FilterSpec (spec) where

import Control.Exception (evaluate)
import Data.Aeson (Value (Array, Object, String), eitherDecodeStrict)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hedgehog (Gen, annotateShow, assert, failure, forAll, success, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageInfo, PackageName, mkPackageName, mkScope)
import Ecluse.Core.Package.Filter (fpDecisions, fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Npm.Filter (
    assembleMergedPackument,
    rewriteVersion,
 )
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Rules.Decision (
    Decision (Admitted),
    EvalContext (EvalContext),
 )
import Ecluse.Core.Rules.Policy (
    PrecededRule,
    Rule (AllowIfOlderThan),
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Rules (atDefaultPrecedence, filterPlan, inertRuleDeps)

spec :: Spec
spec = do
    rewriteSpec
    filterSpec
    coherenceSpec
    propertiesSpec

-- | A fixed "now" so the age-based admit/deny axis is deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

ctx :: EvalContext
ctx = EvalContext now Nothing

{- | The policy under test: a single 7-day publish-age quarantine. A version is
__approved__ iff its @time@ entry is at least 7 days before 'now', and otherwise
deny-by-default -- so a version's survival is controlled purely by its @time@ in
the fixture, exercising the real rules engine rather than a stub.
-}
quarantine :: [PrecededRule]
quarantine = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]

{- | An ISO-8601 instant @ageDays@ before 'now', as the bare npm @time@ string
(no surrounding quotes -- the literal builders add those), parseable by the
projection's @UTCTime@ decoder.
-}
publishedDaysAgo :: Integer -> Text
publishedDaysAgo ageDays =
    toText (iso8601Show (addUTCTime (negate (fromInteger ageDays * nominalDay)) now))

base :: Text
base = "https://proxy.test/npm"

{- | The @{base}\/{pkg}@ prefix the per-version rewrite cases place tarballs
under, as the assembly derives it for the unscoped fixture package.
-}
thingPrefix :: Text
thingPrefix = base <> "/thing"

rewriteSpec :: Spec
rewriteSpec = describe "rewriteVersion" $ do
    it "rewrites dist.tarball to {prefix}/-/{file}" $ do
        v <- versionValue "https://upstream.test/thing/-/thing-1.0.0.tgz" []
        versionTarball (rewriteVersion thingPrefix v)
            `shouldBe` Just "https://proxy.test/npm/thing/-/thing-1.0.0.tgz"

    it "preserves unmodelled keys on the version and dist objects" $ do
        v <- versionValue "https://upstream.test/thing/-/thing-1.0.0.tgz" [("customField", "\"kept\""), ("dist-extra-marker", "true")]
        let r = rewriteVersion thingPrefix v
        bareVersionKey "customField" r `shouldBe` Just (String "kept")
        bareDistKey "fileCount" r `shouldBe` Just (Aeson.Number 7)

    it "leaves a version with no dist object untouched" $ do
        v <- decodeValue "{\"name\":\"thing\",\"version\":\"1.0.0\"}"
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
        -- 2.0.0 is 1 day old (denied by quarantine); 1.0.0 is 30 days old (approved).
        filtered <- filterTo twoVersions
        Map.keys (versionsOf filtered) `shouldBe` ["1.0.0"]
        Map.keys (timeKeysOf filtered) `shouldBe` ["1.0.0"]

    it "repoints latest down to a surviving version when the chosen latest is denied" $ do
        -- latest upstream points at the denied 2.0.0; keep-unless-denied repoints
        -- it down to the surviving 1.0.0.
        filtered <- filterTo twoVersions
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "keeps a surviving upstream latest rather than promoting a higher survivor" $ do
        -- The whole point of keep-unless-denied: upstream latest is 1.0.0 and both
        -- 1.0.0 and the higher 2.0.0 survive, so latest stays 1.0.0 -- it is never
        -- promoted to the higher surviving version.
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
        topLevelKey "_id" (Object (rawObject filtered)) `shouldBe` Just (String "thing")
        versionKey "1.0.0" "customField" (Object (rawObject filtered)) `shouldBe` Just (String "kept")

    it "drops a denied version from time but keeps created/modified bookkeeping" $ do
        -- `time` carries npm's unmodelled `created`/`modified` keys alongside the
        -- per-version timestamps; only the denied 2.0.0 entry must go.
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
        -- The pipeline never hands a non-object here (a non-object body fails
        -- projection and contributes nothing), but the assembly is total: a
        -- non-object base relays no top-level keys and no version objects, so the
        -- result is an object of exactly the plan-owned keys, none fabricated.
        (info, _) <- loadPackument oneVersionPackument
        applyTo ctx quarantine info (Array mempty) >>= \case
            NoSurvivors _ -> expectationFailure "expected an assembled document"
            Assembled out -> do
                Map.keys (objKeys "versions" (asObject out)) `shouldBe` []
                sort (map Key.toText (KeyMap.keys (asObject out))) `shouldBe` ["dist-tags", "time", "versions"]

    it "rewrites a surviving version's dist.tarball under the mount base in the assembly pass" $ do
        -- The rewrite is fused into the assembly (one pass over the versions), so
        -- the assembled document already carries {base}/{pkg}/-/{file}.
        filtered <- filterTo twoVersions
        tarballAt "1.0.0" (Object (rawObject filtered))
            `shouldBe` Just "https://proxy.test/npm/thing/-/thing-1.0.0.tgz"

    it "rewrites a scoped survivor under {base}/@scope/name/-/{file} in the assembly pass" $ do
        -- The prefix embeds the scoped @scope/name form npm uses in URLs; the
        -- scope separator must survive the component-safety gate.
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

    it "leaves a tarball untouched when the document's name carries a traversal" $ do
        -- The fused rewrite gates the upstream-controlled name component-wise:
        -- an unsafe name is never interpolated, so the upstream URL is relayed
        -- unrewritten.
        filtered <- filterTo traversalNamePackument
        tarballAt "1.0.0" (Object (rawObject filtered))
            `shouldBe` Just "https://upstream.test/thing/-/thing-1.0.0.tgz"

    it "leaves a tarball untouched when the document's name carries a control character" $ do
        filtered <- filterTo controlCharNamePackument
        tarballAt "1.0.0" (Object (rawObject filtered))
            `shouldBe` Just "https://upstream.test/thing/-/thing-1.0.0.tgz"

    it "drops a version broken in a required field from the served body, keeping the healthy one" $ do
        -- End-to-end version-level graceful degradation: 2.0.0's `dist` is a scalar (a
        -- malformed required field) yet it is 30 days old, so it would clear the
        -- quarantine if it decoded. It is absent from the served versions/time purely
        -- because the decode dropped it from the decision surface -- a healthy package
        -- keeps serving its good versions despite one poisoned one.
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
        -- `dist-tags: null` passes projection (read as absent) but the raw body
        -- still carries the null; without repair it would ship with no resolvable
        -- latest. Coherence outranks relaying the malformed shape.
        filtered <- filterTo nullDistTagsPackument
        distTag "latest" filtered `shouldBe` Just "1.0.0"

    it "keeps an admitted but unparseable-version key and still resolves a present latest" $ do
        -- `banana` is not parseable semver, so `compareVersions` against it yields
        -- Nothing; it is old enough to survive the quarantine, exercising the
        -- unorderable-version path while coherence (a present latest) must hold.
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
            let p = T.dropWhileEnd (== '/') b <> "/" <> specName spec'
                versions = objKeys "versions" (asObject v)
                once = fmap (rewriteVersion p) versions
            fmap (rewriteVersion p) once === once

    it "every served version's tarball is rewritten under {base}/{pkg}/-/" $
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            (info, v) <- loadOrFail (renderPackument spec')
            b <- forAll genBase
            let prefix = T.dropWhileEnd (== '/') b <> "/" <> specName spec' <> "/-/"
            liftIO (applyToAt b ctx quarantine info v) >>= \case
                NoSurvivors _ -> success
                Assembled out ->
                    forM_ (Map.keys (objKeys "versions" (asObject out))) $ \ver ->
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
                        survivingKeys = Map.keysSet (objKeys "versions" o)
                        timeKeys = Map.keysSet (objKeys "time" o)
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
                        survivingKeys = Map.keysSet (objKeys "versions" o)
                    case lookupTag "latest" o of
                        Just l -> assert (Set.member l survivingKeys)
                        Nothing -> annotateShow out >> failure

    it "the assembled document forces deeply without bottoming (the pdAssemble never-throws contract)" $
        -- The serve tail feeds the assembled document straight into the encoder,
        -- so a lurking bottom in any branch would surface as a request-perimeter
        -- escape at serve time. Force the whole 'Value' ('NFData') here instead,
        -- across generated documents, so the contract is pinned where the
        -- assembly logic lives.
        hedgehog $ do
            spec' <- forAll genPackumentSpec
            (info, v) <- loadOrFail (renderPackument spec')
            liftIO (applyTo ctx quarantine info v) >>= \case
                NoSurvivors _ -> success
                Assembled out -> do
                    _ <- liftIO (evaluate (force out))
                    success

-- | One unscoped version, published 30 days ago (survives the quarantine).
oneVersionPackument :: ByteString
oneVersionPackument =
    encodePackument
        "thing"
        Nothing
        [("latest", "1.0.0")]
        [versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" []]
        [("1.0.0", publishedDaysAgo 30)]

-- | A scoped package, one surviving version.
scopedPackument :: ByteString
scopedPackument =
    encodePackument
        "@myorg/thing"
        Nothing
        [("latest", "1.0.0")]
        [versionLit "@myorg/thing" "1.0.0" "https://upstream.test/@myorg/thing/-/thing-1.0.0.tgz" []]
        [("1.0.0", publishedDaysAgo 30)]

{- | 1.0.0 published 30 days ago (survives) and 2.0.0 published 1 day ago (denied
by the quarantine); @latest@ upstream aims at the denied 2.0.0.
-}
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

{- | Both versions survive, and @latest@ aims at the /lower/ 1.0.0 (the
maintainer's chosen release). Under keep-unless-denied, @latest@ must stay 1.0.0
even though the higher 2.0.0 also survives -- a surviving @latest@ is never
promoted to the higher survivor.
-}
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

-- | As 'twoVersions', but with a `beta` tag also aimed at the denied 2.0.0.
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

-- | As 'twoVersions', but with a `stable` tag aimed at the surviving 1.0.0.
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

{- | A healthy 1.0.0 alongside a 2.0.0 whose @dist@ is a scalar (a malformed
required field). Both are 30 days old, so the broken one would clear the
quarantine if it decoded -- proving its absence from the served body is the
__decode__ dropping it from the decision surface, not the age policy. The broken
version's object literal is supplied raw (the 'versionLit' builder only makes
well-formed versions).
-}
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

-- | Both versions too young: nothing survives.
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

{- | As 'twoVersions', but the @time@ object also carries npm's @created@ /
@modified@ bookkeeping keys, which are unmodelled and must survive filtering.
-}
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

{- | A package with a surviving version whose key is not parseable semver. npm
accepts arbitrary version-key strings; the strict parser yields an unorderable
'Version', so ranking @latest@ goes through the @compareVersions@-returns-Nothing
path. @banana@ is old enough to clear the quarantine, so it survives.
-}
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

-- | A surviving single version carrying extra (unmodelled) keys.
survivorWithExtras :: ByteString
survivorWithExtras = packumentWithExtras

-- | A packument with a top-level @_id@ extra, a version extra, and a dist extra.
packumentWithExtras :: ByteString
packumentWithExtras =
    encodePackument
        "thing"
        (Just [("_id", "\"thing\"")])
        [("latest", "1.0.0")]
        [versionLit "thing" "1.0.0" "https://upstream.test/thing/-/thing-1.0.0.tgz" [("customField", "\"kept\""), ("dist-extra-marker", "true")]]
        [("1.0.0", publishedDaysAgo 30)]

{- | A packument whose (upstream-controlled) @name@ carries an embedded slash and
@..@ traversal. A raw interpolation would aim the rewritten @dist.tarball@ outside
the package's own path, so the rewrite must leave the version's tarball untouched.
-}
traversalNamePackument :: ByteString
traversalNamePackument =
    encode
        ( "{\"name\":\"../evil\",\"dist-tags\":{\"latest\":\"1.0.0\"},"
            <> "\"versions\":{\"1.0.0\":{\"name\":\"../evil\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

{- | A packument whose (upstream-controlled) @name@ carries a control character
(a literal @\\u0001@), which the component-safety gate rejects -- so the rewrite
leaves the version's tarball untouched rather than interpolating it.
-}
controlCharNamePackument :: ByteString
controlCharNamePackument =
    encode
        ( "{\"name\":\"th\\u0001ing\",\"dist-tags\":{\"latest\":\"1.0.0\"},"
            <> "\"versions\":{\"1.0.0\":{\"name\":\"th\\u0001ing\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

-- | A packument with no @dist-tags@ object at all (a malformed-upstream edge).
noDistTagsPackument :: ByteString
noDistTagsPackument =
    encode
        ( "{\"name\":\"thing\","
            <> "\"versions\":{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

{- | A packument whose @dist-tags@ is JSON @null@ -- the common malformed shape.
The projection's @.:?@ reads it as absent, but the raw body still carries the
null, so filtering must repair it rather than relay an unresolvable @latest@.
-}
nullDistTagsPackument :: ByteString
nullDistTagsPackument =
    encode
        ( "{\"name\":\"thing\",\"dist-tags\":null,"
            <> "\"versions\":{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\","
            <> "\"dist\":{\"tarball\":\"https://upstream.test/thing/-/thing-1.0.0.tgz\"}}},"
            <> "\"time\":{\"1.0.0\":\""
            <> publishedDaysAgo 30
            <> "\"}}"
        )

{- | Build a JSON packument literal from its parts. @extras@ are extra top-level
key/raw-JSON pairs; each version is a pre-rendered object literal.
-}
encodePackument ::
    Text ->
    Maybe [(Text, Text)] ->
    [(Text, Text)] ->
    [(Text, Text)] ->
    [(Text, Text)] ->
    ByteString
encodePackument name extras tags versions times =
    encode $
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

{- | Render one version entry as a @(versionKey, objectLiteral)@ pair: the object
carries name, version, a @dist.tarball@, plus any extra raw key/JSON pairs (some
placed on the version, the @dist-extra-marker@ moved into @dist@ to exercise
dist-level passthrough).
-}
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

{- | A generated packument's logical spec: a name and a list of versions, each
with a publish age in days. Survival is derived from the age against 'quarantine'.
-}
data PackumentSpec = PackumentSpec
    { specName :: Text
    , specVersions :: [(Text, Integer)]
    -- ^ (version string, age in days)
    }
    deriving stock (Show)

-- | The versions a generated spec's quarantine would deny (age < 7 days).
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

{- | Render a 'PackumentSpec' to JSON bytes, with @latest@ aimed at the last
version (which may or may not survive -- exercising repointing) when any exist.
-}
renderPackument :: PackumentSpec -> ByteString
renderPackument (PackumentSpec name versions) =
    encodePackument
        name
        Nothing
        latestTag
        [versionLit name ver (upstreamTarball name ver) [] | (ver, _) <- versions]
        [(ver, publishedDaysAgo age) | (ver, age) <- versions]
  where
    -- Aim @latest@ at the first version (which may or may not survive), so
    -- repointing is exercised; the choice of which version is irrelevant.
    latestTag = case versions of
        ((ver, _) : _) -> [("latest", ver)]
        [] -> []

upstreamTarball :: Text -> Text -> Text
upstreamTarball name ver = "https://upstream.test/" <> name <> "/-/" <> baseName name <> "-" <> ver <> ".tgz"
  where
    baseName n = snd (T.breakOnEnd "/" n)

decodeValue :: ByteString -> IO Value
decodeValue bs = either (fail . ("decode failure: " <>)) pure (eitherDecodeStrict bs)

{- | The route-requested 'PackageName' for projecting a fixture: the body's /own/
self-reported top-level @name@, so the projection's name validation is a guaranteed
pass and these tests exercise filtering, not name validation (which has its own
suite). Mirrors the npm scope split the projection performs.
-}
fixtureName :: Value -> PackageName
fixtureName v = npmName (nameOf v)
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

{- | Project a fixture through the live serve projection ('projectNpmManifest') and hold
onto the decoded 'Value' too, so the 'PackageInfo' and the 'Value' the assembly edits
agree (they are the same parse). The requested name is the body's own self-reported name,
so name validation is a guaranteed pass and these tests exercise filtering, not it.
-}
loadPackument :: ByteString -> IO (PackageInfo, Value)
loadPackument bs = do
    v <- decodeValue bs
    info <- either (\e -> fail ("unexpected projection failure: " <> show e)) (pure . fst) (projectNpmManifest defaultLimits (fixtureName v) bs)
    pure (info, v)

{- | The outcome of the serve composition under test: the assembled served document
when survivors remain, or every version's decision when none do -- the shape the
serve layer branches on.
-}
data AssembleResult
    = Assembled Value
    | NoSurvivors [Decision]
    deriving stock (Eq, Show)

{- | Decide the plan ('Ecluse.Test.Rules.filterPlan') over the typed view, merge
the gated survivor set, and assemble the plan onto the raw body under the given
mount base -- the composition the serve layer performs for a single public origin.
The tarball rewrite is fused into the assembly, so the result already carries
mount-based URLs.
-}
applyToAt :: Text -> EvalContext -> [PrecededRule] -> PackageInfo -> Value -> IO AssembleResult
applyToAt mountBase c rules info value = do
    plan <- filterPlan inertRuleDeps c rules info
    pure $
        if Set.null (fpSurvivors plan)
            then NoSurvivors (fpDecisions plan)
            else case mergePackuments [(GatedSource, restrictToSurvivors (fpSurvivors plan) info)] of
                Just merged
                    | not (Map.null (mpSurvivors merged)) ->
                        Assembled (assembleMergedPackument mountBase (Map.singleton 0 value) merged value)
                _ -> NoSurvivors (fpDecisions plan)

-- | 'applyToAt' under the canonical fixture mount base.
applyTo :: EvalContext -> [PrecededRule] -> PackageInfo -> Value -> IO AssembleResult
applyTo = applyToAt base

-- | Assemble a fixture body, requiring survivors; returns the served packument.
filterTo :: ByteString -> IO FilteredPackument
filterTo bs = do
    (info, v) <- loadPackument bs
    applyTo ctx quarantine info v >>= \case
        Assembled out -> pure (FilteredPackument (asObject out))
        NoSurvivors _ -> fail "expected survivors, got NoSurvivors"

-- | A served packument as its top-level object, for read-back assertions.
newtype FilteredPackument = FilteredPackument {rawObject :: KeyMap Value}

versionsOf :: FilteredPackument -> Map Text Value
versionsOf = objKeys "versions" . rawObject

timeKeysOf :: FilteredPackument -> Map Text Value
timeKeysOf = objKeys "time" . rawObject

distTag :: Text -> FilteredPackument -> Maybe Text
distTag tag = lookupTag tag . rawObject

isApproved :: Decision -> Bool
isApproved = \case
    Admitted{} -> True
    _ -> False

decodeOrFail :: ByteString -> H.PropertyT IO Value
decodeOrFail bs = either (\e -> annotateShow e >> failure) pure (eitherDecodeStrict bs)

loadOrFail :: ByteString -> H.PropertyT IO (PackageInfo, Value)
loadOrFail bs = do
    v <- decodeOrFail bs
    info <- either (\e -> annotateShow e >> failure) (pure . fst) (projectNpmManifest defaultLimits (fixtureName v) bs)
    pure (info, v)

asObject :: Value -> KeyMap Value
asObject = \case
    Object o -> o
    _ -> KeyMap.empty

-- | The object at @key@ as a 'Map' from string key to value (empty if absent).
objKeys :: Key.Key -> KeyMap Value -> Map Text Value
objKeys key o = case KeyMap.lookup key o of
    Just (Object inner) -> Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList inner]
    _ -> Map.empty

-- | The @dist-tags@ value for @tag@ as text (if a string).
lookupTag :: Text -> KeyMap Value -> Maybe Text
lookupTag tag o = case Map.lookup tag (objKeys "dist-tags" o) of
    Just (String s) -> Just s
    _ -> Nothing

-- | Every string-valued @dist-tags@ target.
distTagValues :: KeyMap Value -> [Text]
distTagValues o = [s | String s <- Map.elems (objKeys "dist-tags" o)]

-- | The rewritten tarball URL of a version, if present.
tarballAt :: Text -> Value -> Maybe Text
tarballAt ver v = do
    Object vo <- Map.lookup ver (objKeys "versions" (asObject v))
    Object dist <- KeyMap.lookup "dist" vo
    case KeyMap.lookup "tarball" dist of
        Just (String url) -> Just url
        _ -> Nothing

{- | A bare version-object 'Value' -- the versions-map entry 'rewriteVersion'
operates on -- with the fixture name\/version and the given tarball URL and extras.
-}
versionValue :: Text -> [(Text, Text)] -> IO Value
versionValue tarball extras = decodeValue (encode (snd (versionLit "thing" "1.0.0" tarball extras)))

-- | The @dist.tarball@ of a bare version object.
versionTarball :: Value -> Maybe Text
versionTarball v = do
    Object dist <- KeyMap.lookup "dist" (asObject v)
    case KeyMap.lookup "tarball" dist of
        Just (String url) -> Just url
        _ -> Nothing

-- | A top-level key of a bare version object.
bareVersionKey :: Key.Key -> Value -> Maybe Value
bareVersionKey key v = KeyMap.lookup key (asObject v)

-- | A key of a bare version object's @dist@.
bareDistKey :: Key.Key -> Value -> Maybe Value
bareDistKey key v = do
    Object dist <- KeyMap.lookup "dist" (asObject v)
    KeyMap.lookup key dist

versionKey :: Text -> Key.Key -> Value -> Maybe Value
versionKey ver key v = do
    Object vo <- Map.lookup ver (objKeys "versions" (asObject v))
    KeyMap.lookup key vo

topLevelKey :: Key.Key -> Value -> Maybe Value
topLevelKey key v = KeyMap.lookup key (asObject v)

encode :: Text -> ByteString
encode = encodeUtf8
