-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.VersionSpec (spec) where

import Data.Text qualified as T
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Version
import Ecluse.Test.Version (genGem, genNpm, genPyPI)

spec :: Spec
spec = do
    describe "mkVersion" $ do
        it "round-trips the raw text through renderVersion" $
            hedgehog $ do
                v <- forAll (Gen.text (Range.linear 1 12) Gen.ascii)
                renderVersion (mkVersion Npm v) === v
        it "keeps the raw text even when unparseable (proxy fidelity)" $
            renderVersion (mkVersion PyPI "totally bogus") `shouldBe` "totally bogus"
        it "has no key for unparseable input" $
            versionKey (mkVersion PyPI "totally bogus") `shouldBe` Nothing
        it "parses a valid version into a key" $
            versionKey (mkVersion Npm "1.2.3") `shouldSatisfy` isJust

    describe "parseVersionKey" $
        for_ parseableEcosystems $ \(eco, valid, invalid, refusalName, gen) ->
            describe (show eco) $ do
                it ("parses a valid " <> show eco <> " version") $
                    parseVersionKey eco valid `shouldSatisfy` isRight
                it refusalName $
                    parseVersionKey eco invalid `shouldSatisfy` isLeft
                it ("successfully parses generated valid " <> show eco <> " versions") $
                    hedgehog $ do
                        v <- forAll gen
                        assert (isRight (parseVersionKey eco v))

    -- Strictness: the ordering fixture can only rank, so these cases assert rejection (Left)
    -- explicitly and pin the valid spellings that must keep parsing (Right).
    describe "parser strictness (#279, #280)" $ do
        let mustReject eco raw =
                it (show eco <> " rejects " <> show raw) $
                    parseVersionKey eco raw `shouldSatisfy` isLeft
            mustParse eco raw =
                it (show eco <> " parses " <> show raw) $
                    parseVersionKey eco raw `shouldSatisfy` isRight

        describe "PEP 440 empty release segments (#279)" $ do
            -- The parser rejects an interior or leading empty segment. Only the
            -- single trailing release/suffix separator dot may be empty.
            mustReject PyPI "1..0"
            mustReject PyPI ".1.0"
            mustReject PyPI "1.0..dev1"
            -- The dev separator's dot lands in the release text as a legitimate
            -- single trailing empty.
            mustParse PyPI "1.0.dev1"
            -- A bare trailing dot still normalises.
            mustParse PyPI "1.0."

        describe "PEP 440 'r' post-release spelling" $ do
            -- PEP 440 spells the post-release label post, rev, or r, and packaging normalises all
            -- three to post. A rejection would abstain from ordering and let selectLatest repoint
            -- dist-tags.latest past the version.
            mustParse PyPI "1.0.r1"
            mustParse PyPI "1.0r1"
            mustParse PyPI "1.0-r1"
            -- A bare 'r' with no number is post0, exactly as for 'post'/'rev'.
            mustParse PyPI "1.0.r"

        describe "non-ASCII alphanumerics (#280)" $ do
            -- Python's packaging and Ruby's Gem::Version are ASCII-only. A Unicode-aware gate
            -- over-accepts and mis-classifies non-ASCII digits as text, corrupting the order.
            mustReject PyPI "1.0+café" -- Latin-1 letter in a local segment
            mustReject PyPI "１.２.３" -- fullwidth digits
            mustReject PyPI "١.٢.٣" -- Arabic-Indic digits
            mustReject PyPI "1.0²" -- superscript two (a Unicode "number")
            mustReject RubyGems "１.２.３" -- fullwidth digits
            mustReject RubyGems "١.٢.٣" -- Arabic-Indic digits
        describe "numeric run length bound (DoS)" $ do
            -- Reading a numeric segment into an 'Integer' is quadratic in the digit count, so an
            -- unbounded run is a DoS. An over-long version is refused and served raw, unkeyed.
            mustReject PyPI ("1." <> T.replicate 5000 "9")
            mustReject RubyGems ("1." <> T.replicate 5000 "9")
            mustParse PyPI ("1." <> T.replicate 100 "9")
            mustParse RubyGems ("1." <> T.replicate 100 "9")
            -- The @versions@ library keys npm numeric components as fixed-width words, not
            -- 'Integer'. Beyond the shared length bound the parser refuses, so a silent overflow (a
            -- 25-digit major wrapping mod 2^64) cannot key a huge version as a small one.
            mustReject Npm ("1.0." <> T.replicate 5000 "9")
            mustReject Npm (T.replicate 25 "9" <> ".0.0")
            mustParse Npm "1.2.3"
            mustParse Npm ("1.0." <> T.replicate 15 "9")

    -- The differential check against node-semver, Python packaging, and Ruby Gem::Version.
    -- Regenerate the fixture with scripts/gen-version-fixtures.sh: it keeps this test offline.
    rows <- runIO loadFixture
    describe "compareVersions agrees with the reference oracles" $ do
        it "the fixture is populated" $
            length rows `shouldSatisfy` (> 100)
        for_ [Npm, PyPI, RubyGems] $ \eco ->
            it (show eco <> " ordering matches the reference") $
                mismatches (filter (\(Row e _ _ _) -> e == eco) rows) `shouldBe` []

    describe "compareVersions" $ do
        let cmp eco a b = compareVersions (mkVersion eco a) (mkVersion eco b)
        it "npm orders release numbers numerically (10 > 9)" $
            cmp Npm "1.10.0" "1.9.0" `shouldBe` Just GT
        it "npm ranks a prerelease below its release" $
            cmp Npm "1.0.0-rc.1" "1.0.0" `shouldBe` Just LT
        it "npm ranks a numeric prerelease id below an alphanumeric one" $
            cmp Npm "1.0.0-1" "1.0.0-alpha" `shouldBe` Just LT
        it "npm ranks more prerelease fields above fewer" $
            cmp Npm "1.0.0-alpha" "1.0.0-alpha.1" `shouldBe` Just LT
        it "PyPI treats trailing zeros as equal (1.0 == 1.0.0)" $
            cmp PyPI "1.0" "1.0.0" `shouldBe` Just EQ
        it "PyPI ranks a dev release below the final" $
            cmp PyPI "1.0.dev1" "1.0" `shouldBe` Just LT
        it "PyPI ranks a prerelease below the final" $
            cmp PyPI "1.0a1" "1.0" `shouldBe` Just LT
        it "PyPI ranks a post-release above the final" $
            cmp PyPI "1.0.post1" "1.0" `shouldBe` Just GT
        it "PyPI normalises the 'r' post-release spelling (1.0.r1 == 1.0.post1)" $
            cmp PyPI "1.0.r1" "1.0.post1" `shouldBe` Just EQ
        it "PyPI normalises the separatorless 'r' spelling (1.0r1 == 1.0.post1)" $
            cmp PyPI "1.0r1" "1.0.post1" `shouldBe` Just EQ
        it "PyPI still reads 'rev' as post, not r+ev (1.0.rev1 == 1.0.post1)" $
            cmp PyPI "1.0.rev1" "1.0.post1" `shouldBe` Just EQ
        it "PyPI canonicalises a non-normalised spelling (1.0ALPHA1 == 1.0a1)" $
            cmp PyPI "1.0ALPHA1" "1.0a1" `shouldBe` Just EQ
        it "RubyGems ranks a letter (prerelease) segment below the release" $
            cmp RubyGems "1.0.0.beta1" "1.0.0" `shouldBe` Just LT
        it "RubyGems orders numeric segments numerically" $
            cmp RubyGems "1.10.0" "1.9.0" `shouldBe` Just GT
        -- Gem::Version#canonical_segments drops a release trailing zero before the prerelease, so
        -- 2.0.a keys as [2,"a"].
        it "RubyGems canonicalises a release trailing zero before a prerelease (2.t > 2.0.a)" $
            cmp RubyGems "2.t" "2.0.a" `shouldBe` Just GT
        it "RubyGems equates versions that canonicalise alike (2.0.a == 2.a)" $
            cmp RubyGems "2.0.a" "2.a" `shouldBe` Just EQ
        it "RubyGems strips a release trailing zero (2.0 == 2)" $
            cmp RubyGems "2.0" "2" `shouldBe` Just EQ
        -- Gem::Version canonicalises hyphens to a prerelease marker (a global gsub("-", ".pre.")),
        -- so "1.0.0-1" parses as "1.0.0.pre.1".
        it "RubyGems accepts a hyphenated version (1.0.0-1 parses)" $
            parseVersionKey RubyGems "1.0.0-1" `shouldSatisfy` isRight
        it "RubyGems ranks a hyphenated version below its release (1.0.0-1 < 1.0.0)" $
            cmp RubyGems "1.0.0-1" "1.0.0" `shouldBe` Just LT
        it "RubyGems equates a hyphen with the .pre. spelling (1.0.0-1 == 1.0.0.pre.1)" $
            cmp RubyGems "1.0.0-1" "1.0.0.pre.1" `shouldBe` Just EQ
        it "is Nothing when a version cannot be parsed" $
            cmp Npm "not a version" "1.0.0" `shouldBe` Nothing
        it "is reflexive -- EQ when parseable, Nothing otherwise" $
            hedgehog $ do
                eco <- forAll (Gen.element [Npm, PyPI, RubyGems])
                ver <-
                    forAll
                        ( Gen.text
                            (Range.linear 1 12)
                            (Gen.element ('.' : '-' : ['0' .. '9'] <> "abrcdevpost"))
                        )
                let x = mkVersion eco ver
                compareVersions x x === (EQ <$ versionKey x)

    -- The total-order laws on 'compareVersions', over structurally valid version strings so each
    -- side parses. 'versionPair' and 'versionTriple' reuse one raw to keep the EQ class covered.
    describe "compareVersions total-order laws" $
        modifyMaxSuccess (const 400) $
            for_ ecosystemGens $ \(eco, gen) -> describe (show eco) $ do
                it "totality -- both parse ⇒ Just (never Nothing)" $
                    hedgehog $ do
                        (a, b) <- forAll (versionPair gen)
                        let (x, y) = (mkVersion eco a, mkVersion eco b)
                        -- The generators stay structurally valid. Guard so a
                        -- generator gap surfaces as totality, not noise.
                        H.assert (isJust (versionKey x))
                        H.assert (isJust (versionKey y))
                        -- Non-vacuity: 'versionPair' guarantees a healthy fraction
                        -- of equal pairs (EQ), and its independent draws supply
                        -- LT/GT, so all three orderings stay populated.
                        H.cover 1 "LT" (compareVersions x y == Just LT)
                        H.cover 1 "EQ" (compareVersions x y == Just EQ)
                        H.cover 1 "GT" (compareVersions x y == Just GT)
                        H.assert (isJust (compareVersions x y))
                it "antisymmetry -- cmp x y == invert <$> cmp y x" $
                    hedgehog $ do
                        (a, b) <- forAll (versionPair gen)
                        let (x, y) = (mkVersion eco a, mkVersion eco b)
                        compareVersions x y === fmap invertOrdering (compareVersions y x)
                it "transitivity -- x ≤ y and y ≤ z ⇒ x ≤ z" $
                    hedgehog $ do
                        (a, b, c) <- forAll (versionTriple gen)
                        let x = mkVersion eco a
                            y = mkVersion eco b
                            z = mkVersion eco c
                        -- All three parse, since the generators stay valid. Guard
                        -- so a generator gap can't make ≤ vacuously true via Nothing.
                        H.assert (all (isJust . versionKey) [x, y, z])
                        let le p q = compareVersions p q == Just LT || compareVersions p q == Just EQ
                        H.cover 1 "x ≤ y" (le x y)
                        H.cover 1 "x > y" (not (le x y))
                        when (le x y && le y z) (H.assert (le x z))

    describe "isStable" $
        -- Every fixture below parses, so stableOf answers Just True or Just False, never Nothing.
        for_ stabilityGrammars $ \(grammar, eco, cases) ->
            describe grammar $
                for_ cases $ \(name, raw, expected) ->
                    it name $
                        fmap isStable (rightToMaybe (parseVersionKey eco raw)) `shouldBe` Just expected

    describe "canonicalPep440" $ do
        it "spells a release without its trailing zeros" $
            canonicalPep440 "1.0.0" `shouldBe` Just "1"

        it "gives two spellings of one release the same key" $
            canonicalPep440 "1.0" `shouldBe` canonicalPep440 "1.0.0"

        it "normalises an unnormalised prerelease, post-release, dev-release and local segment" $
            canonicalPep440 "1!2.0ALPHA1-1.dev2+Ubuntu.7" `shouldBe` Just "1!2a1.post1.dev2+ubuntu.7"

        it "still spells a release whose every segment is zero" $
            canonicalPep440 "0.0" `shouldBe` Just "0"

        it "has no spelling for text that is not PEP 440" $
            canonicalPep440 "totally bogus" `shouldBe` Nothing

        it "preserves the ordering key, so keying by the spelling keys by the release" $
            -- The PyPI projection keys a release by this spelling. That is only sound while the
            -- spelling and the version it came from carry one ordering key.
            hedgehog $ do
                v <- forAll genPyPI
                (canonicalPep440 v >>= rightToMaybe . parseVersionKey PyPI)
                    === rightToMaybe (parseVersionKey PyPI v)

    describe "selectLatest" $ do
        -- All survivors here are npm versions. selectLatest is ecosystem-agnostic: it calls
        -- compareVersions and isStable on the keys.
        let v = mkVersion Npm
            raws = map renderVersion
            selRaw :: Maybe Text -> [Text] -> Maybe Text
            selRaw chosen survivors =
                renderVersion <$> selectLatest (v <$> chosen) (map v survivors)

        it "returns Nothing when there are no survivors" $
            selRaw (Just "1.0.0") [] `shouldBe` Nothing

        it "keeps the chosen latest when it survives" $
            selRaw (Just "1.2.0") ["1.0.0", "1.2.0", "1.3.0"] `shouldBe` Just "1.2.0"

        it "keeps a stable chosen latest even when a higher prerelease survives" $
            -- Never promotes: npm keeps latest on the last stable release.
            selRaw (Just "1.2.0") ["1.2.0", "2.0.0-rc.1"] `shouldBe` Just "1.2.0"

        it "keeps a surviving prerelease chosen latest (no demotion to a higher stable)" $
            -- Keep is unconditional on survival: a maintainer who tags a prerelease
            -- as latest keeps it, even though a higher stable version survives.
            selRaw (Just "2.0.0-rc.1") ["1.2.0", "2.0.0-rc.1"] `shouldBe` Just "2.0.0-rc.1"

        it "is the identity on a single-version packument" $
            selRaw (Just "1.0.0") ["1.0.0"] `shouldBe` Just "1.0.0"

        it "repoints to the highest stable survivor when the chosen latest is gone" $
            selRaw (Just "2.0.0") ["1.0.0", "1.5.0", "1.3.0"] `shouldBe` Just "1.5.0"

        it "repoints with no chosen latest at all" $
            selRaw Nothing ["1.0.0", "1.5.0", "1.3.0"] `shouldBe` Just "1.5.0"

        it "prefers a stable survivor over a higher prerelease when repointing" $
            selRaw (Just "9.9.9") ["1.0.0", "2.0.0-rc.1"] `shouldBe` Just "1.0.0"

        it "repoints to the highest prerelease only when no stable survives" $
            selRaw (Just "9.9.9") ["2.0.0-rc.1", "2.0.0-rc.2", "1.0.0-beta"]
                `shouldBe` Just "2.0.0-rc.2"

        it "never lets an unparseable version beat a parseable one" $
            -- "garbage" has no key, so the parseable 1.0.0 must win.
            selRaw (Just "9.9.9") ["garbage", "1.0.0"] `shouldBe` Just "1.0.0"

        it "falls back to the lexicographically-smallest survivor when none parse" $
            selRaw (Just "9.9.9") ["zeta", "alpha", "mid"] `shouldBe` Just "alpha"

        it "always returns one of the survivors it was given" $
            hedgehog $ do
                chosenRaw <- forAll (Gen.maybe genRaw)
                survivorRaws <- forAll (Gen.list (Range.linear 0 6) genRaw)
                let survivors = map v survivorRaws
                    result = selectLatest (v <$> chosenRaw) survivors
                case result of
                    Nothing -> survivorRaws === []
                    Just r -> assert (renderVersion r `elem` raws survivors)

{- | Per ecosystem: a version that parses, one that does not, the name its refusal case
carries, and the generator of structurally valid raw versions.
-}
parseableEcosystems :: [(Ecosystem, Text, Text, String, Gen Text)]
parseableEcosystems =
    [ (Npm, "1.2.3", "nope", "returns a VersionError for invalid Npm input", genNpm)
    , (PyPI, "1.2.3", "totally bogus", "returns a VersionError for invalid PyPI input", genPyPI)
    , (RubyGems, "1.2.3", "", "returns a VersionError for unparseable RubyGems input", genGem)
    ]

-- | What each grammar counts as a stable release, and what it counts as a pre-release.
stabilityGrammars :: [(String, Ecosystem, [(String, Text, Bool)])]
stabilityGrammars =
    [
        ( "semver (npm)"
        , Npm
        ,
            [ ("a final release is stable", "1.0.0", True)
            , ("an -rc prerelease is not stable", "1.0.0-rc.1", False)
            , ("a -beta prerelease is not stable", "2.0.0-beta", False)
            , ("a numeric prerelease id is not stable", "1.0.0-1", False)
            ]
        )
    ,
        ( "PEP 440 (PyPI)"
        , PyPI
        ,
            [ ("a final release is stable", "1.0", True)
            , ("a post-release is stable (post is not a prerelease)", "1.0.post1", True)
            , ("an alpha pre-release is not stable", "1.0a1", False)
            , ("an rc pre-release is not stable", "1.0rc1", False)
            , ("a dev release is not stable", "1.0.dev1", False)
            , ("a pre+dev release is not stable", "1.0a1.dev2", False)
            , ("a post+dev release is not stable (dev disqualifies)", "1.0.post1.dev2", False)
            ]
        )
    ,
        ( "RubyGems"
        , RubyGems
        ,
            [ ("an all-numeric version is stable", "1.0.0", True)
            , ("a .pre letter segment is not stable", "1.0.0.pre", False)
            , ("a .rc1 letter segment is not stable", "1.2.0.rc1", False)
            ]
        )
    ]

-- | One fixture comparison: @A@ relative to @B@ is @Ordering@ for @Ecosystem@.
data Row = Row Ecosystem Text Text Ordering
    deriving stock (Eq, Show)

-- | Rows where our comparator disagrees with the fixture, as @(a, b, expected, got)@.
mismatches :: [Row] -> [(Text, Text, Ordering, Maybe Ordering)]
mismatches = mapMaybe check
  where
    check (Row eco a b expected) =
        let got = compareVersions (mkVersion eco a) (mkVersion eco b)
         in if got == Just expected then Nothing else Just (a, b, expected, got)

{- | Load and parse the committed fixture (path relative to the package root,
which is the working directory Cabal runs the tests from).
-}
loadFixture :: IO [Row]
loadFixture = mapMaybe parseRow . lines . decodeUtf8 <$> readFileBS fixturePath
  where
    fixturePath = "core/test/unit/fixtures/version-ordering.txt"

parseRow :: Text -> Maybe Row
parseRow line
    | T.isPrefixOf "#" (T.strip line) = Nothing
    | otherwise = case T.splitOn "|" line of
        [e, a, b, o] -> Row <$> parseEco e <*> pure a <*> pure b <*> parseOrd o
        _ -> Nothing

parseEco :: Text -> Maybe Ecosystem
parseEco = \case
    "npm" -> Just Npm
    "pypi" -> Just PyPI
    "rubygems" -> Just RubyGems
    _ -> Nothing

parseOrd :: Text -> Maybe Ordering
parseOrd = \case
    "LT" -> Just LT
    "EQ" -> Just EQ
    "GT" -> Just GT
    _ -> Nothing

-- | Flip an 'Ordering' (the antisymmetry witness): @LT@↔@GT@, @EQ@ fixed.
invertOrdering :: Ordering -> Ordering
invertOrdering = \case
    LT -> GT
    EQ -> EQ
    GT -> LT

{- | Draw a pair of raw version strings, with a deterministic share of equal pairs, so the
@EQ@ class of an ordering law stays populated at any generator width.
-}
versionPair :: Gen Text -> Gen (Text, Text)
versionPair gen =
    Gen.frequency
        [ (4, (,) <$> gen <*> gen)
        , (1, (\v -> (v, v)) <$> gen)
        ]

{- | Draw a triple of raw version strings, sometimes reusing one draw across two positions so
an adjacent pair compares 'EQ', which exercises the transitivity antecedent across equal steps.
-}
versionTriple :: Gen Text -> Gen (Text, Text, Text)
versionTriple gen =
    Gen.frequency
        [ (3, (,,) <$> gen <*> gen <*> gen)
        , (1, (\u v -> (u, u, v)) <$> gen <*> gen)
        , (1, (\u v -> (u, v, v)) <$> gen <*> gen)
        ]

-- The shared per-ecosystem generators of structurally valid version strings
-- ('Ecluse.Test.Version'), paired with their ecosystem.
ecosystemGens :: [(Ecosystem, Gen Text)]
ecosystemGens =
    [ (Npm, genNpm)
    , (PyPI, genPyPI)
    , (RubyGems, genGem)
    ]

{- | A short raw version string, mixing parseable and unparseable shapes so
'selectLatest' runs on both keyed and key-less survivors.
-}
genRaw :: Gen Text
genRaw =
    Gen.element
        [ "1.0.0"
        , "1.2.0"
        , "1.3.0"
        , "2.0.0"
        , "2.0.0-rc.1"
        , "2.0.0-rc.2"
        , "0.9.0"
        , "garbage"
        , "also bad"
        ]
