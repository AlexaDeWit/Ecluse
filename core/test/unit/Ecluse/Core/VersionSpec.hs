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

import Ecluse.Core.Ecosystem (Ecosystem (..), parseEcosystem)
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

    describe "Cargo identity" $ do
        it "keeps raw build spellings distinct while using native precedence" $ do
            let firstBuild = mkVersion Cargo "1.2.3+first"
                secondBuild = mkVersion Cargo "1.2.3+second"
            firstBuild `shouldNotBe` secondBuild
            compareVersions firstBuild secondBuild `shouldBe` Just EQ
            renderVersion firstBuild `shouldBe` "1.2.3+first"
        it "uses its own grammar and ecosystem key" $ do
            parseVersionKey Cargo "1.2.3" `shouldNotBe` parseVersionKey Npm "1.2.3"
            parseAdvisoryVersionKey Cargo "v1.2.3" `shouldSatisfy` isLeft
            parseAdvisoryVersionKey Npm "v1.2.3" `shouldSatisfy` isRight
            renderVersionKey <$> parseVersionKey Cargo "18446744073709551615.0.0" `shouldBe` Right "18446744073709551615.0.0"

    describe "releaseIdentity" $ do
        it "treats Cargo versions that differ only in build metadata as one release" $
            map (releaseIdentity . mkVersion Cargo) ["1.0.7+corp", "1.0.7", "1.0.7+other"] `shouldBe` ["1.0.7", "1.0.7", "1.0.7"]
        it "keeps a Cargo prerelease distinct from its release" $ do
            releaseIdentity (mkVersion Cargo "1.2.3-rc.1+b") `shouldBe` "1.2.3-rc.1"
            releaseIdentity (mkVersion Cargo "1.2.3-rc.1+b") `shouldNotBe` releaseIdentity (mkVersion Cargo "1.2.3")
        it "keeps the raw spelling of an unparseable Cargo version" $
            releaseIdentity (mkVersion Cargo "not-a-version") `shouldBe` "not-a-version"
        it "keeps the raw spelling in other ecosystems" $ do
            releaseIdentity (mkVersion Npm "1.0.0+a") `shouldBe` "1.0.0+a"
            map (releaseIdentity . mkVersion RubyGems) ["1.0", "1.0.0"] `shouldBe` ["1.0", "1.0.0"]
            releaseIdentity (mkVersion PyPI "1.0") `shouldBe` "1.0"

    describe "versionKeyIn" $
        it "is the key mkVersion builds under that ecosystem, whichever ecosystem built the version" $
            hedgehog $ do
                raw <- forAll (Gen.choice (Gen.element ["1.0", "1.0.0+build", "1.0.0.pre", "v1", "bogus"] : map snd ecosystemGens))
                eco <- forAll (Gen.element (map fst ecosystemGens))
                builtUnder <- forAll (Gen.element (map fst ecosystemGens))
                versionKeyIn eco (mkVersion builtUnder raw) === versionKey (mkVersion eco raw)

    describe "canonical advisory identity" $ do
        for_ ecosystemGens $ \(eco, gen) ->
            it ("round-trips and renders idempotently for " <> show eco) $
                hedgehog $ do
                    raw <- forAll gen
                    key <- H.evalEither (parseVersionKey eco raw)
                    let canonical = renderVersionKey key
                    reparsed <- H.evalEither (parseVersionKey eco canonical)
                    reparsed === key
                    renderVersionKey reparsed === canonical
                    renderVersion (mkVersion eco raw) === raw
        it "normalises npm advisory v prefixes without widening public parsing" $ do
            renderVersionKey <$> parseAdvisoryVersionKey Npm "v1.2.0-rc.1+build.07" `shouldBe` Right "1.2.0-rc.1+build.07"
            parseVersionKey Npm "v1.2.0" `shouldSatisfy` isLeft
            for_ ["vv1.2.0", "V1.2.0", "v01.2.0", "v1.2", "v1.2.0-01", "v1.2.0+1234567890123456789", "v1.2.0+" <> T.replicate 1018 "a"] $ \raw ->
                parseAdvisoryVersionKey Npm raw `shouldSatisfy` isLeft
        for_ [(PyPI, "1.19.0", "1.19"), (RubyGems, "1.19.0", "1.19"), (Npm, "1.19.0", "1.19.0")] $ \(eco, raw, expected) ->
            it ("renders " <> show eco <> " keys while preserving wire text") $ do
                renderVersionKey <$> parseAdvisoryVersionKey eco raw `shouldBe` Right expected
                renderVersion (mkVersion eco raw) `shouldBe` raw

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
            -- Rejecting PEP 440's r spelling would abstain from ordering and let
            -- selectLatest move dist-tags.latest past that version.
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
            -- npm numeric keys use fixed-width words. Refuse oversized components
            -- before overflow can order a huge version as a small one.
            mustReject Npm ("1.0." <> T.replicate 5000 "9")
            mustReject Npm (T.replicate 25 "9" <> ".0.0")
            mustParse Npm "1.2.3"
            mustParse Npm ("1.0." <> T.replicate 15 "9")

    -- The differential check against node-semver, Python packaging, and Ruby Gem::Version.
    -- Regenerate the fixture with scripts/gen-version-fixtures.sh: it keeps this test offline.
    rows <- runIO loadFixture
    describe "reference fixture validation" $ do
        it "ignores only comments and blank lines" $
            parseFixture "  # comment\n\n npm|1|2|LT\npypi|1|2|LT\nrubygems|1|2|LT"
                `shouldBe` Right [Row Npm "1" "2" LT, Row PyPI "1" "2" LT, Row RubyGems "1" "2" LT]
        for_ ["npm|1|2", "unknown|1|2|LT", "npm||2|LT", "npm|1|2|BAD"] $ \bad ->
            it ("reports the malformed row's line: " <> toString bad) $
                parseFixture ("# comment\n\n" <> bad) `shouldBe` Left (MalformedRow 3 bad)
        for_ [Npm, PyPI, RubyGems] $ \missing ->
            it ("rejects missing " <> show missing <> " comparisons") $ do
                let complete = [Row Npm "1" "2" LT, Row PyPI "1" "2" LT, Row RubyGems "1" "2" LT]
                requireEcosystems (filter (\(Row eco _ _ _) -> eco /= missing) complete)
                    `shouldBe` Left (MissingEcosystem missing)
    describe "compareVersions agrees with the reference oracles" $ do
        it "the fixture is populated" $
            length rows `shouldSatisfy` (> 100)
        for_ [Npm, PyPI, RubyGems] $ \eco ->
            it (show eco <> " ordering matches the reference") $ do
                let comparisons = filter (\(Row e _ _ _) -> e == eco) rows
                comparisons `shouldSatisfy` (not . null)
                mismatches comparisons `shouldBe` []

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
                        -- Equal pairs and independent draws must exercise all three
                        -- orderings so the property cannot pass vacuously.
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
        let spelling = fmap renderVersion . canonicalPep440

        it "spells a release without its trailing zeros" $
            spelling "1.0.0" `shouldBe` Just "1"

        it "normalises the documented PEP 440 spellings" $
            map spelling ["2!1.0", "1.0-RC.1", "1.0-1", "1.0dev", "1.0+Ubuntu-1", "not-a-version"]
                `shouldBe` [Just "2!1", Just "1rc1", Just "1.post1", Just "1.dev0", Just "1+ubuntu.1", Nothing]

        it "gives two spellings of one release the same version" $
            canonicalPep440 "1.0" `shouldBe` canonicalPep440 "1.0.0"

        it "normalises an unnormalised prerelease, post-release, dev-release and local segment" $
            spelling "1!2.0ALPHA1-1.dev2+Ubuntu.7" `shouldBe` Just "1!2a1.post1.dev2+ubuntu.7"

        it "still spells a release whose every segment is zero" $
            spelling "0.0" `shouldBe` Just "0"

        it "has no spelling for text that is not PEP 440" $
            canonicalPep440 "totally bogus" `shouldBe` Nothing

        it "preserves the ordering key, so keying by the spelling keys by the release" $
            -- The PyPI projection keys a release by this spelling. That is only sound while the
            -- spelling and the version it came from carry one ordering key.
            hedgehog $ do
                v <- forAll genPyPI
                (spelling v >>= rightToMaybe . parseVersionKey PyPI)
                    === rightToMaybe (parseVersionKey PyPI v)

        it "is the version mkVersion builds from its spelling" $
            for_ ["1.0.0", "1!2.0ALPHA1-1.dev2+Ubuntu.7", "0.0", "v1.0rc"] $ \raw ->
                canonicalPep440 raw `shouldBe` (mkVersion PyPI <$> spelling raw)

        it "carries no key when its spelling passes the length bound, as mkVersion reads none" $ do
            -- The text is at the bound, and its spelling gains the implicit pre-release number.
            let atBound = T.replicate 1023 "1" <> "a"
            spelling atBound `shouldBe` Just (atBound <> "0")
            fmap versionKey (canonicalPep440 atBound) `shouldBe` Just Nothing
            canonicalPep440 atBound `shouldBe` (mkVersion PyPI <$> spelling atBound)
            versionKey (mkVersion PyPI atBound) `shouldSatisfy` isJust

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

data FixtureError = MalformedRow Int Text | MissingEcosystem Ecosystem
    deriving stock (Eq, Show)

loadFixture :: IO [Row]
loadFixture = do
    bytes <- readFileBS "core/test/unit/fixtures/version-ordering.txt"
    either (fail . show) pure (parseFixture (decodeUtf8 bytes))

parseFixture :: Text -> Either FixtureError [Row]
parseFixture input = traverse parseIndexed (zip [1 ..] (lines input)) >>= requireEcosystems . catMaybes
  where
    parseIndexed (number, line)
        | T.null stripped || T.isPrefixOf "#" stripped = Right Nothing
        | otherwise = maybe (Left (MalformedRow number line)) (Right . Just) (parseRow stripped)
      where
        stripped = T.strip line

requireEcosystems :: [Row] -> Either FixtureError [Row]
requireEcosystems rows = do
    for_ [Npm, PyPI, RubyGems] $ \eco ->
        unless (any (\(Row e _ _ _) -> e == eco) rows) (Left (MissingEcosystem eco))
    pure rows

parseRow :: Text -> Maybe Row
parseRow line = case T.splitOn "|" line of
    [e, a, b, o] | not (T.null a || T.null b) -> Row <$> parseEcosystem e <*> pure a <*> pure b <*> parseOrd o
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
