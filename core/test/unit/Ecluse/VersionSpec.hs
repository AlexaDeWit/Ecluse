module Ecluse.VersionSpec (spec) where

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
    describe "mkVersion / parseVersionKey" $ do
        it "round-trips the raw text through unVersion" $
            hedgehog $ do
                v <- forAll (Gen.text (Range.linear 1 12) Gen.ascii)
                unVersion (mkVersion Npm v) === v
        it "keeps the raw text even when unparseable (proxy fidelity)" $
            unVersion (mkVersion PyPI "totally bogus") `shouldBe` "totally bogus"
        it "has no key for unparseable input" $
            versionKey (mkVersion PyPI "totally bogus") `shouldBe` Nothing
        it "parses a valid version into a key" $
            versionKey (mkVersion Npm "1.2.3") `shouldSatisfy` isJust
        it "parseVersionKey reports an error for invalid input" $
            parseVersionKey Npm "nope" `shouldSatisfy` isLeft

    -- Strictness regressions: inputs the hand-rolled PEP 440 / Gem parsers used
    -- to over-accept must now be rejected (Left). The ordering fixture can only
    -- express *ranking*, so rejection is asserted explicitly here. The valid
    -- spellings each fix must keep parsing are pinned alongside as Right.
    describe "parser strictness (#279, #280)" $ do
        let mustReject eco raw =
                it (show eco <> " rejects " <> show raw) $
                    parseVersionKey eco raw `shouldSatisfy` isLeft
            mustParse eco raw =
                it (show eco <> " parses " <> show raw) $
                    parseVersionKey eco raw `shouldSatisfy` isRight

        describe "PEP 440 empty release segments (#279)" $ do
            -- An interior or leading empty segment is rejected (only the single
            -- trailing release/suffix separator dot is allowed to be empty).
            mustReject PyPI "1..0"
            mustReject PyPI ".1.0"
            mustReject PyPI "1.0..dev1"
            -- The regression the fix must protect: the dev separator's dot lands
            -- in the release text as a legitimate single trailing empty.
            mustParse PyPI "1.0.dev1"
            -- And the existing normalisation of a bare trailing dot is preserved.
            mustParse PyPI "1.0."

        describe "non-ASCII alphanumerics (#280)" $ do
            -- Python's packaging / Ruby's Gem::Version are ASCII-only; a
            -- Unicode-aware gate both over-accepts and (for "digits" outside
            -- ASCII) mis-classifies as text, corrupting the order.
            mustReject PyPI "1.0+café" -- Latin-1 letter in a local segment
            mustReject PyPI "１.２.３" -- fullwidth digits
            mustReject PyPI "١.٢.٣" -- Arabic-Indic digits
            mustReject PyPI "1.0²" -- superscript two (a Unicode "number")
            mustReject RubyGems "１.２.３" -- fullwidth digits
            mustReject RubyGems "١.٢.٣" -- Arabic-Indic digits
        describe "numeric run length bound (DoS)" $ do
            -- A numeric segment is read into an 'Integer' with 'readMaybe', which is
            -- quadratic in the digit count, so an unbounded run in hostile registry
            -- metadata would be an algorithmic-complexity DoS. An over-long version is
            -- rejected (and so served raw, without an ordering key) rather than parsed;
            -- a long-but-sane numeric segment still parses.
            mustReject PyPI ("1." <> T.replicate 5000 "9")
            mustReject RubyGems ("1." <> T.replicate 5000 "9")
            mustParse PyPI ("1." <> T.replicate 100 "9")
            mustParse RubyGems ("1." <> T.replicate 100 "9")
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
        it "PyPI canonicalises a non-normalised spelling (1.0ALPHA1 == 1.0a1)" $
            cmp PyPI "1.0ALPHA1" "1.0a1" `shouldBe` Just EQ
        it "RubyGems ranks a letter (prerelease) segment below the release" $
            cmp RubyGems "1.0.0.beta1" "1.0.0" `shouldBe` Just LT
        it "RubyGems orders numeric segments numerically" $
            cmp RubyGems "1.10.0" "1.9.0" `shouldBe` Just GT
        -- Gem::Version#canonical_segments drops a release trailing zero before the
        -- prerelease, so 2.0.a keys as [2,"a"]: 2.t > 2.0.a (a live-oracle
        -- differential counterexample), and 2.0.a == 2.a.
        it "RubyGems canonicalises a release trailing zero before a prerelease (2.t > 2.0.a)" $
            cmp RubyGems "2.t" "2.0.a" `shouldBe` Just GT
        it "RubyGems equates versions that canonicalise alike (2.0.a == 2.a)" $
            cmp RubyGems "2.0.a" "2.a" `shouldBe` Just EQ
        it "RubyGems strips a release trailing zero (2.0 == 2)" $
            cmp RubyGems "2.0" "2" `shouldBe` Just EQ
        -- Gem::Version canonicalises hyphens to a prerelease marker (a global
        -- gsub("-", ".pre.")), so "1.0.0-1" parses as "1.0.0.pre.1": it ranks
        -- below "1.0.0" and equates with the explicit .pre. spelling.
        it "RubyGems accepts a hyphenated version (1.0.0-1 parses)" $
            parseVersionKey RubyGems "1.0.0-1" `shouldSatisfy` isRight
        it "RubyGems ranks a hyphenated version below its release (1.0.0-1 < 1.0.0)" $
            cmp RubyGems "1.0.0-1" "1.0.0" `shouldBe` Just LT
        it "RubyGems equates a hyphen with the .pre. spelling (1.0.0-1 == 1.0.0.pre.1)" $
            cmp RubyGems "1.0.0-1" "1.0.0.pre.1" `shouldBe` Just EQ
        it "is Nothing when a version cannot be parsed" $
            cmp Npm "not a version" "1.0.0" `shouldBe` Nothing
        it "is reflexive — EQ when parseable, Nothing otherwise" $
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

    -- The total-order laws on 'compareVersions', proved generatively over
    -- structurally valid version strings (so each side parses to a key).
    -- Reflexivity is covered above and deliberately not repeated here. The pair
    -- and triple draws inject equal cases explicitly ('versionPair' /
    -- 'versionTriple' reuse one raw across positions, which compares EQ), so the
    -- EQ class stays covered independent of how often two independent draws
    -- collide — robust to the generator's width rather than relying on a narrow,
    -- collision-dense space.
    describe "compareVersions total-order laws" $
        modifyMaxSuccess (const 400) $
            for_ ecosystemGens $ \(eco, gen) -> describe (show eco) $ do
                it "totality — both parse ⇒ Just (never Nothing)" $
                    hedgehog $ do
                        (a, b) <- forAll (versionPair gen)
                        let (x, y) = (mkVersion eco a, mkVersion eco b)
                        -- The generators are meant to stay structurally valid;
                        -- guard so a generator gap surfaces as totality, not noise.
                        H.assert (isJust (versionKey x))
                        H.assert (isJust (versionKey y))
                        -- Non-vacuity: 'versionPair' guarantees a healthy fraction
                        -- of equal pairs (EQ), and its independent draws supply
                        -- LT/GT, so all three orderings stay populated.
                        H.cover 1 "LT" (compareVersions x y == Just LT)
                        H.cover 1 "EQ" (compareVersions x y == Just EQ)
                        H.cover 1 "GT" (compareVersions x y == Just GT)
                        H.assert (isJust (compareVersions x y))
                it "antisymmetry — cmp x y == invert <$> cmp y x" $
                    hedgehog $ do
                        (a, b) <- forAll (versionPair gen)
                        let (x, y) = (mkVersion eco a, mkVersion eco b)
                        compareVersions x y === fmap invertOrdering (compareVersions y x)
                it "transitivity — x ≤ y and y ≤ z ⇒ x ≤ z" $
                    hedgehog $ do
                        (a, b, c) <- forAll (versionTriple gen)
                        let x = mkVersion eco a
                            y = mkVersion eco b
                            z = mkVersion eco c
                        -- All three parse (generators stay valid); guard so a
                        -- generator gap can't make ≤ vacuously true via Nothing.
                        H.assert (all (isJust . versionKey) [x, y, z])
                        let le p q = compareVersions p q == Just LT || compareVersions p q == Just EQ
                        H.cover 1 "x ≤ y" (le x y)
                        H.cover 1 "x > y" (not (le x y))
                        when (le x y && le y z) (H.assert (le x z))

    describe "isStable" $ do
        -- isStable is defined on the parsed key; stableOf parses a known-good
        -- version then applies the predicate (Just True / Just False, never
        -- Nothing for these fixtures, which all parse).
        let stableOf eco raw = fmap isStable (rightToMaybe (parseVersionKey eco raw))

        describe "semver (npm)" $ do
            it "a final release is stable" $
                stableOf Npm "1.0.0" `shouldBe` Just True
            it "an -rc prerelease is not stable" $
                stableOf Npm "1.0.0-rc.1" `shouldBe` Just False
            it "a -beta prerelease is not stable" $
                stableOf Npm "2.0.0-beta" `shouldBe` Just False
            it "a numeric prerelease id is not stable" $
                stableOf Npm "1.0.0-1" `shouldBe` Just False

        describe "PEP 440 (PyPI)" $ do
            it "a final release is stable" $
                stableOf PyPI "1.0" `shouldBe` Just True
            it "a post-release is stable (post is not a prerelease)" $
                stableOf PyPI "1.0.post1" `shouldBe` Just True
            it "an alpha pre-release is not stable" $
                stableOf PyPI "1.0a1" `shouldBe` Just False
            it "an rc pre-release is not stable" $
                stableOf PyPI "1.0rc1" `shouldBe` Just False
            it "a dev release is not stable" $
                stableOf PyPI "1.0.dev1" `shouldBe` Just False
            it "a pre+dev release is not stable" $
                stableOf PyPI "1.0a1.dev2" `shouldBe` Just False
            it "a post+dev release is not stable (dev disqualifies)" $
                stableOf PyPI "1.0.post1.dev2" `shouldBe` Just False

        describe "RubyGems" $ do
            it "an all-numeric version is stable" $
                stableOf RubyGems "1.0.0" `shouldBe` Just True
            it "a .pre letter segment is not stable" $
                stableOf RubyGems "1.0.0.pre" `shouldBe` Just False
            it "a .rc1 letter segment is not stable" $
                stableOf RubyGems "1.2.0.rc1" `shouldBe` Just False

    describe "selectLatest" $ do
        -- All survivors here are npm versions; selectLatest is ecosystem-agnostic
        -- and just calls compareVersions / isStable on the keys. selRaw resolves
        -- and reports the chosen tag's raw text, the value the caller uses.
        let v = mkVersion Npm
            raws = map unVersion
            selRaw :: Maybe Text -> [Text] -> Maybe Text
            selRaw chosen survivors =
                unVersion <$> selectLatest (v <$> chosen) (map v survivors)

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
            -- "garbage" has no key; the parseable 1.0.0 must win.
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
                    Just r -> assert (unVersion r `elem` raws survivors)

-- | Flip an 'Ordering' (the antisymmetry witness): @LT@↔@GT@, @EQ@ fixed.
invertOrdering :: Ordering -> Ordering
invertOrdering = \case
    LT -> GT
    EQ -> EQ
    GT -> LT

{- | Draw a pair of raw version strings, deterministically injecting equal pairs
(the same raw on both sides, which compares 'EQ') a healthy fraction of the time.
This keeps the @EQ@ class of an ordering law populated independent of how often
two independent draws happen to collide, so the 'H.cover' floor is robust to the
generator's width rather than relying on a narrow, collision-dense space.
-}
versionPair :: Gen Text -> Gen (Text, Text)
versionPair gen =
    Gen.frequency
        [ (4, (,) <$> gen <*> gen)
        , (1, (\v -> (v, v)) <$> gen)
        ]

{- | Draw a triple of raw version strings, sometimes reusing one draw across two
positions so an adjacent pair compares 'EQ'. The transitivity antecedent
(@x ≤ y@ and @y ≤ z@) is then exercised across equal as well as strict steps.
-}
versionTriple :: Gen Text -> Gen (Text, Text, Text)
versionTriple gen =
    Gen.frequency
        [ (3, (,,) <$> gen <*> gen <*> gen)
        , (1, (\u v -> (u, u, v)) <$> gen <*> gen)
        , (1, (\u v -> (u, v, v)) <$> gen <*> gen)
        ]

-- The shared per-ecosystem generators of structurally valid version strings
-- ('Ecluse.Test.Version'), paired with their ecosystem. Each emits a string
-- 'versionKey' parses (the totality law guards this), while ranging widely enough
-- that 'H.cover' sees a mix of LT/EQ/GT across pairs.
ecosystemGens :: [(Ecosystem, Gen Text)]
ecosystemGens =
    [ (Npm, genNpm)
    , (PyPI, genPyPI)
    , (RubyGems, genGem)
    ]

{- | A short raw version string, mixing parseable and unparseable shapes so
'selectLatest' is exercised on both keyed and key-less survivors.
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
