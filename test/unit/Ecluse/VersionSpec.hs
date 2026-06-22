module Ecluse.VersionSpec (spec) where

import Data.Text qualified as T
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Version

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
    -- Reflexivity is covered above and deliberately not repeated here.
    -- Run more than the default 100 examples: the EQ branch (collisions /
    -- canonical re-spellings) is naturally a few percent, so a larger sample
    -- keeps its 'H.cover' floor comfortably met rather than seed-flaky.
    describe "compareVersions total-order laws" $
        modifyMaxSuccess (const 400) $
            for_ ecosystemGens $ \(eco, gen) -> describe (show eco) $ do
                it "totality — both parse ⇒ Just (never Nothing)" $
                    hedgehog $ do
                        a <- forAll gen
                        b <- forAll gen
                        let (x, y) = (mkVersion eco a, mkVersion eco b)
                        -- The generators are meant to stay structurally valid;
                        -- guard so a generator gap surfaces as totality, not noise.
                        H.assert (isJust (versionKey x))
                        H.assert (isJust (versionKey y))
                        -- Non-vacuity: the generators draw from a small enough
                        -- space that two independent draws yield a mix of all
                        -- three orderings (EQ via frequent collisions / canonical
                        -- re-spellings, LT/GT from the rest).
                        H.cover 1 "LT" (compareVersions x y == Just LT)
                        H.cover 1 "EQ" (compareVersions x y == Just EQ)
                        H.cover 1 "GT" (compareVersions x y == Just GT)
                        H.assert (isJust (compareVersions x y))
                it "antisymmetry — cmp x y == invert <$> cmp y x" $
                    hedgehog $ do
                        a <- forAll gen
                        b <- forAll gen
                        let (x, y) = (mkVersion eco a, mkVersion eco b)
                        compareVersions x y === fmap invertOrdering (compareVersions y x)
                it "transitivity — x ≤ y and y ≤ z ⇒ x ≤ z" $
                    hedgehog $ do
                        a <- forAll gen
                        b <- forAll gen
                        c <- forAll gen
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

-- | Flip an 'Ordering' (the antisymmetry witness): @LT@↔@GT@, @EQ@ fixed.
invertOrdering :: Ordering -> Ordering
invertOrdering = \case
    LT -> GT
    EQ -> EQ
    GT -> LT

-- Per-ecosystem generators of structurally valid version strings, paired with
-- their ecosystem. Each is built so 'versionKey' is 'Just' (the totality law
-- guards this), while still ranging widely enough that 'H.cover' sees a mix of
-- LT/EQ/GT across pairs.
ecosystemGens :: [(Ecosystem, Gen Text)]
ecosystemGens =
    [ (Npm, genNpm)
    , (PyPI, genPyPI)
    , (RubyGems, genGem)
    ]

{- | A small non-negative integer rendered without a leading-zero pathology.
The range is deliberately narrow so two independent draws collide often,
giving the EQ branch of 'H.cover' a healthy (non-vacuous) population.
-}
genNum :: Gen Text
genNum = show <$> Gen.integral (Range.linear 0 (3 :: Integer))

{- | npm semver: @MAJOR.MINOR.PATCH@ + optional @-prerelease@ (dot-separated
numeric or alphanumeric ids) + optional @+build@ metadata (parser-stripped).
The prerelease ids and build are drawn from small fixed pools so two independent
draws collide often (a healthy EQ population) while still spanning LT/GT.
-}
genNpm :: Gen Text
genNpm = do
    core <- T.intercalate "." <$> Gen.list (Range.singleton 3) genNum
    pre <- Gen.maybe genPre
    build <- Gen.maybe (Gen.element ["build", "001", "exp-1"])
    pure (core <> maybe "" ("-" <>) pre <> maybe "" ("+" <>) build)
  where
    genPre = T.intercalate "." <$> Gen.list (Range.linear 1 3) genPreId
    -- Either a numeric id (small range) or a short alphanumeric id from a fixed
    -- pool; both are accepted by the semver parser (alnum ids are SemverText).
    genPreId = Gen.choice [genNum, Gen.element ["alpha", "beta", "rc", "x", "a1"]]

{- | PEP 440 (PyPI): release tuple + optional @aN@/@bN@/@rcN@ prerelease +
optional @.postN@ + optional @.devN@. All in canonical spelling so it parses.
-}
genPyPI :: Gen Text
genPyPI = do
    release <- T.intercalate "." <$> Gen.list (Range.linear 1 3) genNum
    pre <- Gen.maybe genPre
    post <- Gen.maybe ((".post" <>) <$> genNum)
    dev <- Gen.maybe ((".dev" <>) <$> genNum)
    pure (release <> fromMaybe "" pre <> fromMaybe "" post <> fromMaybe "" dev)
  where
    genPre = do
        stage <- Gen.element ["a", "b", "rc"]
        n <- genNum
        pure (stage <> n)

{- | @Gem::Version@ (RubyGems): dot-separated numeric segments + optional
trailing letter-led prerelease segment (e.g. @1.2.3.beta1@), the latter from a
small fixed pool so collisions (hence EQ) stay frequent.
-}
genGem :: Gen Text
genGem = do
    nums <- Gen.list (Range.linear 1 4) genNum
    pre <- Gen.maybe (Gen.element ["alpha", "beta1", "pre", "rc2"])
    pure (T.intercalate "." (nums <> maybeToList pre))
