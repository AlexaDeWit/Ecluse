module Ecluse.VersionOrderingSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Version

{- | Differential test: our 'compareVersions' must agree with the canonical
reference implementations (node-semver, Python @packaging@, Ruby
@Gem::Version@) on every comparison in the committed fixture. The fixture is
regenerated from those tools by @scripts/gen-version-fixtures.sh@
(@make gen-version-fixtures@); this test stays pure and offline by reading the
baked results. The same comparisons are re-checked against the live tools by the
non-gating smoke suite.
-}
spec :: Spec
spec = do
    rows <- runIO loadFixture
    describe "compareVersions agrees with the reference oracles" $ do
        it "the fixture is populated" $
            length rows `shouldSatisfy` (> 100)
        for_ [Npm, PyPI, RubyGems] $ \eco ->
            it (show eco <> " ordering matches the reference") $
                mismatches (filter (\(Row e _ _ _) -> e == eco) rows) `shouldBe` []

-- | One fixture comparison: @A@ relative to @B@ is @Ordering@ for @Ecosystem@.
data Row = Row Ecosystem Text Text Ordering
    deriving stock (Eq, Show)

{- | Rows where our comparator disagrees with the fixture, as
@(a, b, expected, got)@.
-}
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
