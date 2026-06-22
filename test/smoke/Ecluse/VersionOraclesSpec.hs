module Ecluse.VersionOraclesSpec (spec) where

import Control.Exception (IOException, try)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Version

{- | Smoke tier: validate 'Ecluse.Version.compareVersions' against the /live/
reference oracles (node-semver, Python @packaging@, Ruby @Gem::Version@). Two
complementary checks:

  1. The committed curated fixture is still byte-identical to what the oracles
     produce (the same comparisons the gating unit suite checks offline).
  2. A /generative/ differential: random version strings (a mix of valid and
     messy) are compared by our parser and by the live oracle, asserting they
     agree whenever __both__ accept the input. One-sided disagreement on /what
     parses/ is out of scope here and is skipped.

Non-gating by design (the smoke tier): the oracles come from the Nix dev shell's
version-ordering inputs; a bare checkout may not have them, so the tests pend
rather than fail when a tool is unavailable.
-}
spec :: Spec
spec = do
    describe "version-ordering fixtures vs the live reference oracles" $
        it "regenerating from node-semver / packaging / Gem::Version reproduces the committed fixture" $ do
            tmpDir <- getTemporaryDirectory
            let regenerated = tmpDir <> "/ecluse-version-fixtures-smoke.txt"
            (code, _out, _err) <-
                readProcessWithExitCode "bash" [generatorScript, regenerated] ""
            case code of
                ExitFailure _ ->
                    pendingWith
                        "reference oracles unavailable; run via `nix develop` (node-semver / packaging / ruby)"
                ExitSuccess -> do
                    fresh <- readFileBS regenerated
                    baked <- readFileBS committedFixture
                    fresh `shouldBe` baked

    -- Modest iteration count: this tier shells out to a tool per comparison.
    describe "compareVersions agrees with the live oracle on random inputs" $
        modifyMaxSuccess (const 60) $
            for_ [(Npm, genNpm), (PyPI, genPyPI), (RubyGems, genGem)] $ \(eco, gen) -> do
                -- Probe the oracle once on a known-valid pair. If it can't be
                -- reached (interpreter or library missing), pend the whole
                -- ecosystem — otherwise every iteration would skip and the
                -- property would pass vacuously, hiding a broken oracle.
                available <- runIO (oracleAvailable eco)
                let title = show eco <> " — generative differential (both-accept only)"
                if not available
                    then
                        it title $
                            pendingWith
                                ("reference oracle for " <> show eco <> " unavailable; run via `nix develop`")
                    else it title $
                        hedgehog $ do
                            raw1 <- H.forAll gen
                            raw2 <- H.forAll gen
                            let ours = compareVersions (mkVersion eco raw1) (mkVersion eco raw2)
                            theirs <- H.evalIO (oracleCompare eco raw1 raw2)
                            -- Assert only when both sides accepted the inputs;
                            -- skip one-sided "what parses" disagreement.
                            case (ours, theirs) of
                                (Just o, Just t) -> do
                                    H.footnote (toString (raw1 <> " vs " <> raw2))
                                    o H.=== t
                                _ -> H.success
  where
    generatorScript = "scripts/gen-version-fixtures.sh"
    committedFixture = "test/unit/fixtures/version-ordering.txt"

-- ── live oracle invocation ──────────────────────────────────────────────────

{- | Whether the live oracle for @eco@ is reachable, probed once on a known-valid
pair (@1.0.0 < 1.0.1@) that every working oracle must order as 'LT'. A 'Nothing'
means the interpreter or its library is missing (e.g. Python @packaging@ not on
@PATH@), so the caller pends rather than running a vacuously-green property.
-}
oracleAvailable :: Ecosystem -> IO Bool
oracleAvailable eco = (== Just LT) <$> oracleCompare eco "1.0.0" "1.0.1"

{- | Compare two version strings with the live reference tool for @eco@, mirroring
the exact expressions @scripts/gen-version-fixtures.sh@ uses (npm→@semver.compare@,
PyPI→@packaging.version.Version@, RubyGems→@Gem::Version <=>@). 'Nothing' means the
tool rejected an input (non-zero exit, e.g. a parse error) or is unavailable — the
caller then skips, since one-sided "what parses" disagreement is out of scope.
-}
oracleCompare :: Ecosystem -> Text -> Text -> IO (Maybe Ordering)
oracleCompare eco a b = do
    let (interp, prog) = oracleProgram eco
    -- A missing interpreter makes readProcessWithExitCode throw; treat that as
    -- "unavailable" (Nothing), same as a non-zero exit, so probing stays total.
    result <-
        try (readProcessWithExitCode interp ["-", toString a, toString b] (toString prog))
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right (ExitSuccess, out, _err) -> parseOrdInt (T.strip (T.pack out))
        Right (ExitFailure _, _, _) -> Nothing

{- | The interpreter and stdin program for an ecosystem's oracle. Each reads the
two versions from @argv@ and prints @-1@\/@0@\/@1@ (the sign of @a <=> b@), exiting
non-zero if either input does not parse for that tool.
-}
oracleProgram :: Ecosystem -> (String, Text)
oracleProgram = \case
    Npm ->
        ( "node"
        , unlines
            [ "const semver = require('semver');"
            , "const a = process.argv[2], b = process.argv[3];"
            , "if (semver.valid(a) === null || semver.valid(b) === null) process.exit(1);"
            , "console.log(String(semver.compare(a, b)));"
            ]
        )
    PyPI ->
        ( "python3"
        , unlines
            [ "import sys"
            , "from packaging.version import Version, InvalidVersion"
            , "try:"
            , "    A, B = Version(sys.argv[1]), Version(sys.argv[2])"
            , "except InvalidVersion:"
            , "    sys.exit(1)"
            , "print((A > B) - (A < B))"
            ]
        )
    RubyGems ->
        ( "ruby"
        , unlines
            [ "begin"
            , "  a = Gem::Version.new(ARGV[0])"
            , "  b = Gem::Version.new(ARGV[1])"
            , "rescue ArgumentError"
            , "  exit 1"
            , "end"
            , "puts(a <=> b)"
            ]
        )

-- | Parse the oracle's @-1@\/@0@\/@1@ sign output into an 'Ordering'.
parseOrdInt :: Text -> Maybe Ordering
parseOrdInt = \case
    "-1" -> Just LT
    "0" -> Just EQ
    "1" -> Just GT
    _ -> Nothing

-- ── generators (a mix of structurally valid and messy strings) ──────────────

{- | npm-flavoured strings: valid semver (core + optional pre/build) mixed with
the shared 'messy' generator. The both-accept gate filters out whatever neither
side should compare.
-}
genNpm :: Gen Text
genNpm =
    Gen.choice
        [ validNpm
        , messy
        ]
  where
    validNpm = do
        core <- T.intercalate "." <$> Gen.list (Range.singleton 3) numSeg
        pre <- Gen.maybe (("-" <>) . T.intercalate "." <$> Gen.list (Range.linear 1 3) preId)
        build <- Gen.maybe (("+" <>) <$> alnumRun)
        pure (core <> fromMaybe "" pre <> fromMaybe "" build)
    preId = Gen.choice [numSeg, alnumId]

{- | PEP 440-flavoured strings: canonical releases with optional pre/post/dev,
plus messy variants exercising the both-accept gate.
-}
genPyPI :: Gen Text
genPyPI =
    Gen.choice
        [ valid
        , messy
        ]
  where
    valid = do
        release <- T.intercalate "." <$> Gen.list (Range.linear 1 3) numSeg
        pre <- Gen.maybe ((<>) <$> Gen.element ["a", "b", "rc"] <*> numSeg)
        post <- Gen.maybe ((".post" <>) <$> numSeg)
        dev <- Gen.maybe ((".dev" <>) <$> numSeg)
        pure (release <> fromMaybe "" pre <> fromMaybe "" post <> fromMaybe "" dev)

{- | @Gem::Version@-flavoured strings: dotted numeric segments with an optional
letter-led prerelease segment, plus messy variants.
-}
genGem :: Gen Text
genGem =
    Gen.choice
        [ valid
        , messy
        ]
  where
    valid = do
        nums <- Gen.list (Range.linear 1 4) numSeg
        pre <- Gen.maybe gemPreSeg
        pure (T.intercalate "." (nums <> maybeToList pre))
    gemPreSeg = do
        c <- Gen.element ['a' .. 'z']
        rest <- Gen.text (Range.linear 0 4) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))
        pure (T.cons c rest)

{- | Deliberately messy version-ish text: short tokens, stray separators, and
mixed alnum. Most of these are rejected by one or both sides (and thus skipped),
but they widen the input distribution beyond the strictly-valid generators.
-}
messy :: Gen Text
messy =
    Gen.text
        (Range.linear 1 10)
        (Gen.element ('.' : '-' : '+' : '_' : '!' : ['0' .. '9'] <> "abcrvdevpostpre"))

-- ── shared atoms ─────────────────────────────────────────────────────────────

-- | A small non-negative integer segment.
numSeg :: Gen Text
numSeg = show <$> Gen.integral (Range.linear 0 (15 :: Integer))

-- | A non-empty alphanumeric run (build metadata / prerelease text).
alnumRun :: Gen Text
alnumRun = Gen.text (Range.linear 1 4) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))

-- | A letter-led alphanumeric identifier (a non-numeric semver prerelease id).
alnumId :: Gen Text
alnumId = do
    c <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'])
    rest <- Gen.text (Range.linear 0 4) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-"))
    pure (T.cons c rest)
