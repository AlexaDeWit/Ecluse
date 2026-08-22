-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.VersionOraclesSpec (spec) where

import Control.Exception (IOException, try)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (..), ecosystemName)
import Ecluse.Core.Version
import Ecluse.Test.RegistryCapture (fetchVersions, loadCatalogue, smokeRegistryPackages)
import Ecluse.Test.Version qualified as V

{- | Smoke tier: check 'Ecluse.Core.Version.compareVersions' against the live reference oracles
(node-semver, Python @packaging@, Ruby @Gem::Version@) over the committed fixture, random inputs,
and real registry versions. Every check pends rather than fails when a tool, the network, or a
registry is unavailable, so a red here is a real disagreement.
-}
spec :: Spec
spec = do
    -- The curated package names come from the shared catalogue
    -- ("Ecluse.Test.RegistryCapture"), the one source the benchmark corpus capture
    -- reads too.
    catalogue <- runIO loadCatalogue
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
            for_ [(Npm, npmish), (PyPI, pypiish), (RubyGems, gemish)] $ \(eco, gen) -> do
                -- Probe once: without this every iteration would skip and the property would pass
                -- vacuously, hiding a broken oracle.
                available <- runIO (oracleAvailable eco)
                let title = show eco <> " -- generative differential (both-accept only)"
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
                            -- Assert only when both sides accepted the inputs.
                            -- Skip one-sided "what parses" disagreement.
                            case (ours, theirs) of
                                (Just o, Just t) -> do
                                    H.footnote (toString (raw1 <> " vs " <> raw2))
                                    o H.=== t
                                _ -> H.success

    -- Our 'compareVersions' must induce the reference oracle's order over the versions it
    -- accepts, and must never abstain on one. One subprocess per package, so this scales to
    -- thousands of versions. A registry, network, or tool failure pends rather than fails.
    describe "compareVersions agrees with the reference oracle on live registry versions" $
        for_ (smokeRegistryPackages catalogue) $ \(eco, pkgs) -> do
            -- Probe the oracle once. If its interpreter or library is missing, pend
            -- the whole ecosystem rather than letting every package skip.
            available <- runIO (oracleAvailable eco)
            if not available
                then
                    it (show eco <> " -- live registry ordering") $
                        pendingWith
                            ("reference oracle for " <> show eco <> " unavailable; run via `nix develop`")
                else do
                    manager <- runIO (newManager tlsManagerSettings)
                    for_ pkgs $ \pkg ->
                        it (show eco <> " -- " <> toString pkg <> " (live registry versions)") $ do
                            mVersions <- fetchVersions manager eco pkg
                            case mVersions of
                                Nothing ->
                                    pendingWith
                                        ( toString (ecosystemName eco)
                                            <> " registry unreachable or undecodable for "
                                            <> toString pkg
                                            <> "; smoke test skipped"
                                        )
                                Just [] ->
                                    pendingWith ("no versions published for " <> toString pkg <> "; smoke test skipped")
                                Just versions -> do
                                    mRef <- oracleSort eco versions
                                    case mRef of
                                        Nothing ->
                                            pendingWith
                                                ("reference oracle failed to sort versions for " <> toString pkg <> "; smoke test skipped")
                                        Just [] ->
                                            pendingWith
                                                ("reference oracle accepted none of the published versions for " <> toString pkg <> "; smoke test skipped")
                                        Just refSorted ->
                                            case findDivergences eco refSorted of
                                                [] ->
                                                    -- Non-vacuous: we ordered a real, reference-valid set.
                                                    length refSorted `shouldSatisfy` (> 0)
                                                ds ->
                                                    expectationFailure (renderDivergences eco pkg refSorted ds)
  where
    generatorScript = "scripts/gen-version-fixtures.sh"
    committedFixture = "core/test/unit/fixtures/version-ordering.txt"

{- | Whether the live oracle for @eco@ is reachable, probed on @1.0.0 < 1.0.1@. 'False' means the
interpreter or its library is missing, and the caller pends rather than running a vacuous property.
-}
oracleAvailable :: Ecosystem -> IO Bool
oracleAvailable eco = (== Just LT) <$> oracleCompare eco "1.0.0" "1.0.1"

{- | Compare two version strings with the live reference tool for @eco@, using the same expressions
as @scripts/gen-version-fixtures.sh@. 'Nothing' means the tool rejected an input or is unavailable.
-}
oracleCompare :: Ecosystem -> Text -> Text -> IO (Maybe Ordering)
oracleCompare eco a b = do
    let (interp, prog) = oracleProgram eco
    -- A missing interpreter makes readProcessWithExitCode throw. Treat that as
    -- "unavailable" (Nothing), the same as a non-zero exit, so probing stays total.
    result <-
        try (readProcessWithExitCode interp ["-", toString a, toString b] (toString prog))
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right (ExitSuccess, out, _err) -> parseOrdInt (T.strip (T.pack out))
        Right (ExitFailure _, _, _) -> Nothing

{- | The interpreter and stdin program for an ecosystem's oracle. Each reads the
two versions from @argv@ and prints @-1@\/@0@\/@1@, the sign of @a <=> b@. It exits
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

{- | Sort a version list with the live reference tool for @eco@, keeping only the versions that tool
accepts. 'Nothing' if the tool is unavailable or errors, mirroring 'oracleCompare'.
-}
oracleSort :: Ecosystem -> [Text] -> IO (Maybe [Text])
oracleSort eco versions = do
    let (interp, prog) = oracleSortProgram eco
    result <-
        try (readProcessWithExitCode interp ("-" : map toString versions) (toString prog))
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right (ExitSuccess, out, _err) ->
            Just (filter (not . T.null) (map T.strip (T.lines (T.pack out))))
        Right (ExitFailure _, _, _) -> Nothing

{- | The interpreter and stdin program for an ecosystem's __sort__ oracle, the list counterpart to
'oracleProgram'. Each prints the accepted versions in that tool's order, one per line.
-}
oracleSortProgram :: Ecosystem -> (String, Text)
oracleSortProgram = \case
    Npm ->
        ( "node"
        , unlines
            [ "const semver = require('semver');"
            , "const vs = process.argv.slice(2);"
            , "const valid = vs.filter((v) => semver.valid(v) !== null);"
            , "valid.sort(semver.compare);"
            , "process.stdout.write(valid.join('\\n'));"
            ]
        )
    PyPI ->
        ( "python3"
        , unlines
            [ "import sys"
            , "from packaging.version import Version, InvalidVersion"
            , "def ok(v):"
            , "    try:"
            , "        Version(v)"
            , "        return True"
            , "    except InvalidVersion:"
            , "        return False"
            , "vs = [v for v in sys.argv[1:] if ok(v)]"
            , "vs.sort(key=Version)"
            , "sys.stdout.write(\"\\n\".join(vs))"
            ]
        )
    RubyGems ->
        ( "ruby"
        , unlines
            [ "def ok(v)"
            , "  Gem::Version.new(v)"
            , "  true"
            , "rescue ArgumentError"
            , "  false"
            , "end"
            , "vs = ARGV.select { |v| ok(v) }"
            , "vs = vs.sort_by { |v| Gem::Version.new(v) }"
            , "STDOUT.write(vs.join(\"\\n\"))"
            ]
        )

{- | A way our 'compareVersions' disagrees with the reference oracle's order over
a real, reference-valid version list.
-}
data Divergence
    = {- | Our parser yields no ordering key for a version the reference accepts:
      we cannot order what the reference can.
      -}
      Abstained Text
    | {- | The reference orders the first version before the second, but our
      comparator ranks the first strictly /after/ it.
      -}
      Misordered Text Text
    deriving stock (Eq, Show)

{- | Every way our 'compareVersions' disagrees with @refSorted@, the reference-sorted valid subset:
an abstention on an accepted version, or a consecutive pair we report 'GT' for. Consecutive
pairs suffice because our key 'Ord' is total, and 'EQ' passes because the reference tie-breaks
by input order.
-}
findDivergences :: Ecosystem -> [Text] -> [Divergence]
findDivergences eco refSorted = abstentions <> misorders
  where
    abstentions = [Abstained v | v <- refSorted, isNothing (versionKey (mkVersion eco v))]
    misorders =
        [ Misordered a b
        | (a, b) <- zip refSorted (drop 1 refSorted)
        , compareVersions (mkVersion eco a) (mkVersion eco b) == Just GT
        ]

{- | A human-readable report of the divergences found for a package, for the
failing expectation's message.
-}
renderDivergences :: Ecosystem -> Text -> [Text] -> [Divergence] -> String
renderDivergences eco pkg refSorted ds =
    toString . T.unlines $ header : map render ds
  where
    header =
        "compareVersions diverges from the live reference oracle for "
            <> ecosystemName eco
            <> "/"
            <> pkg
            <> " over "
            <> show (length refSorted)
            <> " reference-valid published versions:"
    render = \case
        Abstained v ->
            "  abstain -- our parser returns no ordering key for reference-valid " <> v
        Misordered a b ->
            "  misorder -- reference orders " <> a <> " before " <> b <> ", but compareVersions says GT"

-- | npm-flavoured strings: the shared valid 'V.genNpm' mixed with 'messy'.
npmish :: Gen Text
npmish = Gen.choice [V.genNpm, messy]

-- | PEP 440-flavoured strings: the shared valid 'V.genPyPI' mixed with 'messy'.
pypiish :: Gen Text
pypiish = Gen.choice [V.genPyPI, messy]

-- | @Gem::Version@-flavoured strings: the shared valid 'V.genGem' mixed with 'messy'.
gemish :: Gen Text
gemish = Gen.choice [V.genGem, messy]

{- | Deliberately messy version-ish text. One or both sides reject most of these, so the property
skips them, but they widen the input distribution beyond the strictly-valid generators.
-}
messy :: Gen Text
messy =
    Gen.text
        (Range.linear 1 10)
        (Gen.element ('.' : '-' : '+' : '_' : '!' : ['0' .. '9'] <> "abcrvdevpostpre"))
