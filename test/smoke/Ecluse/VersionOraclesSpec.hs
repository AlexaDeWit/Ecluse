module Ecluse.VersionOraclesSpec (spec) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (
    Manager,
    httpLbs,
    newManager,
    parseUrlThrow,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (..), ecosystemName)
import Ecluse.Core.Registry.Npm.Wire (Packument (pkmtVersions))
import Ecluse.Core.Registry.Pypi.Wire qualified as Pypi
import Ecluse.Core.Registry.Rubygems.Wire qualified as Rubygems
import Ecluse.Core.Version
import Ecluse.Test.Version qualified as V

{- | Smoke tier: validate 'Ecluse.Core.Version.compareVersions' against the /live/
reference oracles (node-semver, Python @packaging@, Ruby @Gem::Version@). Three
complementary checks:

  1. The committed curated fixture is still byte-identical to what the oracles
     produce (the same comparisons the gating unit suite checks offline).
  2. A /generative/ differential: random version strings (a mix of valid and
     messy) are compared by our parser and by the live oracle, asserting they
     agree whenever __both__ accept the input. One-sided disagreement on /what
     parses/ is out of scope here and is skipped.
  3. A /real-registry/ differential: for a curated handful of real packages we
     fetch their __actually published__ versions from the live registry, let the
     reference oracle sort the reference-valid subset, and assert our
     'compareVersions' induces the same order over that subset (and never
     abstains on a version the reference accepts). This exercises gnarly version
     shapes the curated and random sets never imagined — divergences it surfaces
     are a backlog of corrections, not something to paper over here.

Non-gating by design (the smoke tier): the oracles come from the Nix dev shell's
version-ordering inputs and the registries are uncontrolled external services, so
the tests pend rather than fail when a tool, the network, or a registry is
unavailable. A red here is a real disagreement worth investigating.
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
            for_ [(Npm, npmish), (PyPI, pypiish), (RubyGems, gemish)] $ \(eco, gen) -> do
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

    -- Real-registry differential: fetch each package's actually-published
    -- versions from the live registry, let the reference oracle sort the subset
    -- it accepts, then assert our 'compareVersions' induces the same order over
    -- that subset (and never abstains on a version the reference accepts). One
    -- subprocess per package (the oracle sorts the whole list), so this scales
    -- to packages with thousands of versions. Registry/network/tool failures
    -- pend rather than fail — only a genuine disagreement reddens.
    describe "compareVersions agrees with the reference oracle on live registry versions" $
        for_ registryPackages $ \(eco, pkgs) -> do
            -- Probe the oracle once; if its interpreter/library is missing, pend
            -- the whole ecosystem rather than letting every package skip.
            available <- runIO (oracleAvailable eco)
            if not available
                then
                    it (show eco <> " — live registry ordering") $
                        pendingWith
                            ("reference oracle for " <> show eco <> " unavailable; run via `nix develop`")
                else do
                    manager <- runIO (newManager tlsManagerSettings)
                    for_ pkgs $ \pkg ->
                        it (show eco <> " — " <> toString pkg <> " (live registry versions)") $ do
                            mVersions <- fetchRegistryVersions manager eco pkg
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

-- ── real-registry differential ──────────────────────────────────────────────

{- | A curated handful of real packages per ecosystem, favouring gnarly-versioned
ones (long histories, pre\/post\/dev releases, scoped names). Only the __names__
are pinned here; their version lists are fetched __live__ from the registry, so
the corpus tracks reality and keeps surfacing shapes the curated\/random sets
never imagined.
-}
registryPackages :: [(Ecosystem, [Text])]
registryPackages =
    [ (Npm, ["typescript", "eslint", "@types/node", "react", "webpack", "next", "rxjs", "lodash"])
    , (PyPI, ["numpy", "requests", "django", "boto3", "urllib3", "setuptools", "pip"])
    , (RubyGems, ["rails", "bundler", "nokogiri", "rspec", "activesupport"])
    ]

{- | The registry JSON endpoint that lists a package's published versions. Scoped
npm names are URL-encoded (@\@types/node@ → @\@types%2Fnode@); the other forms
take a bare name.
-}
versionsUrl :: Ecosystem -> Text -> Text
versionsUrl eco pkg = case eco of
    Npm -> "https://registry.npmjs.org/" <> T.replace "/" "%2F" pkg
    PyPI -> "https://pypi.org/pypi/" <> pkg <> "/json"
    RubyGems -> "https://rubygems.org/api/v1/versions/" <> pkg <> ".json"

{- | Fetch a package's published version strings from its registry. 'Nothing' on
any network failure, non-2xx status (a 404 throws via 'parseUrlThrow'), or a body
that does not decode — the caller then pends, since registry flakiness must never
hard-fail the (non-gating) smoke tier.
-}
fetchRegistryVersions :: Manager -> Ecosystem -> Text -> IO (Maybe [Text])
fetchRegistryVersions manager eco pkg = do
    result <- try $ do
        req0 <- parseUrlThrow (toString (versionsUrl eco pkg))
        let req =
                req0
                    { requestHeaders = [("User-Agent", "ecluse-version-oracle-smoke")]
                    , responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)
                    }
        responseBody <$> httpLbs req manager
    pure $ case result of
        Left (_ :: SomeException) -> Nothing
        Right body -> parseRegistryVersions eco body

{- | Extract the published version strings from a registry's response via each
ecosystem's __canonical__ wire decoder, rather than re-parsing the JSON here: the
npm packument ('Ecluse.Core.Registry.Npm.Wire.Packument'), the PyPI project JSON
('Ecluse.Core.Registry.Pypi.Wire.ProjectJson'), or the RubyGems versions array
('Ecluse.Core.Registry.Rubygems.Wire.VersionListing'). 'Nothing' if the body does not
decode for that ecosystem.
-}
parseRegistryVersions :: Ecosystem -> LByteString -> Maybe [Text]
parseRegistryVersions eco body = case eco of
    Npm -> Map.keys . pkmtVersions <$> decode' body
    PyPI -> Pypi.projectVersions <$> decode' body
    RubyGems -> Rubygems.listingVersions <$> decode' body
  where
    decode' :: (FromJSON a) => LByteString -> Maybe a
    decode' = rightToMaybe . eitherDecode

{- | Sort a version list with the live reference tool for @eco@, keeping only the
versions that tool considers valid (node @semver.valid@, Python @packaging@, Ruby
@Gem::Version@). One subprocess per call — the whole list goes on @argv@ and the
reference-sorted valid subset comes back one-per-line. 'Nothing' if the tool is
unavailable or errors, mirroring 'oracleCompare'.
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

{- | The interpreter and stdin program for an ecosystem's __sort__ oracle (the
list counterpart to 'oracleProgram'). Each reads the version list from @argv@,
filters to the versions it accepts, sorts them by that tool's ordering, and prints
the result one-per-line.
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
    = {- | Our parser yields no ordering key for a version the reference accepts —
      we cannot order what the reference can.
      -}
      Abstained Text
    | {- | The reference orders the first version before the second, but our
      comparator ranks the first strictly /after/ it.
      -}
      Misordered Text Text
    deriving stock (Eq, Show)

{- | Every way our 'compareVersions' disagrees with @refSorted@ (the
reference-sorted, reference-valid subset). Two kinds: an abstention (our parser
returns no key for an accepted version) and a misordering (a consecutive pair the
reference put @a@-before-@b@ for which we report 'GT'). Because our key 'Ord' is a
total order, checking only consecutive pairs is sufficient: if our order differed
from the reference's anywhere, some adjacent reference pair would be reversed. An
'EQ' is tolerated — the reference's stable sort tie-breaks by input order, which
we cannot (and need not) reproduce.
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
            "  abstain — our parser returns no ordering key for reference-valid " <> v
        Misordered a b ->
            "  misorder — reference orders " <> a <> " before " <> b <> ", but compareVersions says GT"

-- ── generators (a mix of structurally valid and messy strings) ──────────────

-- The structurally valid cores come from the shared 'Ecluse.Test.Version'
-- generators; each is mixed here with the deliberately 'messy' generator so the
-- differential also exercises malformed inputs. The both-accept gate filters out
-- whatever neither side (ours or the live oracle) should compare.

-- | npm-flavoured strings: the shared valid 'V.genNpm' mixed with 'messy'.
npmish :: Gen Text
npmish = Gen.choice [V.genNpm, messy]

-- | PEP 440-flavoured strings: the shared valid 'V.genPyPI' mixed with 'messy'.
pypiish :: Gen Text
pypiish = Gen.choice [V.genPyPI, messy]

-- | @Gem::Version@-flavoured strings: the shared valid 'V.genGem' mixed with 'messy'.
gemish :: Gen Text
gemish = Gen.choice [V.genGem, messy]

{- | Deliberately messy version-ish text: short tokens, stray separators, and
mixed alnum. Most of these are rejected by one or both sides (and thus skipped),
but they widen the input distribution beyond the strictly-valid generators.
-}
messy :: Gen Text
messy =
    Gen.text
        (Range.linear 1 10)
        (Gen.element ('.' : '-' : '+' : '_' : '!' : ['0' .. '9'] <> "abcrvdevpostpre"))
