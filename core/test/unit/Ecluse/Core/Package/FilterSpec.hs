-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Rule-plan and artifact-location contracts for the shared package filter.
module Ecluse.Core.Package.FilterSpec (spec) where

import Data.Aeson (Value (String))
import Data.Char (toUpper)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Hedgehog (Gen, assert, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    InvalidEntry (invalidKey, invalidKind, invalidReason, invalidValue),
    InvalidEntryKind (InvalidIndexFile, InvalidVersionManifest),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
 )
import Ecluse.Core.Package.Filter (ArtifactRefusal (refusedReason, refusedUrl), FilterPlan (..), artifactOrigin, enforceArtifactLocations, enforceArtifactLocationsOf, resolveArtifact)
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Rules.Types (
    EvalContext (EvalContext),
    PrecededRule,
    Rule (AllowIfOlderThan, DenyInstallTimeExecution),
 )
import Ecluse.Core.Security (AllowedHostPorts, ecosystemArtifactAuthorities)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (sampleArtifact, sampleDetails, thingName)
import Ecluse.Test.Package.Filter (referenceResolveArtifact)
import Ecluse.Test.Rules (atDefaultPrecedence, filterPlan, inertRuleDeps, isApproved)

-- | Exercise rule decisions, survivor selection, and artifact refusal accounting.
spec :: Spec
spec = do
    survivorSpec
    decisionsSpec
    propertiesSpec
    enforceArtifactLocationsSpec
    enforceArtifactLocationsOfSpec
    resolveArtifactSpec

now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

ctx :: EvalContext
ctx = EvalContext now Nothing

policy :: [PrecededRule]
policy =
    [ atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))
    , atDefaultPrecedence DenyInstallTimeExecution
    ]

name :: PackageName
name = thingName

publishedDaysAgo :: Integer -> UTCTime
publishedDaysAgo ageDays = addUTCTime (negate (fromInteger ageDays * nominalDay)) now

detailsAt :: Text -> Integer -> Bool -> PackageDetails
detailsAt rawVer ageDays hasInstall =
    (sampleDetails name (mkVersion Npm rawVer))
        { pkgPublishedAt = Just (publishedDaysAgo ageDays)
        , pkgInstallCode = if hasInstall then RunsCodeOnInstall "postinstall" else NoCodeOnInstall
        }

infoOf :: Maybe Text -> [(Text, Integer, Bool)] -> PackageInfo
infoOf latest vs =
    PackageInfo
        { infoName = name
        , infoVersions = Map.fromList [(v, detailsAt v age install) | (v, age, install) <- vs]
        , infoDistTags = maybe Map.empty (Map.singleton "latest" . mkVersion Npm) latest
        , infoInvalidEntries = []
        }

survivorSpec :: Spec
survivorSpec = describe "fpSurvivors" $ do
    it "keeps only the approved versions, dropping a too-young one" $ do
        plan <- filterPlan inertRuleDeps ctx policy (infoOf (Just "2.0.0") [("1.0.0", 30, False), ("2.0.0", 1, False)])
        fpSurvivors plan `shouldBe` Set.singleton "1.0.0"

    it "drops a version that declares an install script even when old enough" $ do
        plan <- filterPlan inertRuleDeps ctx policy (infoOf (Just "1.0.0") [("1.0.0", 30, False), ("2.0.0", 30, True)])
        fpSurvivors plan `shouldBe` Set.singleton "1.0.0"

    it "is empty when nothing is approved" $ do
        plan <- filterPlan inertRuleDeps ctx policy (infoOf (Just "2.0.0") [("1.0.0", 1, False), ("2.0.0", 1, False)])
        fpSurvivors plan `shouldBe` Set.empty

decisionsSpec :: Spec
decisionsSpec = describe "fpDecisions" $ do
    it "is all-non-approved when nothing survives" $ do
        plan <- filterPlan inertRuleDeps ctx policy (infoOf (Just "1.0.0") [("1.0.0", 1, False), ("2.0.0", 1, True)])
        length (fpDecisions plan) `shouldBe` 2
        any isApproved (fpDecisions plan) `shouldBe` False

propertiesSpec :: Spec
propertiesSpec = describe "properties" $ do
    it "survivors are exactly the approved version keys" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan inertRuleDeps ctx policy (toInfo spec'))
            fpSurvivors plan === approvedKeys spec'

    it "decisions number one per version, all non-approved when no survivor" $
        hedgehog $ do
            spec' <- forAll genSpec
            plan <- liftIO (filterPlan inertRuleDeps ctx policy (toInfo spec'))
            length (fpDecisions plan) === length (specVersions spec')
            when (Set.null (fpSurvivors plan)) $
                assert (not (any isApproved (fpDecisions plan)))

data GenSpec = GenSpec
    { specLatest :: Maybe Text
    , specVersions :: [(Text, Integer, Bool)]
    }
    deriving stock (Show)

toInfo :: GenSpec -> PackageInfo
toInfo spec' = infoOf (specLatest spec') (specVersions spec')

approvedKeys :: GenSpec -> Set Text
approvedKeys =
    Set.fromList . map fst3 . filter (\(_, age, install) -> age >= 7 && not install) . specVersions
  where
    fst3 (a, _, _) = a

genSpec :: Gen GenSpec
genSpec = do
    n <- Gen.int (Range.linear 0 6)
    let versionStrings = take n versionPool
    triples <-
        forM versionStrings $ \v -> do
            age <- Gen.integral (Range.linear 0 60)
            install <- Gen.bool
            pure (v, age, install)
    latest <- case versionStrings of
        [] -> pure Nothing
        _ -> Just <$> Gen.element versionStrings
    pure (GenSpec latest triples)

versionPool :: [Text]
versionPool = ["1.0.0", "1.1.0", "2.0.0-rc.1", "2.0.0", "3.0.0-beta", "10.0.0"]

enforceArtifactLocationsSpec :: Spec
enforceArtifactLocationsSpec = describe "enforceArtifactLocations (served artifact locations)" $ do
    let httpsUpstream = "https://registry.npmjs.org"
        urlOf info = (\(art :| _) -> artUrl art) . pkgArtifacts <$> Map.lookup "1.0.0" (infoVersions info)
        enforce = enforceArtifactLocations noArtifactHosts

    it "upgrades a same-host http artifact URL to https (https upstream)" $
        urlOf (enforce httpsUpstream (infoWithArtifact "http://registry.npmjs.org/thing/-/thing-1.0.0.tgz"))
            `shouldBe` Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"

    it "keeps an https artifact URL on the serving authority" $
        urlOf (enforce httpsUpstream (infoWithArtifact "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"))
            `shouldBe` Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"

    for_ ["https://", "http://"] $ \scheme ->
        for_ [".. ", ". ", " "] $ \filename ->
            it ("drops and records a filename made unsafe by normalising " <> show (scheme <> filename)) $ do
                let enforced = enforce httpsUpstream (infoWithArtifact (scheme <> "registry.npmjs.org/" <> filename))
                Map.lookup "1.0.0" (infoVersions enforced) `shouldBe` Nothing
                map invalidKind (infoInvalidEntries enforced) `shouldBe` [InvalidVersionManifest]
                map invalidReason (infoInvalidEntries enforced) `shouldBe` ["artifact URL has no safe filename"]

    it "drops an https artifact URL on a foreign authority for an ecosystem declaring no artifact hosts" $ do
        let enforced = enforce httpsUpstream (infoWithArtifact "https://cdn.example.net/thing-1.0.0.tgz")
        Map.lookup "1.0.0" (infoVersions enforced) `shouldBe` Nothing
        map invalidKind (infoInvalidEntries enforced) `shouldBe` [InvalidVersionManifest]

    it "keeps an https artifact URL on a declared artifact host" $
        urlOf (enforceArtifactLocations (ecosystemArtifactAuthorities ["https://cdn.example.net"]) httpsUpstream (infoWithArtifact "https://cdn.example.net/thing-1.0.0.tgz"))
            `shouldBe` Just "https://cdn.example.net/thing-1.0.0.tgz"

    it "drops the refused file and keeps the version when another file survives" $ do
        let enforced = enforce httpsUpstream (infoWithArtifacts ("https://registry.npmjs.org/ok.whl" :| ["https://cdn.example.net/bad.whl"]))
        map artFilename . toList . pkgArtifacts <$> Map.lookup "1.0.0" (infoVersions enforced)
            `shouldBe` Just ["ok.whl"]
        map invalidKind (infoInvalidEntries enforced) `shouldBe` [InvalidIndexFile]
        map invalidKey (infoInvalidEntries enforced) `shouldBe` ["bad.whl"]

    it "drops a foreign-host http artifact URL and records it (https upstream)" $ do
        let enforced = enforce httpsUpstream (infoWithArtifact "http://evil.example.test/thing-1.0.0.tgz")
        Map.lookup "1.0.0" (infoVersions enforced) `shouldBe` Nothing
        map invalidKind (infoInvalidEntries enforced) `shouldBe` [InvalidVersionManifest]

    it "records the dropped artifact's authority, never its URL (the value reaches a log line)" $ do
        let enforced = enforce httpsUpstream (infoWithArtifact credentialedTarball)
        map invalidValue (infoInvalidEntries enforced) `shouldBe` [String "evil.test:443"]
        droppedText enforced `shouldSatisfy` (not . T.isInfixOf "hunter2")
        droppedText enforced `shouldSatisfy` (not . T.isInfixOf "sig=abc")

    it "keeps the credential out of the drop reason as well as the value" $
        map invalidReason (infoInvalidEntries (enforce httpsUpstream (infoWithArtifact credentialedTarball)))
            `shouldBe` ["dist.tarball is http on a host other than the upstream registry: evil.test:443"]

    for_ credentialedSpellings $ \(label, url) ->
        it ("reduces a " <> label <> " artifact URL, which carries no scheme to key on") $ do
            let enforced = enforce httpsUpstream (infoWithArtifact url)
            droppedText enforced `shouldSatisfy` (not . T.isInfixOf "hunter2")
            droppedText enforced `shouldSatisfy` (not . T.isInfixOf "sig=abc")

    it "keeps a same-authority artifact URL for a non-https (loopback) upstream" $
        urlOf (enforce "http://127.0.0.1:8080" (infoWithArtifact "http://127.0.0.1:8080/thing/-/thing-1.0.0.tgz"))
            `shouldBe` Just "http://127.0.0.1:8080/thing/-/thing-1.0.0.tgz"

    it "still checks the authority for a non-https upstream, which the download gate also does" $
        Map.lookup "1.0.0" (infoVersions (enforce "http://127.0.0.1:8080" (infoWithArtifact "http://evil.example.test/thing-1.0.0.tgz")))
            `shouldBe` Nothing

enforceArtifactLocationsOfSpec :: Spec
enforceArtifactLocationsOfSpec = describe "enforceArtifactLocationsOf (single-version form)" $ do
    let httpsUpstream = "https://registry.npmjs.org"
        urlOf = fmap ((\(art :| _) -> artUrl art) . pkgArtifacts)
        enforce = enforceArtifactLocationsOf noArtifactHosts

    it "upgrades a same-host http artifact URL to https" $
        urlOf (enforce httpsUpstream (detailsWithArtifact "http://registry.npmjs.org/thing/-/thing-1.0.0.tgz"))
            `shouldBe` Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"

    it "drops the version when its artifact URL is http on a foreign host" $
        enforce httpsUpstream (detailsWithArtifact "http://evil.example.test/thing-1.0.0.tgz")
            `shouldBe` Nothing

    it "drops the version when its artifact authority is neither the origin nor a declared host" $
        enforce httpsUpstream (detailsWithArtifact "https://cdn.example.net/thing-1.0.0.tgz")
            `shouldBe` Nothing

    for_ ["a\\..\\..\\x", ".", "..", ""] $ \filename ->
        it ("drops the sole artifact with refused filename " <> show filename) $
            enforce httpsUpstream (detailsWithArtifact (httpsUpstream <> "/" <> filename))
                `shouldBe` Nothing

    for_ ["https://", "http://"] $ \scheme ->
        for_ [".. ", ". ", " "] $ \filename ->
            it ("drops a selective artifact made unsafe by normalising " <> show (scheme <> filename)) $
                enforce httpsUpstream (detailsWithArtifact (scheme <> "registry.npmjs.org/" <> filename))
                    `shouldBe` Nothing

    it "records a filename refusal without exposing a signed query" $ do
        let kept = enforceArtifactLocations noArtifactHosts httpsUpstream (infoWithArtifact (httpsUpstream <> "/..?sig=secret"))
        map invalidReason (infoInvalidEntries kept) `shouldBe` ["artifact URL has no safe filename"]
        map invalidValue (infoInvalidEntries kept) `shouldBe` [String "registry.npmjs.org:443"]

    it "keeps a same-authority artifact URL for a non-https (loopback) upstream" $
        urlOf (enforce "http://127.0.0.1:8080" (detailsWithArtifact "http://127.0.0.1:8080/thing-1.0.0.tgz"))
            `shouldBe` Just "http://127.0.0.1:8080/thing-1.0.0.tgz"

resolveArtifactSpec :: Spec
resolveArtifactSpec = describe "resolveArtifact (against the per-artifact reference)" $
    modifyMaxSuccess (const 5000) $
        it "returns what the reference returns, on generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                url <- forAll (genHostileUrl served)
                let hosts = ecosystemArtifactAuthorities hostUrls
                    art = sampleArtifact{artUrl = url}
                    expected = referenceResolveArtifact hosts upstreamBaseUrl art
                    refusal = leftToMaybe expected
                    refusedFor reason = maybe False (T.isPrefixOf reason . refusedReason) refusal
                    refusedAsWritten = fmap refusedUrl refusal == Just url
                cover 5 "kept as written" (fmap artUrl expected == Right url)
                cover 2 "kept with its scheme upgraded" (maybe False ((/= url) . artUrl) (rightToMaybe expected))
                cover 5 "refused for its filename as written" (refusedFor "artifact URL has no safe filename" && refusedAsWritten)
                cover 1 "refused for its filename once normalised" (refusedFor "artifact URL has no safe filename" && not refusedAsWritten)
                cover 5 "refused for its scheme" (refusedFor "dist.tarball is")
                cover 5 "refused for its authority" (refusedFor "artifact authority")
                resolveArtifact (artifactOrigin hosts upstreamBaseUrl) art === expected

{- Upstream base URLs, respelled and loopback included, each with its ecosystem's artifact hosts and
authority spellings its reads honour. -}
upstreams :: [(Text, [Text], [Text])]
upstreams =
    [ ("https://registry.npmjs.org", npmArtifactHosts, ["registry.npmjs.org", "REGISTRY.npmjs.ORG", "registry.npmjs.org:443"])
    , ("HTTPS://Registry.NPMJS.org", npmArtifactHosts, ["registry.npmjs.org", "Registry.NPMJS.org:443"])
    , ("https://pypi.org/simple", pypiArtifactHosts, ["pypi.org", "files.pythonhosted.org", "Files.PythonHosted.org:443"])
    , ("https://pypi.org:443/simple", [], ["pypi.org", "PyPI.org:443"])
    , ("https://[::1]:8443", ["https://files.pythonhosted.org"], ["[::1]:8443", "files.pythonhosted.org"])
    , ("http://127.0.0.1:8080", [], ["127.0.0.1:8080"])
    ]

-- A URL assembled from hostile spellings of each part, with an occasional stray character.
genHostileUrl :: [Text] -> Gen Text
genHostileUrl served = do
    leading <- Gen.frequency [(9, pure ""), (1, Gen.element whitespace)]
    scheme <- Gen.frequency [(5, respell "https://"), (3, respell "http://"), (1, Gen.element oddSchemes)]
    userinfo <- Gen.frequency [(6, pure ""), (1, Gen.element ["deploy:hunter2@", "user@", "a@b@", "@", "%40@"])]
    authority <- Gen.frequency [(3, Gen.element served), (1, (<>) <$> Gen.element otherHosts <*> Gen.element ports)]
    directory <- Gen.element ["", "/thing/-", "/packages/ab/cd", "/%2e%2e", "/a b"]
    file <- Gen.frequency [(5, Gen.element plainFiles), (2, Gen.element hostileFiles)]
    trailing <- Gen.frequency [(9, pure ""), (1, Gen.element whitespace)]
    let url = leading <> scheme <> userinfo <> authority <> directory <> file <> trailing
    Gen.frequency [(8, pure url), (1, strayChar url)]
  where
    respell = fmap toText . traverse (\c -> Gen.element (ordNub [c, toUpper c])) . T.unpack
    strayChar url = do
        at <- Gen.int (Range.linear 0 (T.length url))
        stray <- Gen.unicode
        pure (T.take at url <> T.singleton stray <> T.drop at url)
    whitespace = [" ", "\t", "\n", "\xA0", "\x3000"]
    oddSchemes = ["", "//", "ftp://", "https:/", "https:", "HTTPS//", "\x130https://", "\x212Ahttps://", "https\xFF1A//", "http\xFF1A//"]
    otherHosts = ["registry.npmjs.org", "pypi.org", "files.pythonhosted.org", "[::1]", "127.0.0.1", "cdn.example.net", "evil.test", "[2606:4700::1111]", "[::1", "b\xFC\&cher.example", "xn--bcher-kva.example", "", "169.254.169.254"]
    ports = ["", ":443", ":8443", ":8080", ":", ":0", ":0443", ":65536", ":\xFF18\xFF10"]
    plainFiles = ["/thing-1.0.0.tgz", "/numpy-2.0.0-cp312-cp312-manylinux_2_17_x86_64.whl", "/requests-2.32.3.tar.gz", "/%C3%BCber-1.0.tar.gz", "/\xFC\&ber-1.0.tar.gz"]
    hostileFiles = ["/", "", "/.", "/..", "/%2e%2e", "/a%2Fb.whl", "/pkg%00.tgz", "/%E2%80%AE.whl", "/%ff.whl", "/a\\b.tgz", "/x.tgz?sig=abc", "/x.tgz#frag", "/?q", "/ ", "/. ", "/.. ", "/x.tgz\t"]

noArtifactHosts :: AllowedHostPorts
noArtifactHosts = ecosystemArtifactAuthorities []

credentialedTarball :: Text
credentialedTarball = "http://deploy:hunter2@evil.test/x?sig=abc"

credentialedSpellings :: [(String, Text)]
credentialedSpellings =
    [ ("scheme-less", "deploy:hunter2@evil.test/x?sig=abc")
    , ("protocol-relative", "//deploy:hunter2@evil.test/x?sig=abc")
    ]

droppedText :: PackageInfo -> Text
droppedText = show . infoInvalidEntries

infoWithArtifact :: Text -> PackageInfo
infoWithArtifact url =
    PackageInfo
        { infoName = name
        , infoVersions = Map.singleton "1.0.0" (detailsWithArtifact url)
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

detailsWithArtifact :: Text -> PackageDetails
detailsWithArtifact url =
    (detailsAt "1.0.0" 30 False){pkgArtifacts = sampleArtifact{artUrl = url} :| []}

infoWithArtifacts :: NonEmpty Text -> PackageInfo
infoWithArtifacts urls =
    PackageInfo
        { infoName = name
        , infoVersions = Map.singleton "1.0.0" details
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }
  where
    details = (detailsAt "1.0.0" 30 False){pkgArtifacts = fmap artifactAt urls}
    artifactAt url = sampleArtifact{artUrl = url, artFilename = T.takeWhileEnd (/= '/') url}
