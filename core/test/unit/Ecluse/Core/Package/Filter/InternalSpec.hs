-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The location check and its drop records, held to their references on generated hostile URLs.
module Ecluse.Core.Package.Filter.InternalSpec (spec) where

import Data.List (partition, zipWith3)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (Artifact (artUrl), PackageDetails (pkgArtifacts), PackageInfo (infoInvalidEntries, infoVersions))
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Package.Filter.Internal (
    ArtifactLocation (..),
    ArtifactOrigin,
    ArtifactRefusal (refusedReason, refusedUrl),
    LocationRefusal (..),
    artifactOrigin,
    locateArtifact,
    noAuthorityVerdict,
    partitionArtifacts,
    resolveArtifact,
 )
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Registry.ServedDocument (rebaseArtifactUrl)
import Ecluse.Core.Security (
    AuthorityText,
    HostPort (hpHost, hpPort),
    artifactAuthorityHonoured,
    authorityHostPort,
    authorityText,
    ecosystemArtifactAuthorities,
    parseIpLiteral,
 )
import Ecluse.Core.Text (afterFirst, urlFilename)
import Ecluse.Package.Filter.Support (genHostileInfo, genHostileUrl, referenceEnforceArtifactLocations, referenceResolveArtifact, upstreams)
import Ecluse.Test.Package (sampleArtifact)

-- | The check returns what the per-artifact reference returns, and records what the reference records.
spec :: Spec
spec = do
    resolveArtifactSpec
    locateArtifactSpec
    carriedVerdictSpec
    partitionArtifactsSpec

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
                    afterAuthority = T.dropWhile (`notElem` ['/', '?', '#']) (afterFirst "://" url)
                cover 5 "kept as written" (fmap artUrl expected == Right url)
                cover 2 "kept with its scheme upgraded" (maybe False ((/= url) . artUrl) (rightToMaybe expected))
                cover 5 "refused for its filename as written" (refusedFor "artifact URL has no safe filename" && refusedAsWritten)
                cover 1 "refused for its filename once normalised" (refusedFor "artifact URL has no safe filename" && not refusedAsWritten)
                cover 5 "refused for its scheme" (refusedFor "dist.tarball is")
                cover 5 "refused for its authority" (refusedFor "artifact authority")
                cover 2 "a query or fragment ends the authority" (T.take 1 afterAuthority `elem` ["?", "#"])
                cover 2 "an @ follows the end of the authority" ("@" `T.isInfixOf` afterAuthority)
                snd (resolveArtifact (artifactOrigin hosts upstreamBaseUrl) noAuthorityVerdict art) === expected

locateArtifactSpec :: Spec
locateArtifactSpec = describe "locateArtifact" $ do
    it "keeps a URL as written when normalising leaves its text unchanged" $
        locateAlone npmOrigin "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            `shouldBe` Right (ArtifactLocation "thing-1.0.0.tgz" Nothing True)

    it "returns the normalised URL beside the filename as written, when trimming changed the text" $
        locateAlone npmOrigin "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz "
            `shouldBe` Right (ArtifactLocation "thing-1.0.0.tgz " (Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz") True)

    it "reports a URL that names no file once trimmed, which a non-https upstream keeps as written" $
        locateAlone loopbackOrigin "http://127.0.0.1:8080/. "
            `shouldBe` Right (ArtifactLocation ". " Nothing False)

    it "refuses with the failing test's reason and the text that test read" $
        locateAlone npmOrigin "http://registry.npmjs.org/thing/-/. "
            `shouldBe` Left (LocationRefusal "artifact URL has no safe filename" "https://registry.npmjs.org/thing/-/.")

    it "keeps a PyPI file on a declared artifact host, which is not the index's own authority" $
        locateAlone pypiOrigin "https://files.pythonhosted.org/packages/ab/cd/requests-2.32.3-py3-none-any.whl"
            `shouldBe` Right (ArtifactLocation "requests-2.32.3-py3-none-any.whl" Nothing True)

    modifyMaxSuccess (const 5000) $
        it "names the file urlFilename names and decides the trimmed name as rebaseArtifactUrl does, on generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                url <- forAll (genHostileUrl served)
                let located = locateAlone (artifactOrigin (ecosystemArtifactAuthorities hostUrls) upstreamBaseUrl) url
                cover 5 "located" (isRight located)
                cover 2 "located with text that trimming changes" (isRight located && T.strip url /= url)
                cover 0.1 "located with no file named once trimmed" (fmap locatedTrimmedNamesFile located == Right False)
                whenRight_ located $ \location -> do
                    Just (locatedFilename location) === urlFilename url
                    locatedTrimmedNamesFile location === isJust (rebaseArtifactUrl Just url)

carriedVerdictSpec :: Spec
carriedVerdictSpec = describe "the authority verdict carried from one URL to the next" $ do
    modifyMaxSuccess (const 5000) $
        it "returns what it returns for each URL alone, on lists of generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                pool <- forAll (Gen.list (Range.constant 1 4) (genHostileUrl served))
                urls <- forAll (Gen.list (Range.constant 2 12) (Gen.frequency [(2, Gen.element pool), (1, genHostileUrl served)]))
                let origin = artifactOrigin (ecosystemArtifactAuthorities hostUrls) upstreamBaseUrl
                    alone = map (locateAlone origin) urls
                    (reused, replaced) = consecutiveVerdicts (catMaybes (zipWith3 (decided 0) [0 ..] urls alone))
                cover 20 "a verdict reused" (not (null reused))
                cover 5 "an honoured verdict reused" (any (decidedHonoured . snd) reused)
                cover 5 "a refusing verdict reused" (not (all (decidedHonoured . snd) reused))
                cover 5 "a verdict reused by a different URL" (any (\(earlier, later) -> decidedUrl earlier /= decidedUrl later) reused)
                cover 5 "a verdict reused past a URL that never reached the authority test" (any (\(earlier, later) -> decidedPosition later - decidedPosition earlier > 1) reused)
                cover 20 "a verdict replaced" (not (null replaced))
                cover 5 "a verdict replaced by the opposite one" (any (\(earlier, later) -> decidedHonoured earlier /= decidedHonoured later) replaced)
                snd (mapAccumL (locateArtifact origin) noAuthorityVerdict urls) === alone

    modifyMaxSuccess (const 5000) $
        it "keeps and records what the reference enforcement does, over documents whose files share authorities" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                info <- forAll (genHostileInfo served)
                let hosts = ecosystemArtifactAuthorities hostUrls
                    (reused, replaced) = consecutiveVerdicts (decidedInCheckOrder (artifactOrigin hosts upstreamBaseUrl) info)
                cover 10 "an honoured verdict reused" (any (decidedHonoured . snd) reused)
                cover 5 "a refusing verdict reused" (not (all (decidedHonoured . snd) reused))
                cover 5 "a verdict reused under other userinfo" (any (\(earlier, later) -> decidedSpan earlier /= decidedSpan later) reused)
                cover 5 "a verdict reused by the next version checked" (any (\(earlier, later) -> decidedVersion earlier /= decidedVersion later) reused)
                cover 2 "a verdict reused on an IP literal" (any (ipLiteral . snd) reused)
                cover 1 "a verdict replaced by the same host on another port" (any sameHostOtherPort replaced)
                cover 2 "a verdict replaced by another spelling of the same dial target" (any sameDialTarget replaced)
                cover 1 "a verdict replaced by another spelling of an IP literal" (any (\pair -> sameDialTarget pair && all ipLiteral pair && differentHosts pair) replaced)
                cover 5 "a verdict replaced by the opposite one" (any (\(earlier, later) -> decidedHonoured earlier /= decidedHonoured later) replaced)
                enforceArtifactLocations hosts upstreamBaseUrl info === referenceEnforceArtifactLocations hosts upstreamBaseUrl info
  where
    target = authorityHostPort . decidedAuthority
    ipLiteral = maybe False (isJust . parseIpLiteral . hpHost) . target
    sameDialTarget (earlier, later) = artifactAuthorityHonoured (ecosystemArtifactAuthorities []) (target earlier) (target later)
    differentHosts (earlier, later) = fmap hpHost (target earlier) /= fmap hpHost (target later)
    sameHostOtherPort (earlier, later) = case (target earlier, target later) of
        (Just a, Just b) -> hpHost a == hpHost b && hpPort a /= hpPort b
        _ -> False

-- A URL whose check reached the authority test, with the authority it read and the verdict.
data Decided = Decided
    { decidedVersion :: Int
    , decidedPosition :: Int
    , decidedUrl :: Text
    , decidedSpan :: Text
    -- ^ The authority span of the URL the test read, userinfo included.
    , decidedAuthority :: AuthorityText
    , decidedHonoured :: Bool
    }

decided :: Int -> Int -> Text -> Either LocationRefusal ArtifactLocation -> Maybe Decided
decided version position url = \case
    Right location -> Just (reached (fromMaybe url (locatedNormalised location)) True)
    Left refusal
        | "artifact authority" `T.isPrefixOf` unlocatedReason refusal -> Just (reached (unlocatedUrl refusal) False)
        | otherwise -> Nothing
  where
    reached normalised = Decided version position url (T.takeWhile (`notElem` ['/', '?', '#']) (afterFirst "://" normalised)) (authorityText normalised)

-- The URLs that reached the authority test, in the order the document check visits them: the last version first.
decidedInCheckOrder :: ArtifactOrigin -> PackageInfo -> [Decided]
decidedInCheckOrder origin info = catMaybes (zipWith check [0 ..] urls)
  where
    urls = [(version, artUrl art) | (version, details) <- zip [0 ..] (reverse (Map.elems (infoVersions info))), art <- toList (pkgArtifacts details)]
    check position (version, url) = decided version position url (locateAlone origin url)

-- Consecutive decided URLs, split into those that share an authority text and those that do not.
consecutiveVerdicts :: [Decided] -> ([(Decided, Decided)], [(Decided, Decided)])
consecutiveVerdicts reached = partition (\(earlier, later) -> decidedAuthority earlier == decidedAuthority later) (zip reached (drop 1 reached))

partitionArtifactsSpec :: Spec
partitionArtifactsSpec = describe "partitionArtifacts (against the reference enforcement)" $
    modifyMaxSuccess (const 1000) $
        it "keeps and records what the reference does, when a read settles each version in key order" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                info <- forAll (genHostileInfo served)
                let hosts = ecosystemArtifactAuthorities hostUrls
                    expected = referenceEnforceArtifactLocations hosts upstreamBaseUrl info
                    settled = Map.mapWithKey (settledVersion (artifactOrigin hosts upstreamBaseUrl)) (infoVersions info)
                cover 5 "a version left with no file" (any (isNothing . fst) settled)
                cover 5 "a refused file beside a kept one" (any (\(survivors, drops) -> isJust survivors && not (null drops)) settled)
                cover 1 "a version kept whole" (any (null . snd) settled)
                Map.mapMaybe fst settled === Map.map pkgArtifacts (infoVersions expected)
                infoInvalidEntries info <> concatMap snd (Map.elems settled) === infoInvalidEntries expected
  where
    settledVersion origin rawVersion details = partitionArtifacts rawVersion (map (snd . resolveArtifact origin noAuthorityVerdict) (toList (pkgArtifacts details)))

-- The check of one URL with no earlier verdict.
locateAlone :: ArtifactOrigin -> Text -> Either LocationRefusal ArtifactLocation
locateAlone origin = snd . locateArtifact origin noAuthorityVerdict

npmOrigin :: ArtifactOrigin
npmOrigin = artifactOrigin (ecosystemArtifactAuthorities []) "https://registry.npmjs.org"

pypiOrigin :: ArtifactOrigin
pypiOrigin = artifactOrigin (ecosystemArtifactAuthorities pypiArtifactHosts) "https://pypi.org/simple"

loopbackOrigin :: ArtifactOrigin
loopbackOrigin = artifactOrigin (ecosystemArtifactAuthorities []) "http://127.0.0.1:8080"
