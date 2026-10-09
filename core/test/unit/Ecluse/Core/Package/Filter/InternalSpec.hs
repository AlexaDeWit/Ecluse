-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The location check and its drop records, held to their references on generated hostile URLs.
module Ecluse.Core.Package.Filter.InternalSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (Artifact (artUrl), PackageDetails (pkgArtifacts), PackageInfo (infoInvalidEntries, infoVersions))
import Ecluse.Core.Package.Filter.Internal (
    ArtifactLocation (..),
    ArtifactOrigin,
    ArtifactRefusal (refusedReason, refusedUrl),
    LocationRefusal (..),
    artifactOrigin,
    locateArtifact,
    locateArtifacts,
    partitionArtifacts,
    resolveArtifact,
 )
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Registry.ServedDocument (rebaseArtifactUrl)
import Ecluse.Core.Security (AuthorityText, authorityText, ecosystemArtifactAuthorities)
import Ecluse.Core.Text (afterFirst, urlFilename)
import Ecluse.Package.Filter.Support (genHostileInfo, genHostileUrl, referenceEnforceArtifactLocations, referenceResolveArtifact, upstreams)
import Ecluse.Test.Package (sampleArtifact)

-- | The check returns what the per-artifact reference returns, and records what the reference records.
spec :: Spec
spec = do
    resolveArtifactSpec
    locateArtifactSpec
    locateArtifactsSpec
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
                resolveArtifact (artifactOrigin hosts upstreamBaseUrl) art === expected

locateArtifactSpec :: Spec
locateArtifactSpec = describe "locateArtifact" $ do
    it "keeps a URL as written when normalising leaves its text unchanged" $
        locateArtifact npmOrigin "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            `shouldBe` Right (ArtifactLocation "thing-1.0.0.tgz" Nothing True)

    it "returns the normalised URL beside the filename as written, when trimming changed the text" $
        locateArtifact npmOrigin "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz "
            `shouldBe` Right (ArtifactLocation "thing-1.0.0.tgz " (Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz") True)

    it "reports a URL that names no file once trimmed, which a non-https upstream keeps as written" $
        locateArtifact loopbackOrigin "http://127.0.0.1:8080/. "
            `shouldBe` Right (ArtifactLocation ". " Nothing False)

    it "refuses with the failing test's reason and the text that test read" $
        locateArtifact npmOrigin "http://registry.npmjs.org/thing/-/. "
            `shouldBe` Left (LocationRefusal "artifact URL has no safe filename" "https://registry.npmjs.org/thing/-/.")

    it "keeps a PyPI file on a declared artifact host, which is not the index's own authority" $
        locateArtifact pypiOrigin "https://files.pythonhosted.org/packages/ab/cd/requests-2.32.3-py3-none-any.whl"
            `shouldBe` Right (ArtifactLocation "requests-2.32.3-py3-none-any.whl" Nothing True)

    modifyMaxSuccess (const 5000) $
        it "names the file urlFilename names and decides the trimmed name as rebaseArtifactUrl does, on generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                url <- forAll (genHostileUrl served)
                let located = locateArtifact (artifactOrigin (ecosystemArtifactAuthorities hostUrls) upstreamBaseUrl) url
                cover 5 "located" (isRight located)
                cover 2 "located with text that trimming changes" (isRight located && T.strip url /= url)
                cover 0.1 "located with no file named once trimmed" (fmap locatedTrimmedNamesFile located == Right False)
                whenRight_ located $ \location -> do
                    Just (locatedFilename location) === urlFilename url
                    locatedTrimmedNamesFile location === isJust (rebaseArtifactUrl Just url)

locateArtifactsSpec :: Spec
locateArtifactsSpec = describe "locateArtifacts (against locateArtifact for each URL)" $
    modifyMaxSuccess (const 5000) $
        it "returns what locateArtifact returns for each URL, on lists of generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                pool <- forAll (Gen.list (Range.constant 1 4) (genHostileUrl served))
                urls <- forAll (Gen.list (Range.constant 2 12) (Gen.frequency [(2, Gen.element pool), (1, genHostileUrl served)]))
                let origin = artifactOrigin (ecosystemArtifactAuthorities hostUrls) upstreamBaseUrl
                    perUrl = map (locateArtifact origin) urls
                    decided = [reached | (position, url, located) <- zip3 [0 ..] urls perUrl, Just reached <- [decidedAuthority position url located]]
                    steps = zip decided (drop 1 decided)
                    reused = [(earlier, later) | (earlier, later) <- steps, daAuthority earlier == daAuthority later]
                    replaced = [(earlier, later) | (earlier, later) <- steps, daAuthority earlier /= daAuthority later]
                cover 20 "a verdict reused" (not (null reused))
                cover 5 "an honoured verdict reused" (any (daHonoured . snd) reused)
                cover 5 "a refusing verdict reused" (not (all (daHonoured . snd) reused))
                cover 5 "a verdict reused by a different URL" (any (\(earlier, later) -> daUrl earlier /= daUrl later) reused)
                cover 5 "a verdict reused past a URL that never reached the authority test" (any (\(earlier, later) -> daPosition later - daPosition earlier > 1) reused)
                cover 20 "a verdict replaced" (not (null replaced))
                cover 5 "a verdict replaced by the opposite one" (any (\(earlier, later) -> daHonoured earlier /= daHonoured later) replaced)
                locateArtifacts origin urls === perUrl

-- A URL whose check reached the authority test: where it sits, the authority text it read, and the verdict.
data DecidedAuthority = DecidedAuthority
    { daPosition :: Int
    , daUrl :: Text
    , daAuthority :: AuthorityText
    , daHonoured :: Bool
    }

decidedAuthority :: Int -> Text -> Either LocationRefusal ArtifactLocation -> Maybe DecidedAuthority
decidedAuthority position url = \case
    Right location -> Just (DecidedAuthority position url (authorityText (fromMaybe url (locatedNormalised location))) True)
    Left refusal
        | "artifact authority" `T.isPrefixOf` unlocatedReason refusal -> Just (DecidedAuthority position url (authorityText (unlocatedUrl refusal)) False)
        | otherwise -> Nothing

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
    settledVersion origin rawVersion details = partitionArtifacts rawVersion (map (resolveArtifact origin) (toList (pkgArtifacts details)))

npmOrigin :: ArtifactOrigin
npmOrigin = artifactOrigin (ecosystemArtifactAuthorities []) "https://registry.npmjs.org"

pypiOrigin :: ArtifactOrigin
pypiOrigin = artifactOrigin (ecosystemArtifactAuthorities pypiArtifactHosts) "https://pypi.org/simple"

loopbackOrigin :: ArtifactOrigin
loopbackOrigin = artifactOrigin (ecosystemArtifactAuthorities []) "http://127.0.0.1:8080"
