-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The location check and its drop records, held to their references on generated hostile URLs.
module Ecluse.Core.Package.Filter.InternalSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
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
    partitionArtifacts,
    resolveArtifact,
 )
import Ecluse.Core.Registry.ServedDocument (rebaseArtifactUrl)
import Ecluse.Core.Security (ecosystemArtifactAuthorities)
import Ecluse.Core.Text (afterFirst, urlFilename)
import Ecluse.Package.Filter.Support (genHostileInfo, genHostileUrl, referenceEnforceArtifactLocations, referenceResolveArtifact, upstreams)
import Ecluse.Test.Package (sampleArtifact)

-- | The check returns what the per-artifact reference returns, and records what the reference records.
spec :: Spec
spec = do
    resolveArtifactSpec
    locateArtifactSpec
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

    modifyMaxSuccess (const 5000) $
        it "names the file urlFilename names and decides the trimmed name as rebaseArtifactUrl does, on generated hostile URLs" $
            hedgehog $ do
                (upstreamBaseUrl, hostUrls, served) <- forAll (Gen.element upstreams)
                url <- forAll (genHostileUrl served)
                let located = locateArtifact (artifactOrigin (ecosystemArtifactAuthorities hostUrls) upstreamBaseUrl) url
                cover 5 "located" (isRight located)
                whenRight_ located $ \location -> do
                    Just (locatedFilename location) === urlFilename url
                    locatedTrimmedNamesFile location === isJust (rebaseArtifactUrl Just url)

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

loopbackOrigin :: ArtifactOrigin
loopbackOrigin = artifactOrigin (ecosystemArtifactAuthorities []) "http://127.0.0.1:8080"
