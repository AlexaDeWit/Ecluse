-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.PackageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (sampleDetails)

spec :: Spec
spec = do
    describe "Scope" $ do
        it "mkScope strips a leading '@'" $
            unScope (mkScope "@myorg") `shouldBe` "myorg"
        it "mkScope leaves an unprefixed scope unchanged" $
            unScope (mkScope "myorg") `shouldBe` "myorg"
        it "renderScope adds the leading '@'" $
            renderScope (mkScope "@myorg") `shouldBe` "@myorg"
        it "normalises scopes regardless of a leading '@'" $
            hedgehog $ do
                s <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
                mkScope ("@" <> s) === mkScope s

    describe "mkPackageName" $ do
        it "renders a scoped npm package as @scope/name" $
            renderPackageName (mkPackageName Npm (Just (mkScope "myorg")) "thing")
                `shouldBe` "@myorg/thing"
        it "renders an unscoped package as just the name" $
            renderPackageName (mkPackageName Npm Nothing "thing") `shouldBe` "thing"
        it "keeps npm canonical names verbatim (case-sensitive)" $
            pkgCanonical (mkPackageName Npm Nothing "Thing") `shouldBe` "Thing"
        it "normalises PyPI names per PEP 503" $
            pkgCanonical (mkPackageName PyPI Nothing "Flask_Thing.X")
                `shouldBe` "flask-thing-x"
        it "treats PyPI names equal up to normalisation" $
            mkPackageName PyPI Nothing "Flask" `shouldBe` mkPackageName PyPI Nothing "flask"

    describe "unscopedName / pkgBaseName" $ do
        it "drops the @scope/ prefix of a scoped name" $
            unscopedName (mkPackageName Npm (Just (mkScope "babel")) "code-frame")
                `shouldBe` "code-frame"
        it "is the whole name for an unscoped package" $
            unscopedName (mkPackageName Npm Nothing "left-pad") `shouldBe` "left-pad"
        it "reads the stored base field (no display-slicing round-trip)" $
            pkgBaseName (mkPackageName Npm (Just (mkScope "babel")) "code-frame")
                `shouldBe` "code-frame"
        it "does not enter identity: two names differing only in base are still equatable by identity" $
            -- The base name is carried but excluded from 'nameKey', like the display form:
            -- equality stays on (ecosystem, namespace, canonical), so a name equals itself
            -- regardless of how the base is read back.
            mkPackageName Npm (Just (mkScope "babel")) "code-frame"
                `shouldBe` mkPackageName Npm (Just (mkScope "babel")) "code-frame"

    describe "PackageInfo" $ do
        -- A packument-level fixture: one package, one published version "1.0.0"
        -- tagged "latest", carrying its own publish time on the version snapshot. The
        -- map is keyed by the raw version string, as the type documents.
        let name = mkPackageName Npm Nothing "thing"
            version = mkVersion Npm "1.0.0"
            publishedAt = UTCTime (fromGregorian 2026 6 21) 0
            versionDetails = (sampleDetails name version){pkgLicenses = ["MIT"], pkgPublishedAt = Just publishedAt}
            info =
                PackageInfo
                    { infoName = name
                    , infoVersions = Map.singleton "1.0.0" versionDetails
                    , infoDistTags = Map.singleton "latest" version
                    , infoInvalidEntries = []
                    }
        it "round-trips the package identity through infoName" $
            infoName info `shouldBe` name
        it "retrieves a version's details by its raw version-string key" $
            -- The version put in under "1.0.0" is the one that comes back out, and
            -- it still carries its own parsed 'Version' (the map key is just Text).
            (pkgVersion <$> Map.lookup "1.0.0" (infoVersions info)) `shouldBe` Just version
        it "resolves a dist-tag to the version it points at" $
            Map.lookup "latest" (infoDistTags info) `shouldBe` Just version
        it "carries the per-version publish time on the version snapshot" $
            -- The publish time is folded onto the version's own 'PackageDetails', not a
            -- sibling map; the npm wire @time@ object is reconstructed at serialisation.
            (pkgPublishedAt <$> Map.lookup "1.0.0" (infoVersions info)) `shouldBe` Just (Just publishedAt)
        it "is equal exactly when every field agrees" $ do
            -- Equality is structural over all fields: an identically-built document is
            -- equal, and changing a single field (here the version a dist-tag resolves
            -- to) makes it unequal.
            info `shouldBe` info{infoDistTags = Map.singleton "latest" version}
            info `shouldNotBe` info{infoDistTags = Map.singleton "latest" (mkVersion Npm "2.0.0")}
