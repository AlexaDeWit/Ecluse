-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Profiles preserve complete native rows and cannot advertise a selected run as full coverage.
module Ecluse.BenchLoad.ProfileSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Profile
import Ecluse.BenchLoad.Scenarios (findScenario, runsUnder)
import Ecluse.BenchLoad.Support (twoCores)

-- | Exercise real scenario registrations without starting load or reading captures.
spec :: Spec
spec = describe "load profiles" $ do
    it "round trips every workflow profile name" $
        for_ [Full, ReadPaths, MemoryAdmission, Artifacts] $ \profile ->
            parseLoadProfile (renderLoadProfile profile) `shouldBe` Right profile
    it "rejects an unknown or empty profile" $
        for_ ["", "read", "nightly", "../full"] $ \name ->
            parseLoadProfile name `shouldSatisfy` isLeft
    it "keeps full runs unselected for existing floor enforcement" $
        selectProfile Full Nothing `shouldBe` ("full", Nothing)
    it "keeps explicit selections ahead of every profile" $
        for_ [Full, ReadPaths, MemoryAdmission, Artifacts] $ \profile ->
            selectProfile profile (Just ["npm/merge-cold"]) `shouldBe` ("custom", Just ["npm/merge-cold"])
    it "retains both cache controls and every managed PyPI read in read paths" $ do
        let keys = selected ReadPaths
        keys `shouldContain` ["npm/merge-cold", "npm/merge-cold-advisories", "npm/heavy-private"]
        for_ ["npm", "pypi", "cargo"] $ \ecosystem ->
            for_ ["assembled-response-hit", "revalidate-not-modified", "cache-fits-large", "cache-evicts-large"] $ \name ->
                keys `shouldSatisfy` elem (ecosystem <> "/" <> name)
        keys `shouldContain` ["pypi/mirror-native-baseline", "pypi/mirror-managed-warm", "pypi/mirror-managed-revision"]
        length (filter (not . T.isPrefixOf "rubygems/") keys) `shouldBe` 24
        filter (T.isPrefixOf "rubygems/") keys
            `shouldBe` [ "rubygems/info-cold"
                       , "rubygems/info-cold-advisories"
                       , "rubygems/heavy-private"
                       , "rubygems/info-repeated-corpus"
                       , "rubygems/revalidate-not-modified"
                       , "rubygems/info-working-set-one"
                       , "rubygems/info-working-set-many"
                       , "rubygems/quick-gemspec"
                       ]
        length keys `shouldBe` 32
    it "keeps slow clients, bursts under private load and the full admission ramp" $
        for_ ["npm", "pypi", "cargo", "rubygems"] $ \ecosystem ->
            [name | key <- selected MemoryAdmission, Just name <- [T.stripPrefix (ecosystem <> "/") key]]
                `shouldMatchList` ["heavy-private", "slow-readers", "herd", "herd-under-heavy-private", "warm-under-cold", "ramp"]
    it "keeps private, public onboarding and relay-ceiling artifact comparisons" $
        selected Artifacts
            `shouldMatchList` ["npm/tarball-hot-path", "npm/tarball-onboarding", "npm/tarball-ceiling", "pypi/wheel-hot-path", "pypi/wheel-onboarding", "pypi/wheel-ceiling", "cargo/crate-hot-path", "cargo/crate-onboarding", "cargo/crate-ceiling", "rubygems/gem-hot-path", "rubygems/gem-onboarding", "rubygems/gem-ceiling"]
    it "resolves every selected row under the manual default shape without duplicates" $
        for_ [ReadPaths, MemoryAdmission, Artifacts] $ \profile -> do
            let keys = selected profile
            keys `shouldSatisfy` (not . null)
            length (ordNub keys) `shouldBe` length keys
            for_ keys $ \key ->
                case findScenario key of
                    Nothing -> expectationFailure ("profile names no native scenario: " <> toString key)
                    Just scenario -> runsUnder twoCores scenario `shouldBe` True
  where
    selected profile = fromMaybe [] (snd (selectProfile profile Nothing))
