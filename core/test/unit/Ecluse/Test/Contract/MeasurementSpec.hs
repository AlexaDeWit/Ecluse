-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit actual smoke and retention declarations without running their examples.
module Ecluse.Test.Contract.MeasurementSpec (spec) where

import Test.Hspec
import Test.Hspec.Core.Spec (Item (itemRequirement), filterForest, mapSpecForest, mapSpecItem_)

import Ecluse.Core.Registry.Cargo.ReaderResidencySpec qualified as CargoRetention
import Ecluse.Core.Registry.CargoSmokeSpec qualified as CargoSmoke
import Ecluse.Core.Registry.Npm.ReaderResidencySpec qualified as NpmRetention
import Ecluse.Core.Registry.NpmSmokeSpec qualified as NpmSmoke
import Ecluse.Core.Registry.PyPI.ReaderResidencySpec qualified as PyPIRetention
import Ecluse.Core.Registry.PyPISmokeSpec qualified as PyPISmoke
import Ecluse.Core.Registry.RubyGems.ReaderResidencySpec qualified as RubyGemsRetention
import Ecluse.Core.Registry.RubyGemsSmokeSpec qualified as RubyGemsSmoke
import Ecluse.Test.Contract.Binding (labelledCaseIds)
import Ecluse.Test.Contract.Catalogue (caseLabel, labelCaseId, validateCatalogue)
import Ecluse.Test.Contract.Measurement (measurementCatalogue, measurementRegistration)
import Ecluse.Test.Contract.Types (Case (Case), Catalogue, Group (groupCases, groupId), Tier (Residency, Smoke))

-- | Deleting or corrupting an actual declaration must leave an unsatisfied obligation.
spec :: Spec
spec = describe "measurement catalogue" $ do
    it "covers independent obligations with actual smoke and retention declarations" $ do
        catalogue <- inspect registrations
        validateCatalogue catalogue `shouldBe` Right catalogue
    it "rejects every omitted provider registration" $
        for_ registrations $ \(metadata, _) ->
            reject (filter ((/= groupId metadata) . groupId . fst) registrations)
    it "rejects deletion of each actual labelled example" $
        forEachCase $ \provider identifier ->
            reject (changeDeclaration provider (mapSpecForest (filterForest ((/= Just identifier) . labelCaseId . itemRequirement))))
    it "rejects malformed labels on each actual example" $
        forEachCase $ \provider identifier ->
            reject (changeDeclaration provider (renameCase identifier "[malformed ID] example"))
    it "rejects unknown labels on each actual example" $
        forEachCase $ \provider identifier ->
            reject (changeDeclaration provider (renameCase identifier (caseLabel ("unknown/" <> identifier) "example")))
    it "rejects an added malformed label without erasing required examples" $
        for_ registrations $ \(metadata, _) ->
            reject (changeDeclaration (groupId metadata) (withExtraExample "[malformed ID] example"))
    it "rejects an added unknown label without erasing required examples" $
        for_ registrations $ \(metadata, _) ->
            reject (changeDeclaration (groupId metadata) (withExtraExample (caseLabel "unknown/example" "example")))
    it "rejects duplicate actual examples" $
        for_ registrations $ \(metadata, _) ->
            reject (changeDeclaration (groupId metadata) (\declaration -> declaration >> declaration))

registrations :: [(Group, Spec)]
registrations =
    [ (measurementRegistration "retention/npm" "retention" "measurement/npm" Residency [] ["rts-stats", "serial-heap"] [], NpmRetention.spec)
    , (measurementRegistration "retention/pypi" "retention" "measurement/pypi" Residency [] ["rts-stats", "serial-heap"] [], PyPIRetention.spec)
    , (measurementRegistration "retention/cargo" "retention" "measurement/cargo" Residency [] ["rts-stats", "serial-heap"] [], CargoRetention.spec)
    , (measurementRegistration "retention/rubygems" "retention" "measurement/rubygems" Residency [] ["rts-stats", "serial-heap"] [], RubyGemsRetention.spec)
    , (measurementRegistration "live/npm" "live-metadata" "measurement/npm" Smoke ["public-network"] [] [], NpmSmoke.spec)
    , (measurementRegistration "live/pypi" "live-metadata" "measurement/pypi" Smoke ["public-network"] [] [], PyPISmoke.spec)
    , (measurementRegistration "live/rubygems" "live-metadata" "measurement/rubygems" Smoke ["public-network"] [] [], RubyGemsSmoke.spec)
    , (measurementRegistration "live/cargo" "live-metadata" "measurement/cargo" Smoke ["public-network"] [] [], CargoSmoke.spec)
    ]

inspect :: [(Group, Spec)] -> IO Catalogue
inspect declarations = measurementCatalogue <$> traverse observed declarations
  where
    observed (metadata, declaration) = do
        identifiers <- labelledCaseIds declaration
        pure metadata{groupCases = [Case identifier [groupId metadata] | identifier <- identifiers]}

reject :: [(Group, Spec)] -> Expectation
reject declarations = inspect declarations >>= (\catalogue -> validateCatalogue catalogue `shouldSatisfy` isLeft)

forEachCase :: (Text -> Text -> Expectation) -> Expectation
forEachCase assertCase = for_ registrations $ \(metadata, declaration) ->
    labelledCaseIds declaration >>= traverse_ (assertCase (groupId metadata))

changeDeclaration :: Text -> (Spec -> Spec) -> [(Group, Spec)]
changeDeclaration provider change =
    [(metadata, if groupId metadata == provider then change declaration else declaration) | (metadata, declaration) <- registrations]

renameCase :: Text -> String -> Spec -> Spec
renameCase identifier replacement = mapSpecItem_ $ \item ->
    if labelCaseId (itemRequirement item) == Just identifier then item{itemRequirement = replacement} else item

withExtraExample :: String -> Spec -> Spec
withExtraExample label declaration = do
    declaration
    it label (expectationFailure "declaration inspection must not execute examples")
