module Ecluse.Test.RegistryCaptureSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Test.RegistryCapture (
    Catalogue (catBenchPins, catSmokeNames),
    decodeCatalogue,
    loadCatalogue,
    parseRegistryVersions,
    registryUrl,
    smokeRegistryPackages,
 )

spec :: Spec
spec = do
    describe "decodeCatalogue" $ do
        it "decodes the per-ecosystem smoke names and the bench pins" $
            case decodeCatalogue sampleCatalogue of
                Left err -> expectationFailure ("did not decode: " <> err)
                Right cat -> do
                    catSmokeNames cat
                        `shouldBe` Map.fromList [(Npm, ["a", "b"]), (PyPI, ["c"]), (RubyGems, ["d"])]
                    catBenchPins cat `shouldBe` Map.fromList [("lodash", "4.18.1")]

        it "orders smokeRegistryPackages by ecosystem (npm, then pypi, then rubygems)" $
            (map fst . smokeRegistryPackages <$> rightToMaybe (decodeCatalogue sampleCatalogue))
                `shouldBe` Just [Npm, PyPI, RubyGems]

        it "rejects an unknown ecosystem key in smokeNames rather than dropping it" $
            decodeCatalogue unknownEcoCatalogue `shouldSatisfy` isLeft

    describe "registryUrl" $ do
        it "percent-encodes a scoped npm name's separator" $
            registryUrl Npm "@types/node" `shouldBe` "https://registry.npmjs.org/@types%2Fnode"
        it "leaves an unscoped npm name bare" $
            registryUrl Npm "lodash" `shouldBe` "https://registry.npmjs.org/lodash"
        it "builds the PyPI project-JSON endpoint" $
            registryUrl PyPI "requests" `shouldBe` "https://pypi.org/pypi/requests/json"
        it "builds the RubyGems versions endpoint" $
            registryUrl RubyGems "rails" `shouldBe` "https://rubygems.org/api/v1/versions/rails.json"

    describe "parseRegistryVersions" $ do
        it "reads the npm packument's version keys" $
            parseRegistryVersions Npm npmBody `shouldBe` Just ["1.0.0", "2.0.0"]
        it "reads the PyPI release keys" $
            parseRegistryVersions PyPI pypiBody `shouldBe` Just ["1.0", "2.0"]
        it "reads the RubyGems version numbers in listing order" $
            parseRegistryVersions RubyGems gemBody `shouldBe` Just ["3.1.0", "3.0.0"]
        it "returns Nothing on an undecodable body" $
            parseRegistryVersions Npm "{ not json" `shouldBe` Nothing

    -- Guards the committed catalogue itself: a careless edit that drops an
    -- ecosystem, empties a list, or malforms the JSON fails here, in the gating tier.
    describe "the committed catalogue" $
        it "loads with non-empty curated names for every ecosystem and non-empty bench pins" $ do
            cat <- loadCatalogue
            let names = catSmokeNames cat
            Map.lookup Npm names `shouldSatisfy` maybe False (not . null)
            Map.lookup PyPI names `shouldSatisfy` maybe False (not . null)
            Map.lookup RubyGems names `shouldSatisfy` maybe False (not . null)
            -- Anchors so a careless catalogue edit is caught.
            (elem "typescript" <$> Map.lookup Npm names) `shouldBe` Just True
            Map.member "lodash" (catBenchPins cat) `shouldBe` True

-- ── sample bodies ──────────────────────────────────────────────────────────────

-- A well-formed catalogue with one pin and one curated name list per ecosystem.
sampleCatalogue :: LByteString
sampleCatalogue =
    "{\"pins\":{\"lodash\":\"4.18.1\"},\
    \\"smokeNames\":{\"npm\":[\"a\",\"b\"],\"pypi\":[\"c\"],\"rubygems\":[\"d\"]}}"

-- A catalogue whose smokeNames names an ecosystem the build does not serve.
unknownEcoCatalogue :: LByteString
unknownEcoCatalogue = "{\"pins\":{},\"smokeNames\":{\"cargo\":[\"x\"]}}"

-- A minimal npm packument: two versions, each a manifest the wire decoder keeps.
npmBody :: LByteString
npmBody =
    "{\"name\":\"demo\",\"versions\":{\
    \\"1.0.0\":{\"name\":\"demo\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://r/demo-1.0.0.tgz\"}},\
    \\"2.0.0\":{\"name\":\"demo\",\"version\":\"2.0.0\",\"dist\":{\"tarball\":\"https://r/demo-2.0.0.tgz\"}}}}"

-- A minimal PyPI project JSON: two releases with opaque file lists.
pypiBody :: LByteString
pypiBody = "{\"releases\":{\"1.0\":[],\"2.0\":[]}}"

-- A minimal RubyGems versions array, newest first, to pin order preservation.
gemBody :: LByteString
gemBody = "[{\"number\":\"3.1.0\"},{\"number\":\"3.0.0\"}]"
