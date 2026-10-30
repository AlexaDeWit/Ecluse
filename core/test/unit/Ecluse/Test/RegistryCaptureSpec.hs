-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live capture helpers retain native version spelling and propagate cancellation.
module Ecluse.Test.RegistryCaptureSpec (spec) where

import Prelude hiding (universe)

import Control.Exception (AsyncException (ThreadKilled))
import Control.Exception qualified as Exception
import Data.Map.Strict qualified as Map
import Data.Universe.Class (Universe (universe))
import Network.HTTP.Client (ManagerSettings (managerModifyRequest), defaultManagerSettings, newManager, parseUrlThrow)
import Network.HTTP.Types.Status (status200)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Test.RegistryCapture (
    Catalogue (catBenchPins, catSmokeNames),
    decodeCatalogue,
    fetchPackumentBody,
    loadCatalogue,
    parseRegistryVersions,
    registryUrl,
    smokeRegistryPackages,
 )

import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "fetchPackumentBody" $ do
        it "retains optional absence for a synchronous request failure" $ do
            manager <- newManager defaultManagerSettings{managerModifyRequest = \_ -> throwIO (TestContractEscape "request failed")}
            fetchPackumentBody manager Npm "demo" `shouldReturn` Nothing
        it "propagates cancellation before network access" $ do
            manager <- newManager defaultManagerSettings{managerModifyRequest = \_ -> Exception.throwIO ThreadKilled}
            fetchPackumentBody manager Npm "demo" `shouldThrow` (== ThreadKilled)
        it "returns successful response bytes unchanged" $
            withStub status200 npmBody $ \stub -> do
                manager <- newManager defaultManagerSettings{managerModifyRequest = \_ -> parseUrlThrow (toString (stubBaseUrl stub))}
                fetchPackumentBody manager Npm "demo" `shouldReturn` Just npmBody

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
        it "builds the lower-case Cargo sparse shard" $
            registryUrl Cargo "Mixed_Crate" `shouldBe` "https://index.crates.io/mi/xe/mixed_crate"
        it "builds the RubyGems versions endpoint" $
            registryUrl RubyGems "rails" `shouldBe` "https://rubygems.org/api/v1/versions/rails.json"

    describe "parseRegistryVersions" $ do
        it "reads the npm packument's version keys" $
            parseRegistryVersions Npm npmBody `shouldBe` Just ["1.0.0", "2.0.0"]
        it "reads the PyPI release keys" $
            parseRegistryVersions PyPI pypiBody `shouldBe` Just ["1.0", "2.0"]
        it "retains Cargo raw versions in physical record order" $
            parseRegistryVersions Cargo "{\"vers\":\"1.0.0+source.2\"}\n{\"vers\":\"1.0.0-alpha.1\"}"
                `shouldBe` Just ["1.0.0+source.2", "1.0.0-alpha.1"]
        it "refuses a malformed Cargo record beside a healthy version" $
            parseRegistryVersions Cargo "{\"vers\":\"1.0.0\"}\nnot-json" `shouldBe` Nothing
        it "reads the RubyGems version numbers in listing order" $
            parseRegistryVersions RubyGems gemBody `shouldBe` Just ["3.1.0", "3.0.0"]
        it "returns Nothing on an undecodable body" $
            parseRegistryVersions Npm "{ not json" `shouldBe` Nothing

    describe "the committed catalogue" $
        it "loads with non-empty curated names for every ecosystem and non-empty bench pins" $ do
            cat <- loadCatalogue
            let names = catSmokeNames cat
            for_ universe $ \ecosystem ->
                (ecosystem, Map.lookup ecosystem names) `shouldSatisfy` maybe False (not . null) . snd
            (elem "typescript" <$> Map.lookup Npm names) `shouldBe` Just True
            Map.member "lodash" (catBenchPins cat) `shouldBe` True

sampleCatalogue :: LByteString
sampleCatalogue =
    "{\"pins\":{\"lodash\":\"4.18.1\"},\
    \\"smokeNames\":{\"npm\":[\"a\",\"b\"],\"pypi\":[\"c\"],\"rubygems\":[\"d\"]}}"

unknownEcoCatalogue :: LByteString
unknownEcoCatalogue = "{\"pins\":{},\"smokeNames\":{\"unknown\":[\"x\"]}}"

npmBody :: LByteString
npmBody =
    "{\"name\":\"demo\",\"versions\":{\
    \\"1.0.0\":{\"name\":\"demo\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://r/demo-1.0.0.tgz\"}},\
    \\"2.0.0\":{\"name\":\"demo\",\"version\":\"2.0.0\",\"dist\":{\"tarball\":\"https://r/demo-2.0.0.tgz\"}}}}"

pypiBody :: LByteString
pypiBody = "{\"releases\":{\"1.0\":[],\"2.0\":[]}}"

gemBody :: LByteString
gemBody = "[{\"number\":\"3.1.0\"},{\"number\":\"3.0.0\"}]"
