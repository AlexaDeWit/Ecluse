-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.GraphRegistrySpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.BenchLoad.GraphRegistry
import Ecluse.Test.Package (hexSha256Of)

spec :: Spec
spec = describe "frozen graph response provenance" $ do
    it "keeps scoped artifact probes attached to their listing" $
        packageKey "@scope/name/-/name-1.0.0.tgz" `shouldBe` "@scope/name"
    it "cannot turn an HTTP path into a filesystem escape" $
        takeDirectory (capturePath "/capture" "../../outside") `shouldBe` "/capture"
    it "reports an uncaptured key as absent" $
        withSystemTempDirectory "graph" $
            \root -> loadCapture root "absent" `shouldReturn` Nothing
    it "rejects changed bytes against the saved digest" $
        withSystemTempDirectory "graph" $ \root -> do
            let key = "package"
                provenance = Capture key "https://registry.npmjs.org/package" "2026-09-20T00:00:00Z" (hexSha256Of "original") 8 200 "application/json" []
                base = capturePath root key
            LBS.writeFile (base <> ".json") (encode provenance)
            LBS.writeFile (base <> ".body") "replaced"
            loadCapture root key `shouldThrow` anyException
    it "retains upstream refusal bodies and status as evidence" $
        withSystemTempDirectory "graph" $ \root -> do
            let key = "missing"
                provenance = Capture key "https://registry.npmjs.org/missing" "2026-09-20T00:00:00Z" (hexSha256Of "{}") 2 404 "application/json" []
                base = capturePath root key
            LBS.writeFile (base <> ".json") (encode provenance)
            LBS.writeFile (base <> ".body") "{}"
            loadCapture root key `shouldReturn` Just (provenance, "{}")
