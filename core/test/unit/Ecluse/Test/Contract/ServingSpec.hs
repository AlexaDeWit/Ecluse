-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Broken observations must fail the same artifact contract that real providers run.
module Ecluse.Test.Contract.ServingSpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Types qualified
import Test.Hspec

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving

spec :: Spec
spec = do
    it "accepts an artifact HEAD with a nonempty advertised representation" $
        artifactHeadCase (headResponse "" [("Content-Length", "12")])
    it "rejects an artifact HEAD that transfers body bytes" $
        artifactHeadCase (headResponse "body" [("Content-Length", "12")]) `shouldThrow` anyException
    it "rejects an artifact HEAD with no positive representation size" $
        for_ [[], [("Content-Length", "0")], [("Content-Length", "invalid")]] $ \headers ->
            artifactHeadCase (headResponse "" headers) `shouldThrow` anyException
    it "accepts an artifact HEAD that advertises the artifact's exact length" $
        artifactHeadLengthCase 12 (headResponse "" [("Content-Length", "12")])
    it "rejects an artifact HEAD that advertises another positive length" $
        for_ ["1", "13"] $ \advertised ->
            artifactHeadLengthCase 12 (headResponse "" [("Content-Length", advertised)]) `shouldThrow` anyException
    it (caseLabel "core.contract.serving.valid-observations" "accepts matching independently observed bytes and advertised digest") $
        artifactIntegrityCase (observedDriver id id)
    it (caseLabel "core.contract.serving.corrupt-bytes" "rejects changed artifact bytes even when the status is successful") $
        artifactIntegrityCase (observedDriver id (const "wrong bytes")) `shouldThrow` anyException
    it (caseLabel "core.contract.serving.corrupt-digest" "rejects a wrong advertised digest even when artifact bytes match") $
        artifactIntegrityCase (observedDriver (const "wrong digest") id) `shouldThrow` anyException

observedDriver :: (Text -> Text) -> (LByteString -> LByteString) -> ServingDriver
observedDriver changeDigest changeBytes =
    ServingDriver
        { withServing = \_ _ _ releases action -> observed releases action
        , withFaultedServing = \_ _ _ releases action -> observed releases action
        , withPublicFloor = \_ _ _ releases action -> observed releases action
        , servingIdentity = ("thing@" <>)
        , servingDigest = decodeUtf8
        }
  where
    observed :: [ServingRelease] -> (ServingSession -> IO a) -> IO a
    observed releases action = do
        release <- maybe (fail "contract supplied no public fixture") pure (listToMaybe releases)
        let response =
                ServingResponse
                    { responseStatus = 200
                    , responseBody = toLazy (releaseBytes release)
                    , responseHeaders = []
                    , responseVersions = [releaseVersion release]
                    , responseDigests = Map.singleton (releaseVersion release) [changeDigest (decodeUtf8 (releaseBytes release))]
                    }
        action
            ServingSession
                { requestMetadata = \_ _ -> pure response
                , requestArtifact = \_ _ _ -> pure response{responseBody = changeBytes (responseBody response)}
                }

headResponse :: LByteString -> [Network.HTTP.Types.Header] -> ServingResponse
headResponse body headers = ServingResponse 200 body headers [] mempty
