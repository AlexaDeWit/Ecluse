-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Broken observations must fail the same artifact contract that real providers run.
module Ecluse.Test.Contract.ServingSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving

spec :: Spec
spec = do
    it (caseLabel "core.contract.serving.valid-observations" "accepts matching independently observed bytes and advertised digest") $
        artifactIntegrityCase (observedDriver id id)
    it (caseLabel "core.contract.serving.corrupt-bytes" "rejects changed artifact bytes even when the status is successful") $
        artifactIntegrityCase (observedDriver id (const "wrong bytes")) `shouldThrow` anyException
    it (caseLabel "core.contract.serving.corrupt-digest" "rejects a wrong advertised digest even when artifact bytes match") $
        artifactIntegrityCase (observedDriver (const "wrong digest") id) `shouldThrow` anyException

observedDriver :: (Text -> Text) -> (LByteString -> LByteString) -> ServingDriver
observedDriver changeDigest changeBytes =
    ServingDriver
        { withServing = \_ _ releases action -> do
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
                    , requestArtifact = \_ -> pure response{responseBody = changeBytes (responseBody response)}
                    }
        , servingIdentity = ("thing@" <>)
        , servingDigest = decodeUtf8
        }
