-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Owned recent and old native releases for advisory admission controls.
module Ecluse.E2E.Fixtures.Cargo.Advisories (buildCargoAdvisoryReleases) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Time (UTCTime (UTCTime), getCurrentTime, secondsToDiffTime)
import System.FilePath ((</>))

import Ecluse.Core.Text.Iso8601 (renderIso8601Utc)
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Place native advisory releases beside the main Cargo source graph.
buildCargoAdvisoryReleases :: FilePath -> Text -> IO ()
buildCargoAdvisoryReleases root upstream = do
    affected <- cargoLeafRelease "ecluse-e2e-advisory" "1.18.0"
    fixed <- cargoLeafRelease "ecluse-e2e-advisory" "1.19.0"
    writeCargoRegistry root upstream Nothing False [affected, fixed]
    UTCTime day dayTime <- getCurrentTime
    -- Cargo's pubtime excludes fractional seconds.
    let now = renderIso8601Utc (UTCTime day (secondsToDiffTime (floor dayTime)))
        recent = case cargoIndexRecord fixed of
            Object fields -> Object (KeyMap.insert "pubtime" (String now) fields)
            other -> other
    writeFileLBS (root </> toString (cargoShard "ecluse-e2e-advisory")) (encode (cargoIndexRecord affected) <> "\n" <> encode recent <> "\n")
