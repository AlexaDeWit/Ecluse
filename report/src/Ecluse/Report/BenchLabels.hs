-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exact benchmark labels shared by row producers and report readers.
module Ecluse.Report.BenchLabels (tierName, tierPrefix, ecosystemPrefix) where

-- | The top-level work-per-request benchmark group.
tierName :: Text
tierName = "ecluse-core (work-per-request)"

-- | The CSV path prefix, including the root that tasty-bench inserts.
tierPrefix :: Text
tierPrefix = "All." <> tierName <> "."

-- | The group prefix before an ecosystem's canonical name, including its trailing space.
ecosystemPrefix :: Text
ecosystemPrefix = "ecosystem: "
