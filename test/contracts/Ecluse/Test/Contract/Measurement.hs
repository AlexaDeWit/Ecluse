-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent live and retention obligations, checked by the smoke and residency suites when they start.
module Ecluse.Test.Contract.Measurement (checkLiveCases, checkRetentionCases) where

import Prelude hiding (universe)

import Data.Text qualified as T
import Data.Universe.Class (universe)
import Test.Hspec (Spec)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Test.Contract.Binding (labelledCaseIds)
import Ecluse.Test.Contract.Catalogue (validateCatalogue)
import Ecluse.Test.Contract.Manifest (failRunner)
import Ecluse.Test.Contract.Types

-- | Exit before any example runs unless each live smoke spec declares exactly its obligation's cases.
checkLiveCases :: [(Ecosystem, Spec)] -> IO ()
checkLiveCases = checkCases Smoke "live" "live-metadata"

-- | Exit before any example runs unless each reader-retention spec declares exactly its obligation's cases.
checkRetentionCases :: [(Ecosystem, Spec)] -> IO ()
checkRetentionCases = checkCases Residency "retention" "retention"

-- Read each spec's labels without running its examples, then validate them against the contract's obligations.
checkCases :: Tier -> Text -> Text -> [(Ecosystem, Spec)] -> IO ()
checkCases tier prefix contract declarations = do
    groups <- traverse registration declarations
    whenLeft_ (validateCatalogue (catalogue groups)) $
        failRunner . T.unlines . (("the " <> prefix <> " cases do not meet their measurement obligations:") :)
  where
    registration (ecosystem, spec) = do
        let identifier = prefix <> "/" <> ecosystemName ecosystem
        cases <- labelledCaseIds spec
        pure $ Group identifier contract ("measurement/" <> ecosystemName ecosystem) tier "measurement" [Case item [identifier] | item <- cases] (capabilities contract) [] [] []
    catalogue =
        Catalogue
            (map ecosystemName (filter (isJust . adapterFor) universe))
            profiles
            (filter ((== contract) . obligationContract) obligations)

profiles :: [Profile]
profiles =
    [ Profile "measurement/npm" ["npm"] [] ["full-read", "selected-read"]
    , Profile "measurement/pypi" ["pypi"] [] ["full-read", "selected-read"]
    , Profile "measurement/rubygems" ["rubygems"] [] ["full-read", "selected-read"]
    , Profile "measurement/cargo" ["cargo"] [] ["full-read", "selected-read"]
    ]

-- Listed by hand, never derived from the specs, so a deleted or renamed case leaves a gap.
obligations :: [Obligation]
obligations =
    [ required
        "retention/npm"
        "measurement/npm"
        "retention"
        [ "retention/npm/selected/keys"
        , "retention/npm/selected/members"
        , "retention/npm/selected/dependencies"
        , "retention/npm/full/keys"
        , "retention/npm/full/members"
        , "retention/npm/full/dependencies"
        , "retention/npm/selected/containers"
        ]
    , required
        "retention/pypi"
        "measurement/pypi"
        "retention"
        [ "retention/pypi/selected/containers"
        , "retention/pypi/selected/members"
        , "retention/pypi/selected/other-releases"
        , "retention/pypi/full/members"
        , "retention/pypi/full/past-limit"
        ]
    , required
        "retention/cargo"
        "measurement/cargo"
        "retention"
        [ "retention/cargo/selected/other-releases"
        , "retention/cargo/full/past-limit"
        ]
    , required
        "retention/rubygems"
        "measurement/rubygems"
        "retention"
        [ "retention/rubygems/selected/other-releases"
        , "retention/rubygems/full/past-limit"
        ]
    , required
        "live/npm"
        "measurement/npm"
        "live-metadata"
        ["live/npm/react", "live/npm/types-node", "live/npm/lodash", "live/npm/typescript"]
    , required "live/pypi" "measurement/pypi" "live-metadata" ["live/pypi/sampleproject"]
    , required "live/rubygems" "measurement/rubygems" "live-metadata" ["live/rubygems/rake"]
    , required "live/cargo" "measurement/cargo" "live-metadata" ["live/cargo/serde"]
    ]

required :: Text -> Text -> Text -> [Text] -> Obligation
required identifier profile contract = Obligation identifier profile contract Supported (capabilities contract)

capabilities :: Text -> [Text]
capabilities contract = ["full-read"] <> ["selected-read" | contract == "retention"]
