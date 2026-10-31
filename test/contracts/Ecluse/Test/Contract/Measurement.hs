-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent obligations and provider metadata for live and retention audits.
module Ecluse.Test.Contract.Measurement (measurementCatalogue, measurementRegistration) where

import Prelude hiding (universe)

import Data.Universe.Class (universe)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Test.Contract.Types

-- | Compare actual registrations with independent live and retention obligations.
measurementCatalogue :: [Group] -> Catalogue
measurementCatalogue registrations =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName (filter (isJust . adapterFor) universe)
        , catalogueProfiles =
            [ Profile "measurement/npm" ["npm"] [] ["full-read", "selected-read"]
            , Profile "measurement/pypi" ["pypi"] [] ["full-read", "selected-read"]
            , Profile "measurement/rubygems" ["rubygems"] [] ["full-read", "selected-read"]
            , Profile "measurement/cargo" ["cargo"] [] ["full-read", "selected-read"]
            ]
        , catalogueObligations =
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
                , "retention/pypi/full/repeated-envelope"
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
        , catalogueGroups = registrations
        }

required :: Text -> Text -> Text -> [Text] -> Obligation
required identifier profile contract = Obligation identifier profile contract Supported (capabilities contract)

-- | Attach discovered case IDs to the provider's lifecycle and capability metadata.
measurementRegistration :: Text -> Text -> Text -> Tier -> [Text] -> [Text] -> [Text] -> Group
measurementRegistration identifier contract profile tier prerequisites isolation cases =
    Group identifier contract profile tier "measurement" [Case item [identifier] | item <- cases] (capabilities contract) prerequisites [] isolation

capabilities :: Text -> [Text]
capabilities contract = ["full-read"] <> ["selected-read" | contract == "retention"]
