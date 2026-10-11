-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure discovery of migrated live and retention cases, without fetching or probing.
module Ecluse.Test.Contract.Measurement (measurementCatalogue) where

import Prelude hiding (universe)

import Data.Universe.Class (universe)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Test.Contract.Types

-- | Native protocol checks remain in each case alongside its shared law.
measurementCatalogue :: Catalogue
measurementCatalogue =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName (filter (isJust . adapterFor) universe)
        , catalogueProfiles =
            [ Profile "measurement/npm" ["npm"] [] ["full-read", "selected-read"]
            , Profile "measurement/pypi" ["pypi"] [] ["full-read", "selected-read"]
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
                ]
            , required
                "live/npm"
                "measurement/npm"
                "live-metadata"
                ["live/npm/react", "live/npm/types-node", "live/npm/lodash", "live/npm/typescript"]
            , required "live/pypi" "measurement/pypi" "live-metadata" ["live/pypi/sampleproject"]
            ]
        , catalogueGroups =
            [ measurementGroup
                "retention/npm"
                "retention"
                "measurement/npm"
                Residency
                []
                ["rts-stats", "serial-heap"]
                ( ["retention/npm/" <> mode <> "/" <> shape | mode <- ["selected", "full"], shape <- ["keys", "members", "dependencies"]]
                    <> ["retention/npm/selected/containers"]
                )
            , measurementGroup
                "retention/pypi"
                "retention"
                "measurement/pypi"
                Residency
                []
                ["rts-stats", "serial-heap"]
                ["retention/pypi/" <> item | item <- ["selected/containers", "selected/members", "selected/other-releases", "full/members", "full/past-limit"]]
            , measurementGroup
                "live/npm"
                "live-metadata"
                "measurement/npm"
                Smoke
                ["public-network"]
                []
                ["live/npm/" <> package | package <- ["react", "types-node", "lodash", "typescript"]]
            , measurementGroup "live/pypi" "live-metadata" "measurement/pypi" Smoke ["public-network"] [] ["live/pypi/sampleproject"]
            ]
        }

required :: Text -> Text -> Text -> [Text] -> Obligation
required identifier profile contract = Obligation identifier profile contract Supported (capabilities contract)

measurementGroup :: Text -> Text -> Text -> Tier -> [Text] -> [Text] -> [Text] -> Group
measurementGroup identifier contract profile tier prerequisites isolation cases =
    Group identifier contract profile tier "measurement" [Case item [identifier] | item <- cases] (capabilities contract) prerequisites [] isolation

capabilities :: Text -> [Text]
capabilities contract = ["full-read"] <> ["selected-read" | contract == "retention"]
