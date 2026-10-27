-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native scheduling profiles remain separate from actual scenario registration.
module Ecluse.E2E.RubyGems.Profiles (rubyProfiles) where

import Ecluse.E2E.RubyGems.DredgerE2ESpec (rubyDredgerProfile)
import Ecluse.Test.Contract.Types (Profile (..))

-- | Every profile must retain an independently supported obligation.
rubyProfiles :: [Profile]
rubyProfiles =
    [ Profile "rubygems/read" ["rubygems"] ["http"] ["read", "client-install", "telemetry"]
    , Profile "rubygems/write" ["rubygems"] ["http", "sqs", "s3", "collector"] ["read", "mirror", "client-install", "telemetry", "advisories"]
    , Profile "rubygems/publication" ["rubygems"] ["http"] ["read", "publication", "client-install"]
    , rubyDredgerProfile
    , Profile "rubygems/contracts" ["rubygems"] [] ["contract-registration"]
    ]
