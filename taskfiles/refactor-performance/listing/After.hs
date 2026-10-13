-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Adapt the refactored bounded listing reader to the common measurement bracket.
module ListingAdapter (readListing) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Json.Walk (readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Npm.Maintenance.Listing (packageListingWalk)
import Ecluse.Core.Security (BodyLimit)

-- | Keep the production walk and bounded driver unchanged.
readListing :: BodyLimit -> IO ByteString -> IO (Either Text (StreamResult [PackageName]))
readListing bound next = first show <$> readJsonWalk bound packageListingWalk next
