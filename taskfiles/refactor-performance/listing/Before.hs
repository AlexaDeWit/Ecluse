-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Adapt the pre-refactor bounded listing reader to the common measurement bracket.
module ListingAdapter (readListing) where

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.JsonStream (StreamResult, readJsonStream)
import Ecluse.Core.Registry.Npm.Maintenance (packageListingParser)
import Ecluse.Core.Security (BodyLimit)

-- | Keep the production parser and bounded driver unchanged.
readListing :: BodyLimit -> IO ByteString -> IO (Either Text (StreamResult [PackageName]))
readListing bound next = first show <$> readJsonStream bound packageListingParser (\_ names -> Right names) [] next
