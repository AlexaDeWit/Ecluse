-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Held package-name listings passed through the production selective parser.
module Ecluse.Test.Registry.Npm.Maintenance (parsePackageListing) where

import Data.ByteString qualified as BS
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Npm.Maintenance (packageListingParser)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)

-- | Parse buffered fixture input with the same extraction used by a live store listing.
parsePackageListing :: ByteString -> Either ParseError [PackageName]
parsePackageListing body = do
    streamed <- first (ParseError . show) (parseJsonChunks (MetadataBodyLimit (BS.length body)) packageListingParser (\_ names -> Right names) [] [body])
    streamValue streamed
