-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Held package-name listings passed through the production walk and an independent combinator oracle.
module Ecluse.Test.Registry.Npm.Maintenance (parsePackageListing, referencePackageListingParser) where

import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Npm.Maintenance.Listing (packageListingWalk)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonStream (walkJsonChunks)

-- | Parse buffered fixture input with the same extraction used by a live store listing.
parsePackageListing :: ByteString -> Either ParseError [PackageName]
parsePackageListing body = do
    streamed <- first (ParseError . show) (walkJsonChunks (MetadataBodyLimit (BS.length body)) packageListingWalk [body])
    streamValue streamed

-- | Independent combinator oracle for the production package-list walk.
referencePackageListingParser :: J.Parser [PackageName]
referencePackageListingParser = J.mapWithFailure finish (J.foldI collect Nothing events)
  where
    events = J.objectFound Nothing Nothing (Just . fst <$> J.objectItems (pure ()))
    collect found Nothing = Just (fromMaybe mempty found)
    collect found (Just raw) = Just $ case projectName raw of
        Right name | raw /= "_updated" -> Map.insert raw name (fromMaybe mempty found)
        _ -> fromMaybe mempty found
    finish Nothing = Left "the store's package listing is not a JSON object"
    finish (Just names) = Right (Map.elems names)
