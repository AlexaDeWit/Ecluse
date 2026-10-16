-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | Conditional S3 writes and the listing that tells a missing key from a refused read.

amazonka's generated @PutObject@ has no @If-Match@ or @If-None-Match@ field, so 'ConditionalPut'
adds the header before signing and the signature covers it. A store that ignores the header would
overwrite unconditionally, so a writer that relies on it needs a store that enforces it.
-}
module Ecluse.Runtime.Aws.S3.Conditional (
    WritePrecondition (..),
    ConditionalPut (..),
    isWriteConflict,
    absenceListing,
    listingProvesAbsence,
) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Amazonka.Types qualified as Types
import Lens.Micro ((%~), (?~), (^.))
import Network.HTTP.Types (Header)

import Ecluse.Runtime.Aws.Fault (serviceStatus)

-- | What a conditional write requires of the object the key holds.
data WritePrecondition
    = -- | No object holds the key: @If-None-Match: *@.
      IfAbsent
    | -- | The object holds exactly this ETag: @If-Match@.
      IfRevision ByteString
    deriving stock (Eq, Show)

-- | A @PutObject@ the store applies only while its precondition holds.
data ConditionalPut = ConditionalPut WritePrecondition S3.PutObject

instance AWS.AWSRequest ConditionalPut where
    type AWSResponse ConditionalPut = S3.PutObjectResponse
    request overrides (ConditionalPut precondition native) =
        coerce (AWS.request overrides native & Types.request_headers %~ withPrecondition precondition)
    response decode service _ = AWS.response decode service (Proxy :: Proxy S3.PutObject)

-- Any precondition the generated request already carries is replaced, so exactly one is signed.
withPrecondition :: WritePrecondition -> [Header] -> [Header]
withPrecondition precondition headers =
    condition : filter (\(name, _) -> name /= "If-Match" && name /= "If-None-Match") headers
  where
    condition = case precondition of
        IfAbsent -> ("If-None-Match", "*")
        IfRevision etag -> ("If-Match", etag)

-- | Whether the store refused a conditional write because its precondition no longer held.
isWriteConflict :: AWS.Error -> Bool
isWriteConflict err = serviceStatus err `elem` [Just 409, Just 412]

-- | The one-key listing a refused read falls back on, which needs only prefix-scoped @s3:ListBucket@.
absenceListing :: Text -> Text -> S3.ListObjectsV2
absenceListing bucket key =
    S3.newListObjectsV2 (S3.BucketName bucket)
        & S3L.listObjectsV2_prefix
        ?~ key
            & S3L.listObjectsV2_maxKeys
        ?~ 1

-- | Only a complete, empty listing proves absence. A truncated or populated one proves nothing.
listingProvesAbsence :: S3.ListObjectsV2Response -> Bool
listingProvesAbsence response =
    response ^. S3L.listObjectsV2Response_isTruncated == Just False
        && null (fromMaybe [] (response ^. S3L.listObjectsV2Response_contents))
