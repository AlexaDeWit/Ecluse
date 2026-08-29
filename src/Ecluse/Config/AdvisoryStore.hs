-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Derive the advisory-database store from @advisories.url@'s own scheme, once at load
('mkAdvisoryStoreUrl'), the way "Ecluse.Config.QueueTarget" derives the mirror-queue backend.

@s3:\/\/bucket[\/prefix]@ names the S3 store. No separate provider selector exists to disagree
with the URL, and a scheme this build does not know is refused at load rather than dialled.
'advisoryStoreBucket' and 'advisoryObjectKey' are the one place the proxy's sync and Pilot's
export agree on where an artifact lives.
-}
module Ecluse.Config.AdvisoryStore (
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl,
    advisoryStoreUrlText,
    advisoryStoreTarget,
    mkAdvisoryStoreUrl,
    advisoryStoreBucket,
    advisoryObjectKey,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.Text qualified as T

import Ecluse.Config.Advisory.Internal (
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl (..),
    advisoryStoreTarget,
    advisoryStoreUrlText,
 )
import Ecluse.Core.Security (refuseCredentialMaterial)
import Ecluse.Core.Text (nonBlank)

-- | The scheme this build knows, named in its own refusal so the message stays in step.
s3Scheme :: Text
s3Scheme = "s3://"

{- | Build the 'AdvisoryStoreUrl' an @advisories.url@ key resolves to, the key naming every
refusal. The credential refusal runs first, because the refusals below it quote the value.
-}
mkAdvisoryStoreUrl :: Text -> Text -> Either Text AdvisoryStoreUrl
mkAdvisoryStoreUrl key raw
    | Left reason <- refuseCredentialMaterial key trimmed = Left reason
    | Just rest <- T.stripPrefix s3Scheme trimmed = AdvisoryStoreUrl trimmed <$> s3StoreOf key rest
    | otherwise =
        Left
            ( key
                <> " must name an object store this build knows, currently only "
                <> s3Scheme
                <> "bucket[/prefix] (got "
                <> trimmed
                <> ")"
            )
  where
    trimmed = T.strip raw

-- Split the authority from the key prefix, refusing a bucket the S3 naming rules would.
s3StoreOf :: Text -> Text -> Either Text AdvisoryStoreTarget
s3StoreOf key rest = do
    let (bucket, slashPrefix) = T.breakOn "/" rest
    unless (validBucketName bucket) (Left (bucketRefusal key bucket))
    Right (S3Store bucket (normalisedPrefix slashPrefix))

bucketRefusal :: Text -> Text -> Text
bucketRefusal key bucket =
    key
        <> " must name an S3 bucket of 3 to 63 characters, lowercase letters, digits, dots, and"
        <> " hyphens, starting and ending alphanumeric (got "
        <> bucket
        <> ")"

{- | The S3 bucket naming rules, checked at load so a malformed name fails the boot rather than
the first advisory poll. The dotted forms stay legal, because an existing bucket may carry one.
-}
validBucketName :: Text -> Bool
validBucketName bucket =
    T.compareLength bucket 3 /= LT
        && T.compareLength bucket 63 /= GT
        && T.all bucketChar bucket
        && maybe False (alphanumeric . fst) (T.uncons bucket)
        && maybe False (alphanumeric . snd) (T.unsnoc bucket)
  where
    bucketChar c = alphanumeric c || c == '-' || c == '.'
    alphanumeric c = isAsciiLower c || isDigit c

-- A prefix is stored without its surrounding slashes, so 'advisoryObjectKey' writes exactly one.
normalisedPrefix :: Text -> Maybe Text
normalisedPrefix = nonBlank . T.dropWhileEnd (== '/') . T.dropWhile (== '/')

-- | The bucket the store names.
advisoryStoreBucket :: AdvisoryStoreUrl -> Text
advisoryStoreBucket url = case advisoryStoreTarget url of
    S3Store bucket _ -> bucket

{- | The object key one compiled artifact takes in the store: the configured prefix ahead of the
artifact's own file name. The proxy's sync and Pilot's export both address an object through this.
-}
advisoryObjectKey :: AdvisoryStoreUrl -> FilePath -> Text
advisoryObjectKey url fileName = case advisoryStoreTarget url of
    S3Store _ Nothing -> toText fileName
    S3Store _ (Just prefix) -> prefix <> "/" <> toText fileName
