-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Derive the mirror-queue backend from the queue URL's own shape.

The queue URL is the single source of truth for which backend carries the mirror
jobs, the same derivation the mirror-write credential follows
("Ecluse.Config.MirrorCredential"): a real SQS queue URL
(@https:\/\/sqs.{region}.amazonaws.com\/{account}\/{queue}@) names the SQS backend
and carries its region in its host, and a Pub\/Sub topic resource
(@projects\/{project}\/topics\/{topic}@) names the GCP backend and carries its
project. Because the mechanism is parsed from the very destination it will serve, a
backend\/URL disagreement is unrepresentable rather than merely guarded, and no
separate backend selector exists to disagree with the URL.
-}
module Ecluse.Config.QueueTarget (
    QueueTarget (..),
    parseQueueTarget,
) where

import Data.Text qualified as T

import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Text (nonBlank)

-- | A recognised mirror-queue destination, parsed from the queue URL's shape.
data QueueTarget
    = -- | An SQS queue URL; carries the region parsed from its host.
      SqsTarget Text
    | -- | A Pub\/Sub topic resource; carries its project and topic.
      PubSubTarget Text Text
    deriving stock (Eq, Show)

{- | Parse a queue URL into the backend it names, or 'Nothing' for a shape that
names neither -- which the caller refuses loudly rather than guessing a backend.
The SQS shape is judged on the URL's host alone (an @sqs.{region}.amazonaws.com@
endpoint, however the path spells the account and queue name); the Pub\/Sub shape
is the whole value as a topic resource.
-}
parseQueueTarget :: Text -> Maybe QueueTarget
parseQueueTarget raw = sqsTargetOf raw <|> pubSubTargetOf raw

-- The region slot must be a single host label: a dotted "region" means the host is
-- some other AWS endpoint shape, never an SQS queue's, so it is not mis-parsed here.
sqsTargetOf :: Text -> Maybe QueueTarget
sqsTargetOf url = do
    region <- nonBlank =<< T.stripSuffix ".amazonaws.com" =<< T.stripPrefix "sqs." (hostAddress url)
    guard (T.all (/= '.') region)
    pure (SqsTarget region)

pubSubTargetOf :: Text -> Maybe QueueTarget
pubSubTargetOf raw = case T.splitOn "/" raw of
    ["projects", project, "topics", topic] ->
        PubSubTarget <$> nonBlank project <*> nonBlank topic
    _ -> Nothing
