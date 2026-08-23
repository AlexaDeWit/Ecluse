-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Derive the mirror-queue backend from the queue URL's own shape.

The queue URL is the single source of truth for which backend carries the mirror
jobs, the same derivation the mirror-write credential follows
("Ecluse.Config.MirrorCredential"). A real SQS queue URL
(@https:\/\/sqs.{region}.amazonaws.com\/{account}\/{queue}@) names the SQS backend
and carries its region in its host. A Pub\/Sub topic resource
(@projects\/{project}\/topics\/{topic}@) names the GCP backend and carries its
project. The mechanism comes from the same destination it will serve, so a
backend\/URL disagreement is unrepresentable rather than merely guarded. No separate
backend selector exists to disagree with the URL.
-}
module Ecluse.Config.QueueTarget (
    QueueTarget (..),
    parseQueueTarget,
) where

import Data.Text qualified as T

import Ecluse.Config.MirrorCredential (isAccountId)
import Ecluse.Config.Parser (HttpScheme (Https), splitHttpScheme)
import Ecluse.Core.Security (splitHostPort)
import Ecluse.Core.Text (nonBlank)

-- | A recognised mirror-queue destination, parsed from the queue URL's shape.
data QueueTarget
    = -- | An SQS queue URL, carrying the region parsed from its host.
      SqsTarget Text
    | -- | A Pub\/Sub topic resource, carrying its project and topic.
      PubSubTarget Text Text
    deriving stock (Eq, Show)

{- | Parse a queue URL into the backend it names, or 'Nothing' when it names neither. The
caller refuses that loudly rather than guessing a backend.

The SQS form is exactly @https:\/\/sqs.{region}.amazonaws.com\/{account}\/{queue}@, with a
single-label region, a 12-digit account, one non-empty queue segment, and no port, query, or
fragment. A nearly-but-not SQS URL is a transcription error to surface, never to repair.
-}
parseQueueTarget :: Text -> Maybe QueueTarget
parseQueueTarget raw = sqsTargetOf raw <|> pubSubTargetOf raw

-- The region slot must be a single host label. A dotted "region" means the host is
-- some other AWS endpoint shape, never an SQS queue's, so this does not mis-parse it.
sqsTargetOf :: Text -> Maybe QueueTarget
sqsTargetOf raw = do
    let url = T.strip raw
    (scheme, rest) <- splitHttpScheme url
    guard (scheme == Https)
    guard (T.all (\c -> c /= '?' && c /= '#') rest)
    let (authority, slashPath) = T.breakOn "/" rest
    -- The canonical form writes no port and no brackets, so the authority must be exactly
    -- the host the shared split recovers.
    (host, _) <- splitHostPort authority
    guard (host == authority)
    region <- nonBlank =<< T.stripSuffix ".amazonaws.com" =<< T.stripPrefix "sqs." (T.toLower host)
    guard (T.all (/= '.') region)
    case T.splitOn "/" (T.drop 1 slashPath) of
        [account, queueName]
            | isAccountId account && isJust (nonBlank queueName) ->
                pure (SqsTarget region)
        _ -> Nothing

pubSubTargetOf :: Text -> Maybe QueueTarget
pubSubTargetOf raw = case T.splitOn "/" raw of
    ["projects", project, "topics", topic] ->
        PubSubTarget <$> nonBlank project <*> nonBlank topic
    _ -> Nothing
