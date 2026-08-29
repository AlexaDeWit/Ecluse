-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Derive the mirror-queue backend from the queue URL's own shape, once at load ('mkQueueUrl').

The queue URL is the single source of truth for which backend carries the mirror jobs, the same
derivation the mirror-write credential follows ("Ecluse.Config.MirrorCredential"). An SQS queue URL
(@https:\/\/sqs.{region}.amazonaws.com\/{account}\/{queue}@) names the SQS backend and carries its
region, and a Pub\/Sub topic resource (@projects\/{project}\/topics\/{topic}@) names the GCP one. No
separate backend selector exists to disagree with the URL.
-}
module Ecluse.Config.QueueTarget (
    QueueTarget (..),
    QueueUrl,
    queueUrlText,
    queueUrlTarget,
    mkQueueUrl,
    parseQueueTarget,
) where

import Data.Text qualified as T

import Ecluse.Config.MirrorCredential (isAccountId)
import Ecluse.Config.Queue.Internal (QueueTarget (..), QueueUrl (..))
import Ecluse.Config.Types (HttpScheme (Https), splitHttpScheme)
import Ecluse.Core.Security (refuseCredentialMaterial, splitHostPort)
import Ecluse.Core.Text (nonBlank)

{- | Build the 'QueueUrl' a @queue.url@ key resolves to, the key naming every refusal. A shape
naming no backend is carried, not refused: the SQS endpoint override dials one that matches none.
-}
mkQueueUrl :: Text -> Text -> Either Text QueueUrl
mkQueueUrl key raw
    | Left reason <- refuseCredentialMaterial key trimmed = Left reason
    | T.null trimmed = Left (key <> " must be a non-empty URL")
    | otherwise = Right (QueueUrl{queueUrlText = trimmed, queueUrlTarget = parseQueueTarget trimmed})
  where
    trimmed = T.strip raw

{- | Parse a queue URL into the backend it names, or 'Nothing' when it names neither. The SQS form
is exact, and a nearly-but-not SQS URL is a transcription error to surface, never to repair.
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
