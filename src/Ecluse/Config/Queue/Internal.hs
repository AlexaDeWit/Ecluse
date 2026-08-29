-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private construction boundary for 'QueueUrl'.

The @ecluse@ library does not expose this module (it is an @other-module@), so the raw constructor
is reachable only from inside the library. 'Ecluse.Config.QueueTarget.mkQueueUrl' is the only
builder, and "Ecluse.Config.Types" re-exports the type abstractly with its two selectors. A
'QueueUrl' whose text skipped the credential refusal, or whose target disagrees with its text, is
therefore unrepresentable outside this module.
-}
module Ecluse.Config.Queue.Internal (
    QueueTarget (..),
    QueueUrl (..),
) where

-- | A recognised mirror-queue destination, parsed from the queue URL's shape.
data QueueTarget
    = -- | An SQS queue URL, carrying the region parsed from its host.
      SqsTarget Text
    | -- | A Pub\/Sub topic resource, carrying its project and topic.
      PubSubTarget Text Text
    deriving stock (Eq, Show)

{- | @queue.url@ as parsed at load ('Ecluse.Config.QueueTarget.mkQueueUrl'): the value as written,
with the backend its shape names, or no backend when only the SQS endpoint override can dial it.
-}
data QueueUrl = QueueUrl
    { queueUrlText :: Text
    , queueUrlTarget :: Maybe QueueTarget
    }
    deriving stock (Eq, Show)
