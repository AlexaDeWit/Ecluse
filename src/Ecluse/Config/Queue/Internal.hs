-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private construction boundary for 'QueueUrl'.

The @ecluse@ library does not expose this module (it is an @other-module@), so the raw constructor
is reachable only from inside the library. 'Ecluse.Config.QueueTarget.mkQueueUrl' is the only
builder, and "Ecluse.Config.Types" re-exports the type abstractly with its two selectors. A
'QueueUrl' whose text skipped the credential refusal, or whose target disagrees with its text, is
therefore unrepresentable outside this library.
-}
module Ecluse.Config.Queue.Internal (
    QueueTarget (..),
    QueueUrl (..),
    queueUrlText,
    queueUrlTarget,
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
data QueueUrl = QueueUrl Text (Maybe QueueTarget)
    deriving stock (Eq, Show)

-- The two halves are positional rather than record fields, and these accessors are hand-written,
-- because record-update syntax needs only a field label in scope to rebuild a value past 'mkQueueUrl'.

-- | The value as written, trimmed.
queueUrlText :: QueueUrl -> Text
queueUrlText (QueueUrl value _) = value

-- | The backend the value's shape names, 'Nothing' when it names none.
queueUrlTarget :: QueueUrl -> Maybe QueueTarget
queueUrlTarget (QueueUrl _ target) = target
