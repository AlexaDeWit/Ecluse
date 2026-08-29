-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's mirror-queue backend selection: the pure decision of which queue
this binary builds and the boot warnings the choice warrants.

'planMirrorQueue' is the single place that knows which backends this binary can build. The
composition root pattern-matches its 'MirrorQueuePlan' to make the one constructor call, and
the warning functions turn the plan and the dead-letter probe into the boot's warning lines.
Failures aggregate as 'Ecluse.Composition.BootError.BootError's, so one run reports every
missing input.
-}
module Ecluse.Composition.MirrorQueue (
    MirrorRuntimePlan (..),
    planMirrorRuntime,
    MirrorQueuePlan (..),
    planMirrorQueue,
    mirrorQueuePlanWarning,
    memoryQueueBootWarning,
    memoryQueueDropWarning,
    deadLetterTerminusWarning,
) where

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Config (
    AppConfig (..),
    Config (..),
    Mount (mountRegistries),
    QueueSettings (qsMaxReceiveCount, qsUrl),
    QueueTarget (..),
    QueueUrl (queueUrlTarget, queueUrlText),
    regMirrorTarget,
 )
import Ecluse.Config.Ambient (AmbientAws (..), parseEndpointUrl)
import Ecluse.Core.Fault (TransportFault, tfDetail)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    retiringDelivery,
 )
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint, sqsMaxReceiveCount), defaultSqsConfig)

{- | Whether this deployment runs a mirror runtime at all. A serve-only deployment never
consults the queue configuration, so it boots with no queue variables set.
-}
data MirrorRuntimePlan
    = -- | No mount mirrors: no queue, no enqueue buffer, no worker.
      NoMirroring
    | -- | At least one mount mirrors: build the planned queue backend.
      MirrorWith MirrorQueuePlan
    deriving stock (Eq, Show)

{- | Decide whether the composition root builds a mirror runtime at all. A serve-only
deployment cannot fail boot over queue variables it does not need.
-}
planMirrorRuntime :: AmbientAws -> Config -> Either [BootError] MirrorRuntimePlan
planMirrorRuntime ambient config
    | noneMirror = Right NoMirroring
    | otherwise = MirrorWith <$> planMirrorQueue ambient (configApp config)
  where
    noneMirror = all (isNothing . regMirrorTarget . mountRegistries) (configMounts config)

{- | Which mirror-queue backend the composition root builds. The plan carries no sizes:
the in-memory backend's depth cap is a memory-plan tenant, allocated after this choice.
-}
data MirrorQueuePlan
    = -- | The durable AWS SQS backend, built by @Ecluse.Runtime.Queue.Sqs.newSqsQueue@.
      SqsBackend SqsConfig
    | {- | The bounded in-memory backend, built by
      'Ecluse.Core.Queue.newBoundedInMemoryQueue'. Non-durable and best-effort, so boot warns.
      -}
      MemoryBackend
    deriving stock (Eq, Show)

{- | Select the mirror-queue backend from @ECLUSE_QUEUE__URL@'s shape and the ambient
SDK environment. There is no backend selector, so a backend that disagrees with the URL
is unrepresentable. An @AWS_ENDPOINT_URL_SQS@ override forces the SQS reading whatever
the URL's shape, because an emulator or VPC endpoint URL matches no public shape. The
generic @AWS_ENDPOINT_URL@ is deliberately not consulted: it is the S3 advisory client's
override, and honouring it here would let an S3-only override redirect queue traffic.
-}
planMirrorQueue :: AmbientAws -> AppConfig -> Either [BootError] MirrorQueuePlan
planMirrorQueue ambient env = case qsUrl (cfgQueue env) of
    -- No queue URL: a deliberate rollover to the in-memory queue, never a boot failure.
    -- Mirroring is demand-driven, so a job lost to a restart re-enqueues on the next
    -- demand: the rollover costs durability, not safety.
    Nothing -> Right MemoryBackend
    Just queueUrl ->
        let url = queueUrlText queueUrl
         in case nonBlank =<< ambientAwsEndpointUrlSqs ambient of
                Just override -> case (regionE, endpointE override) of
                    (Right region, Right endpoint) ->
                        Right (SqsBackend (sqsConfigFor url region){sqsEndpoint = Just endpoint})
                    (r, e) -> Left (lefts [void r, void e])
                Nothing -> case queueUrlTarget queueUrl of
                    Just (SqsTarget region) -> Right (SqsBackend (sqsConfigFor url region))
                    Just (PubSubTarget _project _topic) -> Left [QueueProviderUnavailable "pubsub"]
                    Nothing -> Left [QueueUrlUnrecognised url]
  where
    -- The provider knobs stay at their defaults. The operator's redelivery budget comes
    -- in as the floor, and the built backend raises it past any attached terminus.
    sqsConfigFor :: Text -> Text -> SqsConfig
    sqsConfigFor url region =
        (defaultSqsConfig url region)
            { sqsMaxReceiveCount = DeliveryBudget (qsMaxReceiveCount (cfgQueue env))
            }

    -- AWS_REGION, required only under the endpoint override, because a real SQS URL
    -- carries its region in its host. A blank value counts as absent.
    regionE :: Either BootError Text
    regionE = maybeToRight QueueRegionMissing (nonBlank =<< ambientAwsRegion ambient)

    endpointE :: Text -> Either BootError AwsEndpoint
    endpointE = first QueueEndpointMalformed . parseEndpointUrl

{- | The loud boot warning a 'MirrorQueuePlan' warrants, or 'Nothing' for a durable
backend. The composition root logs the 'Just' at @WarningS@ when it selects the plan.
-}
mirrorQueuePlanWarning :: MirrorQueuePlan -> Maybe Text
mirrorQueuePlanWarning = \case
    SqsBackend _ -> Nothing
    MemoryBackend -> Just memoryQueueBootWarning

-- | The boot warning for a rollover to the in-memory queue (no @ECLUSE_QUEUE__URL@).
memoryQueueBootWarning :: Text
memoryQueueBootWarning =
    "no ECLUSE_QUEUE__URL is set, so the mirror queue is IN-MEMORY, NON-DURABLE, and BEST-EFFORT. "
        <> "Jobs are dropped on cap overflow and lost on restart or redeploy; each is re-mirrored on the next "
        <> "demand (no data loss, only deferred mirroring). Point ECLUSE_QUEUE__URL at a durable queue (SQS) "
        <> "for a production mirror that must not shed under load."

{- | The boot warning a built queue's dead-letter probe warrants, or 'Nothing' when none
is due. Pass the built handle's budget, not the plan's configured floor, so the warning
states what the worker will do. The memory backend stays silent because
'memoryQueueBootWarning' already says the mirror sheds jobs.
-}
deadLetterTerminusWarning :: MirrorQueuePlan -> DeliveryBudget -> Either TransportFault DeadLetterTerminus -> Maybe Text
deadLetterTerminusWarning plan budget probed = case plan of
    MemoryBackend -> Nothing
    SqsBackend{} -> case probed of
        Right TerminusAttached{} -> Nothing
        Right TerminusAbsent -> Just (noDeadLetterTerminusWarning budget)
        Left fault -> Just (terminusUnprobedWarning (tfDetail fault))

-- The no-terminus warning. It names the budget that stands in for the missing
-- dead-letter queue, so an operator sees what happens to a poison message.
noDeadLetterTerminusWarning :: DeliveryBudget -> Text
noDeadLetterTerminusWarning budget =
    "the mirror queue has NO DEAD-LETTER TERMINUS: no redrive policy is attached, so nothing captures a "
        <> "mirror job that can never be published. Écluse retires such a job itself once it has been "
        <> "delivered "
        <> show (retiringDelivery budget)
        <> " times, alarming and counting it, rather than letting it cycle until the queue's "
        <> "retention window discards it unseen. Attach a redrive policy with a dead-letter queue to keep "
        <> "poison messages for inspection."

-- The unreadable-policy warning. Boot continues on the configured budget, but Écluse
-- can no longer confirm that the budget sits above an attached terminus's capture count.
terminusUnprobedWarning :: Text -> Text
terminusUnprobedWarning detail =
    "could not read the mirror queue's redrive policy, so whether poison messages have a dead-letter "
        <> "terminus is unknown; grant sqs:GetQueueAttributes on the queue to let Écluse check. The "
        <> "configured ECLUSE_QUEUE__MAX_RECEIVE_COUNT stands as the delivery budget, and may retire a job "
        <> "before a dead-letter queue would have captured it: "
        <> detail

{- | The cap-overflow drop warning for the in-memory backend. The queue rate-limits this
report, so it does not fire once per dropped job.
-}
memoryQueueDropWarning :: Int -> Text
memoryQueueDropWarning dropped =
    "mirror queue at capacity: dropped a mirror job (drop-newest); "
        <> show dropped
        <> " job(s) dropped so far. Each is re-mirrored on the next demand; raise "
        <> "ECLUSE_QUEUE__MEMORY_MAX_DEPTH to shed fewer under load."
