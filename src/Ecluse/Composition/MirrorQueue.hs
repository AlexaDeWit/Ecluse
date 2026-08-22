-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's mirror-queue backend selection: the pure decision of
which queue this binary builds and the boot warnings the choice warrants.

'planMirrorQueue' is the single place that knows which backends this binary can
build. The composition root pattern-matches its 'MirrorQueuePlan' to make the one
constructor call, and 'mirrorQueuePlanWarning' tells it whether a boot warning is
due. Once the queue exists, 'deadLetterTerminusWarning' turns the dead-letter probe
result into the second boot warning. The decision stays here, and only the call sits
at the effectful build. Failures aggregate as
'Ecluse.Composition.BootError.BootError's, so one run reports every missing input.
The shared 'Ecluse.Config.Ambient.parseEndpointUrl' parses the SQS endpoint override.
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

import Data.Text qualified as T

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Config (
    AppConfig (..),
    Config (..),
    Mount (mountRegistries),
    QueueSettings (qsMaxReceiveCount, qsUrl),
    regMirrorTarget,
    unUrl,
 )
import Ecluse.Config.Ambient (AmbientAws (..), parseEndpointUrl)
import Ecluse.Config.QueueTarget (QueueTarget (..), parseQueueTarget)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    QueueFault (qfDetail),
    retiringDelivery,
 )
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint, sqsMaxReceiveCount), SqsEndpoint (..), defaultSqsConfig)

{- | Whether this deployment runs a mirror runtime at all. With zero mirroring mounts
there is no queue to build and no worker to start ('NoMirroring'). Nothing then
consults the queue configuration, so a serve-only deployment boots with no queue
variables under the shipped @sqs@ default. With at least one mirroring mount, the
queue selection applies ('MirrorWith').
-}
data MirrorRuntimePlan
    = -- | No mount mirrors: no queue, no enqueue buffer, no worker.
      NoMirroring
    | -- | At least one mount mirrors: build the planned queue backend.
      MirrorWith MirrorQueuePlan
    deriving stock (Eq, Show)

{- | The one decision the composition root branches the mirror runtime on. It derives
whether anything mirrors from the resolved mounts, and only then consults the queue
configuration ('planMirrorQueue'). A serve-only deployment can therefore never fail
boot over queue variables it does not need.
-}
planMirrorRuntime :: AmbientAws -> Config -> Either [BootError] MirrorRuntimePlan
planMirrorRuntime ambient config
    | noneMirror = Right NoMirroring
    | otherwise = MirrorWith <$> planMirrorQueue ambient (configApp config)
  where
    noneMirror = all (isNothing . regMirrorTarget . mountRegistries) (configMounts config)

{- | Which mirror-queue backend the composition root builds, resolved from config:
the durable AWS @sqs@ backend (with its 'SqsConfig'), or the bounded best-effort
in-memory backend. This is the pure decision 'planMirrorQueue' yields. The
composition root pattern-matches it to make the one constructor call, and
'mirrorQueuePlanWarning' tells it whether a boot warning is due.

Selection needs no sizes. The in-memory backend's depth cap is a memory-plan tenant
allocated __after__ this choice, because only the memory backend spends heap on
queued jobs. The cap therefore parametrises the build
('Ecluse.Boot.buildMirrorQueue'), never the plan.
-}
data MirrorQueuePlan
    = -- | The durable AWS SQS backend, built by @Ecluse.Runtime.Queue.Sqs.newSqsQueue@.
      SqsBackend SqsConfig
    | {- | The bounded in-memory backend, built by
      'Ecluse.Core.Queue.newBoundedInMemoryQueue'. Non-durable and best-effort, so boot warns.
      -}
      MemoryBackend
    deriving stock (Eq, Show)

{- | Select the mirror-queue backend from the queue URL's shape and the ambient SDK
environment. It yields the 'MirrorQueuePlan' the composition root builds the queue
from, or the aggregated boot errors that block it.

This is the pure half of the queue's backend choice, the single place that knows
which backends this binary can build. There is no backend selector. The operator
points @ECLUSE_QUEUE__URL@ at a destination, and the backend follows from its shape
("Ecluse.Config.QueueTarget", the queue's counterpart of the mirror-credential
derivation). A backend\/URL disagreement is therefore unrepresentable.

A real SQS queue URL resolves to a 'SqsBackend' carrying its 'SqsConfig'. That
'SqsConfig' takes its region from the URL's own host, never from @AWS_REGION@. The
composition root passes it to @Ecluse.Runtime.Queue.Sqs.newSqsQueue@. A Pub\/Sub
topic resource names the GCP backend, which this binary recognises but does not
build. That is a fail-loud 'QueueProviderUnavailable', never a silent fall-through.
Any other shape is a fail-loud 'QueueUrlUnrecognised' naming the accepted forms.

An __absent__ @ECLUSE_QUEUE__URL@ rolls over to the bounded in-memory
'MemoryBackend'. The memory plan allocates its depth cap after this selection, and
that cap parametrises only the build. Mirroring is demand-driven and self-healing: a
job lost to a restart re-enqueues on the next demand. The rollover therefore degrades
durability, never safety, and the composition root emits the
'memoryQueueBootWarning' so it is never a silent surprise.

An endpoint override (@AWS_ENDPOINT_URL_SQS@, the AWS-SDK-standard service-specific
variable) __forces__ the SQS interpretation of the queue URL whatever its shape. An
emulator (@ministack@) or VPC endpoint URL matches no public shape by design. The
ambient @AWS_REGION@ must then scope it, and a missing one is the
'QueueRegionMissing' boot error. This override is the only path that still raises it.
The override parses into the backend's 'SqsEndpoint', and a malformed one is a
fail-loud 'QueueEndpointMalformed', aggregated with the region failure so one boot
reports both.

The generic @AWS_ENDPOINT_URL@ is deliberately __not__ consulted here. It is the S3
advisory client's override, and honouring it for the queue would let an S3-only
override silently redirect the queue's traffic. With no override, the SQS backend
uses AWS's default endpoint and credential resolution.
-}
planMirrorQueue :: AmbientAws -> AppConfig -> Either [BootError] MirrorQueuePlan
planMirrorQueue ambient env = case qsUrl (cfgQueue env) of
    -- No queue URL: the bounded in-memory queue, a graceful rollover (loudly
    -- warned), never a boot failure, because there is nothing to misconfigure. Its
    -- depth cap is the memory plan's to allocate, after this selection.
    Nothing -> Right MemoryBackend
    Just queueUrl ->
        let url = unUrl queueUrl
         in case nonBlank =<< ambientAwsEndpointUrlSqs ambient of
                Just override -> case (regionE, endpointE override) of
                    (Right region, Right endpoint) ->
                        Right (SqsBackend (sqsConfigFor url region){sqsEndpoint = Just endpoint})
                    (r, e) -> Left (lefts [void r, void e])
                Nothing -> case parseQueueTarget url of
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
    regionE = case T.strip <$> ambientAwsRegion ambient of
        Just region | not (T.null region) -> Right region
        _ -> Left QueueRegionMissing

    endpointE :: Text -> Either BootError SqsEndpoint
    endpointE override = case parseEndpointUrl override of
        Nothing -> Left (QueueEndpointMalformed override)
        Just (secure, host, port) ->
            Right SqsEndpoint{endpointSecure = secure, endpointHost = host, endpointPort = port}

{- | The loud boot warning a 'MirrorQueuePlan' warrants before its queue is built, or
'Nothing' for a durable backend that needs none. The composition root logs the
'Just' at @WarningS@ on selection. An operator who chose the in-memory backend then
learns plainly that the mirror is non-durable, never a silent surprise.
-}
mirrorQueuePlanWarning :: MirrorQueuePlan -> Maybe Text
mirrorQueuePlanWarning = \case
    SqsBackend _ -> Nothing
    MemoryBackend -> Just memoryQueueBootWarning

{- | The boot warning for a rollover to the in-memory queue (no @ECLUSE_QUEUE__URL@).
It states plainly that the mirror is in-memory, non-durable, and best-effort. It also
states that a lost job is re-mirrored on the next demand, so there is no data loss,
only deferred mirroring. Nobody then mistakes the rollover for a durable cloud
backend.
-}
memoryQueueBootWarning :: Text
memoryQueueBootWarning =
    "no ECLUSE_QUEUE__URL is set, so the mirror queue is IN-MEMORY, NON-DURABLE, and BEST-EFFORT. "
        <> "Jobs are dropped on cap overflow and lost on restart or redeploy; each is re-mirrored on the next "
        <> "demand (no data loss, only deferred mirroring). Point ECLUSE_QUEUE__URL at a durable queue (SQS) "
        <> "for a production mirror that must not shed under load."

{- | The boot warning a built queue's dead-letter probe warrants, or 'Nothing' when
none is due. The composition root logs the 'Just' at @WarningS@ once, right after the
queue is built. It passes the __built handle's__ budget, not the plan's configured
floor, so the warning states what the worker will do.

A durable queue with no redrive policy attached is the case worth warning about.
Nothing captures a mirror job the queue can never publish. The worker's own delivery
budget then becomes the only terminus. A failed probe warrants a warning too, because
the budget may then retire a job a dead-letter queue would capture. The in-memory
backend stays silent here. 'memoryQueueBootWarning' already says that mirror is
non-durable and sheds jobs, and a second line on the same fact would dilute it.
-}
deadLetterTerminusWarning :: MirrorQueuePlan -> DeliveryBudget -> Either QueueFault DeadLetterTerminus -> Maybe Text
deadLetterTerminusWarning plan budget probed = case plan of
    MemoryBackend -> Nothing
    SqsBackend{} -> case probed of
        Right TerminusAttached{} -> Nothing
        Right TerminusAbsent -> Just (noDeadLetterTerminusWarning budget)
        Left fault -> Just (terminusUnprobedWarning (qfDetail fault))

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

{- | The cap-overflow drop warning for the in-memory backend, carrying the running
total of dropped jobs. The queue rate-limits this report, so it does not fire per
dropped job.
-}
memoryQueueDropWarning :: Int -> Text
memoryQueueDropWarning dropped =
    "mirror queue at capacity: dropped a mirror job (drop-newest); "
        <> show dropped
        <> " job(s) dropped so far. Each is re-mirrored on the next demand; raise "
        <> "ECLUSE_QUEUE__MEMORY_MAX_DEPTH to shed fewer under load."
