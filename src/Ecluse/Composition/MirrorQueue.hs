-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's mirror-queue backend selection: the pure decision of
which queue this binary builds, the boot warnings the choice warrants, and the
endpoint-URL parsing the cloud backend's override needs.

'planMirrorQueue' is the single place that knows which backends this binary can
build; the composition root pattern-matches its 'MirrorQueuePlan' to make the one
constructor call, and 'mirrorQueuePlanWarning' tells it whether a boot warning is
due. Failures aggregate as 'Ecluse.Composition.BootError.BootError's, so one run
reports every missing input.
-}
module Ecluse.Composition.MirrorQueue (
    MirrorRuntimePlan (..),
    planMirrorRuntime,
    MirrorQueuePlan (..),
    planMirrorQueue,
    mirrorQueuePlanWarning,
    memoryQueueBootWarning,
    memoryQueueDropWarning,
    parseEndpointUrl,
) where

import Data.Text qualified as T

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Config (
    AppConfig (..),
    Config (..),
    Mount (mountRegistries),
    regMirrorTarget,
    unUrl,
 )
import Ecluse.Config.Ambient (AmbientAws (..))
import Ecluse.Config.QueueTarget (QueueTarget (..), parseQueueTarget)
import Ecluse.Core.Queue.Memory (MemoryQueueConfig, defaultMemoryQueueConfig)
import Ecluse.Core.Security (splitHostPort)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint), SqsEndpoint (..), defaultSqsConfig)

{- | Whether this deployment runs a mirror runtime at all: with zero mirroring
mounts there is no queue to build and no worker to start ('NoMirroring'), and the
queue configuration is not even consulted, so a serve-only deployment boots with no
queue variables under the shipped @sqs@ default. With at least one mirroring mount,
exactly today's queue selection applies ('MirrorWith').
-}
data MirrorRuntimePlan
    = -- | No mount mirrors: no queue, no enqueue buffer, no worker.
      NoMirroring
    | -- | At least one mount mirrors: build the planned queue backend.
      MirrorWith MirrorQueuePlan
    deriving stock (Eq, Show)

{- | The one decision the composition root branches the mirror runtime on: derive
whether anything mirrors from the resolved mounts, and only then consult the queue
configuration ('planMirrorQueue'), so a serve-only deployment can never fail boot
over queue variables it does not need.
-}
planMirrorRuntime :: AmbientAws -> Config -> Either [BootError] MirrorRuntimePlan
planMirrorRuntime ambient config
    | noneMirror = Right NoMirroring
    | otherwise = MirrorWith <$> planMirrorQueue ambient (configApp config)
  where
    noneMirror = all (isNothing . regMirrorTarget . mountRegistries) (configMounts config)

{- | Which mirror-queue backend the composition root will build, resolved from
config: the durable AWS @sqs@ backend (with its 'SqsConfig'), or the bounded
best-effort in-memory backend (with its 'MemoryQueueConfig'). The pure decision
'planMirrorQueue' yields; the composition root pattern-matches it to make the one
constructor call, and 'mirrorQueuePlanWarning' tells it whether a boot warning is due.
-}
data MirrorQueuePlan
    = -- | The durable AWS SQS backend, built by @Ecluse.Runtime.Queue.Sqs.newSqsQueue@.
      SqsBackend SqsConfig
    | {- | The bounded in-memory backend, built by
      'Ecluse.Core.Queue.newBoundedInMemoryQueue'. Non-durable and best-effort -- boot warns.
      -}
      MemoryBackend MemoryQueueConfig
    deriving stock (Eq, Show)

{- | Select the mirror-queue backend from the queue URL's shape and the ambient SDK
environment, yielding the 'MirrorQueuePlan' the composition root builds the queue
from, or the aggregated boot errors that block it.

This is the pure half of the queue's backend choice -- the single place that knows
which backends this binary can build. There is no backend selector: the operator
points @ECLUSE_QUEUE_URL@ at a destination and the backend is derived from its
shape ("Ecluse.Config.QueueTarget", the queue's counterpart of the mirror-credential
derivation), so a backend\/URL disagreement is unrepresentable. A real SQS queue
URL resolves to a 'SqsBackend' carrying its 'SqsConfig', with the region parsed
from the URL's own host (@AWS_REGION@ is not consulted for it); the composition
root passes that to @Ecluse.Runtime.Queue.Sqs.newSqsQueue@. A Pub\/Sub topic
resource names the GCP backend, which is recognised but not built, so it is a
fail-loud 'QueueProviderUnavailable' rather than a silent fall-through, and any
other shape is a fail-loud 'QueueUrlUnrecognised' naming the accepted forms. An
__absent__ @ECLUSE_QUEUE_URL@ rolls over to the bounded in-memory 'MemoryBackend'
carrying its depth cap: mirroring is demand-driven and self-healing (a job lost to
a restart re-enqueues on the next demand), so the rollover degrades durability,
never safety, and the composition root emits the 'memoryQueueBootWarning' so it is
never a silent surprise.

When an endpoint override is set (@AWS_ENDPOINT_URL_SQS@, the AWS-SDK-standard
service-specific variable), it __forces__ the SQS interpretation of the queue URL
regardless of shape -- an emulator (@ministack@) or VPC endpoint URL matches no
public shape by design -- and the ambient @AWS_REGION@ must scope it (a missing one
is the 'QueueRegionMissing' boot error, this override being the only path that
still raises it). The override is parsed into the backend's 'SqsEndpoint'; a
malformed one is a fail-loud 'QueueEndpointMalformed', aggregated with the region
failure so one boot reports both. The generic @AWS_ENDPOINT_URL@ is deliberately
__not__ consulted here: it is the S3 advisory client's override, and honouring it
for the queue would let an S3-only override silently redirect the queue's traffic.
With no override, the SQS backend uses AWS's default endpoint and credential
resolution.
-}
planMirrorQueue :: AmbientAws -> AppConfig -> Either [BootError] MirrorQueuePlan
planMirrorQueue ambient env = case cfgQueueUrl env of
    -- No queue URL: the bounded in-memory queue, a graceful rollover (loudly
    -- warned), never a boot failure -- there is nothing to misconfigure.
    Nothing -> Right memoryPlan
    Just queueUrl ->
        let url = unUrl queueUrl
         in case nonBlank =<< ambientAwsEndpointUrlSqs ambient of
                Just override -> case (regionE, endpointE override) of
                    (Right region, Right endpoint) ->
                        Right (SqsBackend (defaultSqsConfig url region){sqsEndpoint = Just endpoint})
                    (r, e) -> Left (lefts [void r, void e])
                Nothing -> case parseQueueTarget url of
                    Just (SqsTarget region) -> Right (SqsBackend (defaultSqsConfig url region))
                    Just (PubSubTarget _project _topic) -> Left [QueueProviderUnavailable "pubsub"]
                    Nothing -> Left [QueueUrlUnrecognised url]
  where
    memoryPlan :: MirrorQueuePlan
    memoryPlan = MemoryBackend (defaultMemoryQueueConfig (cfgQueueMemoryMaxDepth env))

    -- AWS_REGION, required only under the endpoint override (a real SQS URL carries
    -- its region in its host); a blank value is treated as absent.
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
'Just' at @WarningS@ on selection, so an operator who chose the in-memory backend is
told plainly that the mirror is non-durable -- never a silent surprise.
-}
mirrorQueuePlanWarning :: MirrorQueuePlan -> Maybe Text
mirrorQueuePlanWarning = \case
    SqsBackend _ -> Nothing
    MemoryBackend _ -> Just memoryQueueBootWarning

{- | The boot warning emitted when mirroring rolls over to the in-memory queue (no
@ECLUSE_QUEUE_URL@): it states plainly that the mirror is in-memory, non-durable,
and best-effort, and that a lost job is re-mirrored on the next demand (so there is
no data loss, only deferred mirroring), so the rollover is never mistaken for a
durable cloud backend.
-}
memoryQueueBootWarning :: Text
memoryQueueBootWarning =
    "no ECLUSE_QUEUE_URL is set, so the mirror queue is IN-MEMORY, NON-DURABLE, and BEST-EFFORT. "
        <> "Jobs are dropped on cap overflow and lost on restart or redeploy; each is re-mirrored on the next "
        <> "demand (no data loss, only deferred mirroring). Point ECLUSE_QUEUE_URL at a durable queue (SQS) "
        <> "for a production mirror that must not shed under load."

{- | The cap-overflow drop warning for the in-memory backend, carrying the running
total of dropped jobs (this report is rate-limited at the queue, so it does not fire
per dropped job). A note on a one-line follow-up: a drop __metric__
(@ecluse.mirror.*@, S26 PR2) hooks in alongside this log once that catalogue lands.
-}
memoryQueueDropWarning :: Int -> Text
memoryQueueDropWarning dropped =
    "mirror queue at capacity: dropped a mirror job (drop-newest); "
        <> show dropped
        <> " job(s) dropped so far. Each is re-mirrored on the next demand; raise "
        <> "ECLUSE_QUEUE_MEMORY_MAX_DEPTH to shed fewer under load."

{- | Parse an endpoint URL into its (TLS flag, host, port). The scheme picks the TLS
flag and the default port (443\/80) when none is given; an absent scheme or a
non-numeric port yields 'Nothing'. The @host[:port]@ authority is split by the
shared bracket-aware 'Ecluse.Core.Security.splitHostPort', so a bracketed IPv6 literal
(@[::1]:4566@) is split on its closing bracket, not on an inner colon, and the host
is returned without brackets -- the same primitive the data-plane host extractor
uses, so the two cannot drift on an authority edge case.
-}
parseEndpointUrl :: Text -> Maybe (Bool, Text, Int)
parseEndpointUrl raw = do
    (secure, afterScheme) <-
        ((True,) <$> T.stripPrefix "https://" raw) <|> ((False,) <$> T.stripPrefix "http://" raw)
    let authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
    (hostText, portText) <- splitHostPort authority
    host <- nonBlank hostText
    port <- case T.stripPrefix ":" portText of
        Nothing -> Just (if secure then 443 else 80)
        Just digits -> readMaybe (toString digits)
    pure (secure, host, port)
