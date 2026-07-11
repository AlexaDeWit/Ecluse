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
    QueueBackend (..),
    Url,
    unUrl,
 )
import Ecluse.Core.Queue.Memory (MemoryQueueConfig, defaultMemoryQueueConfig)
import Ecluse.Core.Security (splitHostPort)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint), SqsEndpoint (..), defaultSqsConfig)

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

{- | Select the mirror-queue backend from the environment layer, yielding the
'MirrorQueuePlan' the composition root builds the queue from, or the aggregated boot
errors that block it.

This is the pure half of the queue's backend choice -- the single place that knows
which backends this binary can build. The AWS @sqs@ backend resolves to a
'SqsBackend' carrying its 'SqsConfig' (the queue URL and region, with the provider
knobs at their defaults); the composition root passes that to
@Ecluse.Runtime.Queue.Sqs.newSqsQueue@. The @memory@ backend resolves to a 'MemoryBackend'
carrying its depth cap, built in-process with no cloud queue (@ECLUSE_QUEUE_URL@ and
@AWS_REGION@ are not consulted for it) -- an explicit operator choice for a simple,
single-node, or air-gapped deployment, never an automatic fallback (which would
soften the fail-loud-on-misconfig posture); the composition root emits the
'memoryQueueBootWarning' on selection. The GCP @pubsub@ arm is recognised but not
built, so it is a fail-loud 'QueueProviderUnavailable' boot error rather than a
silent fall-through. @ECLUSE_QUEUE_URL@ is optional at the env layer; it is required
__here__ for @sqs@ (the jobs need a queue), so a missing one is a fail-loud
'QueueUrlMissing' boot error, and a missing @AWS_REGION@ under @sqs@ is a
'QueueRegionMissing' boot error -- the @sqs@ arm aggregates the region, queue-URL, and
endpoint failures, and the whole result is a list so it aggregates with the rest of
the boot-time validation.

When an endpoint override is configured (@AWS_ENDPOINT_URL_SQS@, else
@AWS_ENDPOINT_URL@ -- the AWS-SDK-standard variables), it is parsed into the
backend's 'SqsEndpoint' so the released image can target a local emulator
(@ministack@) or a VPC endpoint without a test-only code path; a malformed override URL is
a fail-loud 'QueueEndpointMalformed' boot error. With no override, the SQS backend
uses AWS's default endpoint and credential resolution.
-}
planMirrorQueue :: AppConfig -> Either [BootError] MirrorQueuePlan
planMirrorQueue env = case cfgQueueBackend env of
    PubSubQueue -> Left [QueueProviderUnavailable PubSubQueue]
    -- The in-memory backend needs no cloud queue: ECLUSE_QUEUE_URL and AWS_REGION are
    -- not consulted, so it can never fail on a missing one.
    MemoryQueue -> Right (MemoryBackend (defaultMemoryQueueConfig (cfgQueueMemoryMaxDepth env)))
    SqsQueue -> case (regionE, urlE, resolveSqsEndpoint env) of
        (Right region, Right url, Right endpoint) ->
            Right (SqsBackend (defaultSqsConfig (unUrl url) region){sqsEndpoint = endpoint})
        (_, _, endpointE) ->
            -- Aggregate every SQS-resolution failure (missing region, missing queue
            -- URL, malformed endpoint) so one boot reports them all at once.
            Left (lefts [void regionE, void urlE] <> fromLeft [] endpointE)
  where
    -- AWS_REGION, required to scope the SQS queue; a blank value is treated as absent.
    regionE :: Either BootError Text
    regionE = case T.strip <$> cfgAwsRegion env of
        Just region | not (T.null region) -> Right region
        _ -> Left QueueRegionMissing

    -- ECLUSE_QUEUE_URL is optional at the env layer; it is required here for SQS (the
    -- jobs need a queue to be sent to), an absent one being a fail-loud boot error.
    urlE :: Either BootError Url
    urlE = maybe (Left (QueueUrlMissing SqsQueue)) Right (cfgQueueUrl env)

{- | The loud boot warning a 'MirrorQueuePlan' warrants before its queue is built, or
'Nothing' for a durable backend that needs none. The composition root logs the
'Just' at @WarningS@ on selection, so an operator who chose the in-memory backend is
told plainly that the mirror is non-durable -- never a silent surprise.
-}
mirrorQueuePlanWarning :: MirrorQueuePlan -> Maybe Text
mirrorQueuePlanWarning = \case
    SqsBackend _ -> Nothing
    MemoryBackend _ -> Just memoryQueueBootWarning

{- | The boot warning emitted when the in-memory mirror-queue backend is selected: it
states plainly that the mirror is in-memory, non-durable, and best-effort, and that a
lost job is re-mirrored on the next demand (so there is no data loss, only deferred
mirroring), so the choice is never mistaken for a durable cloud backend.
-}
memoryQueueBootWarning :: Text
memoryQueueBootWarning =
    "mirror queue provider 'memory' selected: the mirror queue is IN-MEMORY, NON-DURABLE, and BEST-EFFORT. "
        <> "Jobs are dropped on cap overflow and lost on restart or redeploy; each is re-mirrored on the next "
        <> "demand (no data loss, only deferred mirroring). Use a durable backend ('sqs') for a production mirror "
        <> "that must not shed under load."

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

{- Resolve the optional SQS endpoint override into an 'SqsEndpoint', or 'Nothing' for
AWS's default resolution. The AWS-SDK-standard @AWS_ENDPOINT_URL_SQS@ takes precedence
over the generic @AWS_ENDPOINT_URL@; the override URL is parsed into its TLS flag,
host, and port, and the request signing keys are taken from the standard
@AWS_ACCESS_KEY_ID@\/@AWS_SECRET_ACCESS_KEY@ (an emulator is off the ambient chain).
A malformed override URL is a fail-loud boot error. -}
resolveSqsEndpoint :: AppConfig -> Either [BootError] (Maybe SqsEndpoint)
resolveSqsEndpoint env =
    case nonBlank =<< cfgAwsEndpointUrlSqs env of
        Nothing -> Right Nothing
        Just url -> case parseEndpointUrl url of
            Nothing -> Left [QueueEndpointMalformed url]
            Just (secure, host, port) ->
                Right (Just SqsEndpoint{endpointSecure = secure, endpointHost = host, endpointPort = port})

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
