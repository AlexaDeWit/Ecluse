-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared authentication, admission shedding, flight keys, and refusal handling.
Route handlers keep their own response formats while sharing these policy decisions.
-}
module Ecluse.Core.Server.Pipeline.Shared (
    -- * Edge authentication
    edgeTokenMatches,
    forwardedCredential,
    unauthorisedMessage,
    privateAuthorisationRefusal,

    -- * Admission shed
    withMetadataAdmission,
    withMetadataAdmissionC,
    spikeStatsLoop,
    spikeEnableQueue,
    shedStatus,
    shedMessage,
    hRetryAfter,
    shedRetryAfter,
    retryAfterHeaders,
    upstreamCooling,

    -- * Shared work
    flightOf,

    -- * First-party refusals
    firstPartyRule,

    -- * Integrity-floor rejections
    integrityMissing,
    integrityBelowFloor,
    trustedIntegrityMissing,
    trustedIntegrityBelowFloor,
) where

import Network.HTTP.Types (Header, HeaderName, ResponseHeaders, Status, status503)
import Network.Wai (Request, requestHeaders)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Exception (finally)

import Ecluse.Core.Credential (ClientCredential (credSecret), Secret)
import Ecluse.Core.Registry.Request (credentialRecover)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.STM (check, orElse)

import Data.Sequence qualified as Seq
import Ecluse.Core.Deadline (holdSwept, withSweptFlag)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Server.Admission (withServeAdmissionP)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, withMemoryEntryP)
import Ecluse.Core.Server.Admission.Types (FlightKey (FlightKey))

import Ecluse.Core.Server.Admission.Weighted (Shed (..), admissionWaitMicros, spikePlacesFreed, spikeSignalOne, spikeWaiters)
import Ecluse.Core.Server.Cache.Types (CacheKey, cacheKeyIdentity, cacheKeyStore)
import Ecluse.Core.Server.Context (MountBinding (bindingCredential), ServeRuntime (srAdmission, srMemoryMeter, srMetrics, srPublicClient))
import Ecluse.Core.Server.Response (
    HelpMessage,
    Refusal,
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    RetryAfter (RetryAfter),
    RuleName (RuleName),
    ServeDecision (Reject),
    Transience (WillResolve),
    mkRefusal,
    rejectUnavailable,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpServeDecision))

-- | The fixed refusal shared by private metadata and artifact access failures.
privateAuthorisationRefusal :: Maybe HelpMessage -> Refusal
privateAuthorisationRefusal help = mkRefusal help "the private upstream refused access, so public content cannot replace it"

hRetryAfter :: HeaderName
hRetryAfter = "Retry-After"

-- | Use 503 to report server admission capacity, without implying a client rate limit.
shedStatus :: Status
shedStatus = status503

-- | The shed retry delay, rounded down to whole seconds from 'admissionWaitMicros'.
shedRetryAfter :: Header
shedRetryAfter = (hRetryAfter, show (admissionWaitMicros `div` 1_000_000))

-- | The body every read-path shed answers with, so the three handlers say one thing.
shedMessage :: Text
shedMessage = "server is busy; retry later"

-- | Emit Retry-After only when a decision supplies a delay.
retryAfterHeaders :: Maybe RetryAfter -> ResponseHeaders
retryAfterHeaders = maybe [] (\(RetryAfter secs) -> [(hRetryAfter, show secs)])

-- | The flight that the shared fetch or render behind a cache key runs under: the key's store and identity.
flightOf :: CacheKey -> FlightKey
flightOf key = FlightKey (cacheKeyStore key) (cacheKeyIdentity key)

-- | Acquire memory and CPU admission before metadata work. Answer after release, counting a shed once.
withMetadataAdmission :: (MonadUnliftIO m) => ServeRuntime -> m received -> (MemoryTicket -> m a) -> (a -> m received) -> m received
withMetadataAdmission runtime shed = withMetadataAdmissionC runtime (const shed)

-- Spike 1866: how a request the door refused is handled.
data SpikeShed = ShedNow | ShedHold | ShedLobby | ShedLobbyFair | ShedLobbyEvent | ShedLobbyWake | ShedLobbyEnd | ShedPause
    deriving stock (Eq, Show)

spikeEnv :: String -> Maybe String
spikeEnv key = unsafePerformIO (lookupEnv key)
{-# NOINLINE spikeEnv #-}

-- The arm SPIKE1866_SHED names, if any.
spikeShedFromEnv :: Maybe SpikeShed
spikeShedFromEnv = case spikeEnv "SPIKE1866_SHED" of
    Just "now" -> Just ShedNow
    Just "hold" -> Just ShedHold
    Just "lobby" -> Just ShedLobby
    Just "lobby-fair" -> Just ShedLobbyFair
    Just "lobby-event" -> Just ShedLobbyEvent
    Just "lobby-wake" -> Just ShedLobbyWake
    Just "lobby-end" -> Just ShedLobbyEnd
    Just "pause" -> Just ShedPause
    _ -> Nothing
{-# NOINLINE spikeShedFromEnv #-}

-- Today's immediate refusal until the service boots, so a runtime a test builds directly keeps it.
spikeShedRef :: IORef SpikeShed
spikeShedRef = unsafePerformIO (newIORef (fromMaybe ShedNow spikeShedFromEnv))
{-# NOINLINE spikeShedRef #-}

-- | Spike 1866: the service's boot turns the queue on, unless SPIKE1866_SHED names another arm.
spikeEnableQueue :: IO ()
spikeEnableQueue = when (isNothing spikeShedFromEnv) (writeIORef spikeShedRef ShedLobbyWake)

-- The wait, in microseconds.
spikeWaitMicros :: Int
spikeWaitMicros = maybe 500_000 (* 1000) (readMaybe =<< spikeEnv "SPIKE1866_WAIT_MS")
{-# NOINLINE spikeWaitMicros #-}

-- How many requests may wait at once.
spikeCap :: Int
spikeCap = fromMaybe 320 (readMaybe =<< spikeEnv "SPIKE1866_CAP")
{-# NOINLINE spikeCap #-}

-- Close on every shed (all), past the cap (over), or never.
spikeCloseAll, spikeCloseOver :: Bool
spikeCloseAll = spikeEnv "SPIKE1866_CLOSE" == Just "all"
spikeCloseOver = spikeEnv "SPIKE1866_CLOSE" == Just "over"
{-# NOINLINE spikeCloseAll #-}
{-# NOINLINE spikeCloseOver #-}

spikeHeld :: IORef Int
spikeHeld = unsafePerformIO (newIORef 0)
{-# NOINLINE spikeHeld #-}

-- Counters: first-attempt door refusals, first-attempt wait expiries, entered the hold, past the cap,
-- admitted after holding, refused after holding, door retries while held, most held at once.
spikeCounters :: IORef (Int, Int, Int, Int, Int, Int, Int, Int)
spikeCounters = unsafePerformIO (newIORef (0, 0, 0, 0, 0, 0, 0, 0))
{-# NOINLINE spikeCounters #-}

data SpikeCount = CDoor | CExpired | CEntered | COver | CAdmitted | CRefused | CRetry

spikeCount :: SpikeCount -> IO ()
spikeCount which = atomicModifyIORef' spikeCounters $ \(a, b, c, d, e, f, g, h) -> (,()) $ case which of
    CDoor -> (a + 1, b, c, d, e, f, g, h)
    CExpired -> (a, b + 1, c, d, e, f, g, h)
    CEntered -> (a, b, c + 1, d, e, f, g, h)
    COver -> (a, b, c, d + 1, e, f, g, h)
    CAdmitted -> (a, b, c, d, e + 1, f, g, h)
    CRefused -> (a, b, c, d, e, f + 1, g, h)
    CRetry -> (a, b, c, d, e, f, g + 1, h)

-- | Spike 1866: append the counters to SPIKE1866_STATS_FILE every second. Never returns.
spikeStatsLoop :: IO [Int] -> (Int -> IO ()) -> IO ()
spikeStatsLoop gauges pause = case spikeEnv "SPIKE1866_STATS_FILE" of
    Nothing -> pass
    Just path -> forever $ do
        pause 250_000
        levels <- gauges
        held <- readIORef spikeHeld
        (a, b, c, d, e, f, g, h) <- atomicModifyIORef' spikeCounters (\(a, b, c, d, e, f, g, h) -> let m = max h held in ((a, b, c, d, e, f, g, m), (a, b, c, d, e, f, g, m)))
        t <- getMonotonicTimeNSec
        appendFileText path (unwords (map show ([fromIntegral (t `div` 1_000_000) :: Int, a, b, c, d, e, f, g, h, held] <> levels)) <> "\n")

-- Take a place among the held requests, or 'False' past the cap.
spikeEnterHold :: IO Bool
spikeEnterHold = do
    entered <- atomicModifyIORef' spikeHeld (\n -> if n < spikeCap then (n + 1, True) else (n, False))
    spikeCount (if entered then CEntered else COver)
    when entered $ do
        held <- readIORef spikeHeld
        atomicModifyIORef' spikeCounters (\(a, b, c, d, e, f, g, h) -> ((a, b, c, d, e, f, g, max h held), ()))
    pure entered

spikeLeaveHold :: IO ()
spikeLeaveHold = atomicModifyIORef' spikeHeld (\n -> (n - 1, ()))

-- | Spike 1866: 'withMetadataAdmission' whose shed answer learns whether to close the connection.
withMetadataAdmissionC :: (MonadUnliftIO m) => ServeRuntime -> (Bool -> m received) -> (MemoryTicket -> m a) -> (a -> m received) -> m received
withMetadataAdmissionC runtime shed gated answer =
    liftIO (readIORef spikeShedRef) >>= \mode ->
        firstTry mode >>= \case
            Right result -> answer result
            Left WaitExpired -> liftIO (spikeCount CExpired) >> refuse False
            Left DoorFull ->
                liftIO (spikeCount CDoor) >> case mode of
                    ShedNow -> refuse False
                    ShedLobbyWake -> do
                        entered <- liftIO spikeEnterHold
                        if not entered
                            then refuse spikeCloseOver
                            else do
                                outcome <- withRunInIO (\run -> wakeLobby run)
                                case outcome of
                                    Right result -> liftIO (spikeCount CAdmitted) >> answer result
                                    Left _ -> liftIO (spikeCount CRefused) >> refuse False
                    ShedPause -> do
                        entered <- liftIO spikeEnterHold
                        if entered
                            then do
                                received <- refuse False
                                liftIO (holdSwept deadlines spikeWaitMicros `finally` spikeLeaveHold)
                                pure received
                            else refuse spikeCloseOver
                    _ -> do
                        entered <- liftIO spikeEnterHold
                        if not entered
                            then refuse spikeCloseOver
                            else do
                                start <- liftIO getMonotonicTimeNSec
                                outcome <- withRunInIO (\run -> held mode start run `finally` spikeLeaveHold)
                                case outcome of
                                    Right result -> liftIO (spikeCount CAdmitted) >> answer result
                                    Left _ -> liftIO (spikeCount CRefused) >> refuse False
  where
    metrics = srMetrics runtime
    deadlines = ecDeadlines (srPublicClient runtime)
    refuse closing = do
        liftIO (mpServeDecision metrics Metric.Unavailable)
        shed (closing || spikeCloseAll)
    -- A newcomer joins the held requests while any wait, so it never jumps them.
    firstTry mode
        | mode `elem` [ShedLobbyFair, ShedLobbyEvent] = liftIO (readIORef spikeHeld) >>= \waiting -> if waiting > 0 then pure (Left DoorFull) else admitted
        | otherwise = admitted
    admitted = admittedP pass
    admittedP placed = fmap join . withMemoryEntryP metrics (srMemoryMeter runtime) placed $ \ticket ->
        withServeAdmissionP metrics (srAdmission runtime) placed (gated ticket)
    held mode start run = case mode of
        ShedHold -> holdSwept deadlines spikeWaitMicros $> Left DoorFull
        ShedLobbyEnd -> holdSwept deadlines spikeWaitMicros >> spikeCount CRetry >> run admitted
        ShedLobbyEvent -> eventLobby run
        _ -> lobby start run
    -- Wait in arrival order for a signal that a place freed, a sweeper tick, or the end of the wait. Each try
    -- uses the normal door. The request counts as held until it holds a place, and a signal it cannot use passes on.
    wakeLobby run = do
        entry <- newTVarIO (0 :: Int)
        atomically (modifyTVar' spikeWaiters (Seq.|> entry))
        current <- newIORef entry
        let
            -- Leave the queue and the count. A signal that arrived meanwhile goes to the next waiter.
            leave = do
                me <- readIORef current
                was <- atomically $ do
                    s <- readTVar me
                    unless (s == 2) (writeTVar me 2)
                    when (s == 1) spikeSignalOne
                    pure s
                unless (was == 2) spikeLeaveHold
            -- Back to the head of the queue after a try that found no place, with a fresh entry so that no
            -- request is queued twice. An entry still waiting in the queue stays where it is.
            requeue = do
                me <- readIORef current
                was <- readTVarIO me
                unless (was == 0) $ do
                    fresh <- newTVarIO 0
                    atomically (writeTVar me 2 >> modifyTVar' spikeWaiters (fresh Seq.<|))
                    writeIORef current fresh
                    when (was == 2) (atomicModifyIORef' spikeHeld (\n -> (n + 1, ())))
            -- A signal that woke this request is spent on its try. State 3: awake, out of the queue.
            consume = readIORef current >>= \me -> atomically (readTVar me >>= \s -> when (s == 1) (writeTVar me 3))
            wait deadline = do
                me <- readIORef current
                withSweptFlag deadlines 1 $ \tick ->
                    atomically $
                        ((readTVar me >>= check . (== 1)) $> False)
                            `orElse` ((readTVar deadline >>= check) $> True)
                            `orElse` ((readTVar tick >>= check) $> False)
            loop deadline = do
                expired <- wait deadline
                if expired
                    then pure (Left DoorFull)
                    else do
                        consume
                        spikeCount CRetry
                        outcome <- run (admittedP leave)
                        case outcome of
                            Left DoorFull -> requeue >> loop deadline
                            _ -> pure outcome
        (withSweptFlag deadlines spikeWaitMicros loop `finally` leave)

    -- Wait for a freed place or the end of the wait. A request stops counting as held once it holds a place.
    eventLobby run = do
        counted <- newIORef True
        let leave = atomicModifyIORef' counted (False,) >>= \was -> when was spikeLeaveHold
            rejoin = atomicModifyIORef' counted (True,) >>= \was -> unless was (atomicModifyIORef' spikeHeld (\n -> (n + 1, ())))
            attempt flag = do
                seen <- readTVarIO spikePlacesFreed
                spikeCount CRetry
                outcome <- run (admittedP leave)
                case outcome of
                    Left DoorFull -> do
                        rejoin
                        expired <- atomically (((readTVar spikePlacesFreed >>= check . (/= seen)) $> False) `orElse` ((readTVar flag >>= check) $> True))
                        if expired then pure outcome else attempt flag
                    _ -> pure outcome
            firstWait flag = do
                seen <- readTVarIO spikePlacesFreed
                expired <- atomically (((readTVar spikePlacesFreed >>= check . (/= seen)) $> False) `orElse` ((readTVar flag >>= check) $> True))
                if expired then pure (Left DoorFull) else attempt flag
        -- The caller's 'finally' leaves the count once, so this only marks the request as already out.
        withSweptFlag deadlines spikeWaitMicros firstWait `finally` (atomicModifyIORef' counted (False,) >>= \was -> unless was (atomicModifyIORef' spikeHeld (\n -> (n + 1, ()))))
    -- Retry the door each sweeper tick until it lets the request in or the wait runs out.
    lobby start run = do
        holdSwept deadlines 100_000
        now <- getMonotonicTimeNSec
        spikeCount CRetry
        outcome <- run admitted
        case outcome of
            Left DoorFull | now - start < fromIntegral spikeWaitMicros * 1000 -> lobby start run
            _ -> pure outcome

-- | Match the configured inbound secret without content-dependent early exit. An unconfigured edge is open.
edgeTokenMatches :: Maybe Secret -> Maybe ClientCredential -> Bool
edgeTokenMatches expected forwarded = case expected of
    Nothing -> True
    Just want -> fmap credSecret forwarded == Just want

-- | Report edge authentication failure without disclosing either token.
unauthorisedMessage :: Text
unauthorisedMessage = "authentication required"

forwardedCredential :: MountBinding -> Request -> Maybe ClientCredential
forwardedCredential mount = credentialRecover (bindingCredential mount) . requestHeaders

-- | Use one rule label for first-party refusals across read paths.
firstPartyRule :: RuleName
firstPartyRule = RuleName "first-party"

integrityMissing :: ServeDecision
integrityMissing =
    Reject (Rejection MissingIntegrity "this version carries no integrity digest and cannot be served from a public upstream")

integrityBelowFloor :: ServeDecision
integrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this version's integrity digest is weaker than the configured minimum and cannot be served from a public upstream")

trustedIntegrityMissing :: ServeDecision
trustedIntegrityMissing =
    Reject (Rejection MissingIntegrity "this private version carries no integrity digest and was not served")

trustedIntegrityBelowFloor :: ServeDecision
trustedIntegrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this private version's integrity digest is weaker than the configured trusted minimum and was not served")

-- | A public authority that supplies a positive retry delay.
upstreamCooling :: RetryAfter -> ServeDecision
upstreamCooling retry = rejectUnavailable (WillResolve (Just retry)) "the public upstream is cooling down"
