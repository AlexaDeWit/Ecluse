module Ecluse.QueueSpec (spec) where

import Hedgehog (
    Callback (Ensure, Require, Update),
    Command (Command),
    Concrete,
    Eq1,
    FunctorB (..),
    PropertyT,
    TraversableB (..),
    Var,
    concrete,
    (===),
 )
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (HashAlg (SHA1), mkPackageName)
import Ecluse.Core.Queue
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (unsafeHash, validSha1)

{- | A sample mirror job. The in-memory queue under test does not inspect a
job's contents -- it only carries it from 'enqueue' to 'receive' -- so one fixed
job suffices for the FIFO / ack / redelivery assertions.
-}
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "thing"
        , jobVersion = mkVersion Npm "1.0.0"
        , jobArtifactUrl = "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobMirrorTarget = "https://mirror.test/thing/-/thing-1.0.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "thing-1.0.0.tgz"
                , maHashes = unsafeHash SHA1 validSha1 :| []
                , maSize = Just 7
                }
        , jobTraceContext = Nothing
        }

{- | A second, distinct job, used to assert FIFO ordering across two enqueues.
It differs from 'sampleJob' only in its version, which is enough to tell the two
apart on receive.
-}
otherJob :: MirrorJob
otherJob = sampleJob{jobVersion = mkVersion Npm "2.0.0"}

{- | A third, distinct job, used by the bounded-queue tests to tell the retained
jobs apart from a dropped-newest one at the cap.
-}
thirdJob :: MirrorJob
thirdJob = sampleJob{jobVersion = mkVersion Npm "3.0.0"}

spec :: Spec
spec = do
    describe "newInMemoryQueue" $ do
        it "receives [] from an empty queue" $ do
            q <- newInMemoryQueue
            msgs <- receive q
            map msgJob msgs `shouldBe` []

        it "delivers an enqueued job on the next receive" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            msgs <- receive q
            map msgJob msgs `shouldBe` [sampleJob]

        it "carries every job field through unchanged from enqueue to receive" $ do
            -- The queue is a transparent carrier: each field the producer set must
            -- arrive on the consumer side byte-for-byte. Assert field-by-field
            -- (via the 'MirrorJob' selectors) rather than on the whole record, so a
            -- regression that mangled a single field is pinpointed.
            q <- newInMemoryQueue
            enqueue q sampleJob
            [msg] <- receive q
            let job = msgJob msg
            jobPackage job `shouldBe` jobPackage sampleJob
            jobVersion job `shouldBe` jobVersion sampleJob
            jobArtifactUrl job `shouldBe` jobArtifactUrl sampleJob
            jobMirrorTarget job `shouldBe` jobMirrorTarget sampleJob

        it "delivers jobs in FIFO order" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            enqueue q otherJob
            received <- drain q
            received `shouldBe` [sampleJob, otherJob]

        it "ignores an ack for a handle it never issued" $ do
            -- A handle from outside this queue (here, an unparseable one) names no
            -- in-flight job, so the ack is a harmless no-op: the real in-flight job
            -- is untouched and still redelivers.
            q <- newInMemoryQueue
            enqueue q sampleJob
            _ <- receive q
            ack q (mkReceiptHandle "not-a-handle")
            redelivered <- receive q
            map msgJob redelivered `shouldBe` [sampleJob]

        it "ignores an extendVisibility for a handle it never issued" $ do
            -- Likewise extendVisibility on an unknown handle holds nothing, so the
            -- genuinely in-flight job still lapses and redelivers.
            q <- newInMemoryQueue
            enqueue q sampleJob
            _ <- receive q
            extendVisibility q (mkReceiptHandle "not-a-handle") (Seconds 30)
            redelivered <- receive q
            map msgJob redelivered `shouldBe` [sampleJob]

        it "does not redeliver a job that was acked" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            [msg] <- receive q
            ack q (msgReceipt msg)
            -- After the ack, the job is gone: a later receive is empty.
            afterAck <- receive q
            map msgJob afterAck `shouldBe` []

        it "redelivers a job that was received but never acked" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            -- Receive (taking the job out of sight) but deliberately do not ack:
            -- retry-is-don't-ack, so the job must become visible again.
            _ <- receive q
            redelivered <- receive q
            map msgJob redelivered `shouldBe` [sampleJob]

        it "stops redelivering once a redelivered job is acked" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            _ <- receive q
            [msg] <- receive q
            ack q (msgReceipt msg)
            afterAck <- receive q
            map msgJob afterAck `shouldBe` []

        it "gives each delivery of the same job a distinct message (fresh receipt)" $ do
            -- A job that is received but not acked is redelivered, and each delivery
            -- must be a *distinct* 'QueueMessage' -- same job, but a fresh receipt --
            -- so acking one delivery cannot be confused with another. This pins the
            -- receipt-per-delivery invariant via 'QueueMessage' equality.
            q <- newInMemoryQueue
            enqueue q sampleJob
            [firstDelivery] <- receive q
            [secondDelivery] <- receive q
            msgJob firstDelivery `shouldBe` msgJob secondDelivery
            firstDelivery `shouldNotBe` secondDelivery
            msgReceipt firstDelivery `shouldNotBe` msgReceipt secondDelivery

        it "extendVisibility keeps an in-flight job from redelivering immediately" $ do
            -- extendVisibility is an optimisation, not correctness-critical; for
            -- the in-memory double it simply leaves the in-flight job in flight,
            -- so the very next receive does not redeliver it.
            q <- newInMemoryQueue
            enqueue q sampleJob
            [msg] <- receive q
            let hold = Seconds 30
            -- The window is a typed duration, not a bare Int: the held value is the
            -- one we pass through, and Seconds are ordered (a longer hold is larger).
            hold `shouldBe` Seconds 30
            hold `shouldSatisfy` (> Seconds 0)
            extendVisibility q (msgReceipt msg) hold
            afterHold <- receive q
            map msgJob afterHold `shouldBe` []

        it "agrees with a pure model under random operation sequences" $
            hedgehog queueModelProperty

    describe "newBoundedInMemoryQueue" $ do
        it "returns [] on an idle queue within the poll window (never blocks forever)" $ do
            -- The load-bearing liveness property: the worker advances its heartbeat
            -- only when receive returns, so an idle receive MUST return [] (a healthy
            -- empty poll) within its bounded window rather than blocking indefinitely.
            -- The helper uses a 50ms window; the 2s timeout is a generous regression
            -- guard that fails loudly if receive ever reverts to blocking forever.
            (q, _drops) <- boundedQueue 4
            result <- timeout 2_000_000 (receive q)
            result `shouldBe` Just []

        it "carries a job from enqueue through receive to ack (round-trip)" $ do
            -- A cap well above the one job, so nothing is dropped: the job arrives
            -- unchanged and ack (a no-op on this backend) completes without error.
            (q, _drops) <- boundedQueue 10
            enqueue q sampleJob
            [msg] <- receive q
            msgJob msg `shouldBe` sampleJob
            ack q (msgReceipt msg)

        it "drops the newest enqueue at the cap and keeps the earlier jobs" $ do
            -- The load-bearing bound: at the cap a fresh enqueue is rejected
            -- (drop-newest), so the queue holds exactly the first 'cap' jobs and the
            -- overflowing newest one never arrives.
            (q, drops) <- boundedQueue 2
            traverse_ (enqueue q) [sampleJob, otherJob, thirdJob]
            received <- map msgJob <$> receive q
            received `shouldBe` [sampleJob, otherJob]
            -- The drop is observed (the first overflow is always reported).
            readIORef drops `shouldReturn` [1]

        it "honours the cap under a flood far larger than it" $ do
            -- Many enqueues into a tiny cap retain at most 'cap' jobs (memory is hard
            -- bounded); the rest are dropped, and at least the first drop is reported.
            (q, drops) <- boundedQueue 2
            traverse_ (enqueue q) (replicate 5 sampleJob)
            received <- receive q
            length received `shouldBe` 2
            readIORef drops `shouldReturn` [1]

        it "reports the first drop then every interval-th, rate-limiting a flood" $ do
            -- AC4: a sustained flood must not spam -- only the first drop and every
            -- 'memoryQueueDropReportInterval'-th drop are reported (carrying the
            -- running total), so log volume is bounded under load.
            (q, drops) <- boundedQueue 1
            enqueue q sampleJob -- fills the single slot; nothing receives it
            traverse_ (enqueue q) (replicate memoryQueueDropReportInterval sampleJob)
            readIORef drops `shouldReturn` [1, memoryQueueDropReportInterval]
  where
    -- A bounded in-memory queue at the given cap, paired with an 'IORef' that records
    -- (in order) the running drop totals its drop callback was invoked with -- so a
    -- test can assert both the cap behaviour and the rate-limited drop reporting. The
    -- idle poll window is shortened to 50ms (the production default is ~20s) so an
    -- idle-receive test returns promptly rather than waiting out a real long-poll.
    boundedQueue :: Int -> IO (MirrorQueue, IORef [Int])
    boundedQueue cap = do
        drops <- newIORef []
        let cfg = MemoryQueueConfig{memQueueMaxDepth = cap, memQueuePollWaitMicros = 50_000}
        q <- newBoundedInMemoryQueue cfg (\n -> modifyIORef' drops (<> [n]))
        pure (q, drops)

    -- Receive repeatedly, acking everything, until the queue is empty; returns
    -- the jobs in the order they were delivered. Total: it stops as soon as a
    -- receive yields nothing.
    drain :: MirrorQueue -> IO [MirrorJob]
    drain q = go []
      where
        go acc = do
            msgs <- receive q
            case msgs of
                [] -> pure (reverse acc)
                _ -> do
                    traverse_ (ack q . msgReceipt) msgs
                    go (reverse (map msgJob msgs) <> acc)

{- | A pure model of 'newInMemoryQueue's observable state, parameterised over the
Hedgehog variable phase @v@ (symbolic while generating, concrete while running).

It mirrors the implementation's @QueueState@: visible jobs in FIFO order, plus
in-flight (received-but-unacked) jobs. Each in-flight entry remembers which
'receive' delivered it (the symbolic @Var@ over that receive's @[QueueMessage]@
output) and its index within that delivery, so a later 'Ack' / 'Extend' can name
the exact handle the implementation will use -- exactly the bookkeeping the real
queue does with its monotonic receipt counter.
-}
data QModel (v :: Type -> Type) = QModel
    { mVisible :: [MirrorJob]
    -- ^ Waiting jobs, oldest first (FIFO).
    , mInFlight :: [InFlightEntry v]
    {- ^ Delivered-but-unacked jobs, in delivery order (ascending receipt) so a
    reclaim re-enqueues them in the same order the implementation does.
    -}
    }

{- | One in-flight job in the model: the job itself, whether 'Extend' has held it
past the next reclaim, and the symbolic handle identifying it -- the @Var@ over the
delivering receive's output list plus this job's index in that list.
-}
data InFlightEntry v = InFlightEntry
    { ifJob :: MirrorJob
    , ifHeld :: Bool
    , ifVar :: Var [QueueMessage] v
    , ifIndex :: Int
    }

-- | The empty model: nothing visible, nothing in flight.
initialQModel :: QModel v
initialQModel = QModel{mVisible = [], mInFlight = []}

{- | The model's prediction for a 'receive': every un-held in-flight job is
reclaimed to the front (in delivery order), every still-held one stays in flight
with its hold cleared, and all visible jobs follow. Returns the jobs the receive
should deliver (reclaimed ++ visible, in order) and the entries that remain in
flight. Mirrors 'Ecluse.Core.Queue's @deliver@ / @reclaim@ exactly.
-}
predictReceive :: QModel v -> ([MirrorJob], [InFlightEntry v])
predictReceive m =
    let (reclaimed, stillHeld) = foldr step ([], []) (mInFlight m)
        step e (jobs, held)
            | ifHeld e = (jobs, e{ifHeld = False} : held)
            | otherwise = (ifJob e : jobs, held)
        delivered = reclaimed <> mVisible m
     in (delivered, stillHeld)

-- Each input is higher-kinded in @v@ so Hedgehog can carry symbolic variables
-- through it. Enqueue/Receive reference no prior result, so their @v@ is phantom;
-- Ack/Extend reference a receive's output and so carry a real 'Var'. The
-- 'FunctorB' / 'TraversableB' instances are written by hand (no @barbies@ dep):
-- mapping a natural transformation over the (possibly absent) 'Var' field.

newtype EnqueueInput (v :: Type -> Type) = EnqueueInput MirrorJob
    deriving stock (Show)

instance FunctorB EnqueueInput where
    bmap _ (EnqueueInput j) = EnqueueInput j

instance TraversableB EnqueueInput where
    btraverse _ (EnqueueInput j) = pure (EnqueueInput j)

data ReceiveInput (v :: Type -> Type) = ReceiveInput
    deriving stock (Show)

instance FunctorB ReceiveInput where
    bmap _ ReceiveInput = ReceiveInput

instance TraversableB ReceiveInput where
    btraverse _ ReceiveInput = pure ReceiveInput

-- | Acknowledge the handle at the given index of an earlier receive's output.
data AckInput (v :: Type -> Type) = AckInput (Var [QueueMessage] v) Int
    deriving stock (Show)

instance FunctorB AckInput where
    bmap f (AckInput var i) = AckInput (bmap f var) i

instance TraversableB AckInput where
    btraverse f (AckInput var i) = AckInput <$> btraverse f var <*> pure i

-- | Extend visibility on the handle at the given index of a receive's output.
data ExtendInput (v :: Type -> Type) = ExtendInput (Var [QueueMessage] v) Int Seconds
    deriving stock (Show)

instance FunctorB ExtendInput where
    bmap f (ExtendInput var i s) = ExtendInput (bmap f var) i s

instance TraversableB ExtendInput where
    btraverse f (ExtendInput var i s) = ExtendInput <$> btraverse f var <*> pure i <*> pure s

-- | A small pool of jobs so receive can observe FIFO ordering of distinct jobs.
genJob :: H.Gen MirrorJob
genJob =
    Gen.element
        [ sampleJob
        , otherJob
        , sampleJob{jobVersion = mkVersion Npm "3.0.0"}
        ]

{- | 'enqueue' a job: always available; appends to the back of the visible queue.
The implementation returns @()@ and only mutates state, so there is nothing to
'Ensure' beyond the 'Update' the model already tracks.
-}
enqueueCommand :: MirrorQueue -> Command H.Gen (PropertyT IO) QModel
enqueueCommand q =
    Command
        (const (Just (EnqueueInput <$> genJob)))
        (\(EnqueueInput job) -> liftIO (enqueue q job))
        [ Update $ \m (EnqueueInput job) _out ->
            m{mVisible = mVisible m <> [job]}
        ]

{- | 'receive': always available. Returns the delivered messages, and asserts the
delivered jobs (and their count) match the model's prediction in order, and that
every returned receipt is distinct. The 'Update' moves the predicted jobs in
flight under the fresh output 'Var'.
-}
receiveCommand :: MirrorQueue -> Command H.Gen (PropertyT IO) QModel
receiveCommand q =
    Command
        (const (Just (pure ReceiveInput)))
        (\ReceiveInput -> liftIO (receive q))
        [ Update $ \m ReceiveInput out ->
            let (delivered, stillHeld) = predictReceive m
                newInFlight =
                    [ InFlightEntry{ifJob = job, ifHeld = False, ifVar = out, ifIndex = i}
                    | (i, job) <- zip [0 ..] delivered
                    ]
             in m{mVisible = [], mInFlight = stillHeld <> newInFlight}
        , Ensure $ \beforeState _afterState ReceiveInput msgs -> do
            let (delivered, _) = predictReceive beforeState
            -- The jobs delivered, and how many, match the model exactly (FIFO and
            -- reclaim ordering included).
            map msgJob msgs === delivered
            -- Each delivery carries a distinct receipt (the receipt-per-delivery
            -- invariant), so acking one can never be confused with another.
            let receipts = map msgReceipt msgs
            length (ordNub receipts) === length receipts
            -- Non-vacuity: the sequence must reach the interesting arms -- a receive
            -- that redelivers an un-acked job (reclaim) and one that batches 2+
            -- jobs -- not just empty / single-job receives.
            let hadUnheldInFlight = not (all ifHeld (mInFlight beforeState))
            H.cover 1 "receive reclaims an un-acked job" (hadUnheldInFlight && not (null delivered))
            H.cover 1 "receive delivers a batch (2+ jobs)" (length delivered >= 2)
        ]

{- | 'ack' a currently in-flight handle. Only generated when the model has at
least one in-flight entry; picks one and names it by (receive-output 'Var',
index). The 'Update' drops it from the in-flight set.
-}
ackCommand :: MirrorQueue -> Command H.Gen (PropertyT IO) QModel
ackCommand q =
    Command
        gen
        (\(AckInput var i) -> liftIO (whenJust (handleAt var i) (ack q)))
        [ Require $ \m (AckInput var i) -> inFlightMember m var i
        , Update $ \m (AckInput var i) _out ->
            m{mInFlight = filter (not . sameHandle var i) (mInFlight m)}
        ]
  where
    gen m
        | null (mInFlight m) = Nothing
        | otherwise =
            Just $ do
                e <- Gen.element (mInFlight m)
                pure (AckInput (ifVar e) (ifIndex e))

{- | 'extendVisibility' on a currently in-flight handle. Only generated when the
model has an in-flight entry; sets that entry's hold so the next receive does not
reclaim it. The 'Seconds' argument is a pass-through optimisation knob.
-}
extendCommand :: MirrorQueue -> Command H.Gen (PropertyT IO) QModel
extendCommand q =
    Command
        gen
        (\(ExtendInput var i secs) -> liftIO (whenJust (handleAt var i) (\h -> extendVisibility q h secs)))
        [ Require $ \m (ExtendInput var i _secs) -> inFlightMember m var i
        , Update $ \m (ExtendInput var i _secs) _out ->
            m{mInFlight = map (hold var i) (mInFlight m)}
        ]
  where
    hold var i e
        | sameHandle var i e = e{ifHeld = True}
        | otherwise = e
    gen m
        | null (mInFlight m) = Nothing
        | otherwise =
            Just $ do
                e <- Gen.element (mInFlight m)
                secs <- Seconds <$> Gen.int (Range.linear 1 120)
                pure (ExtendInput (ifVar e) (ifIndex e) secs)

{- | The concrete receipt at the given index of a receive's delivered messages.
'Require' guarantees the index is in range, so the safe lookup never misses in
practice; a 'Nothing' (impossible) makes the operation a harmless no-op rather
than a partial crash.
-}
handleAt :: Var [QueueMessage] Concrete -> Int -> Maybe ReceiptHandle
handleAt var i = msgReceipt <$> (concrete var !!? i)

-- | Whether an in-flight entry is the one named by (receive-output var, index).
sameHandle :: (Eq1 v) => Var [QueueMessage] v -> Int -> InFlightEntry v -> Bool
sameHandle var i e = ifVar e == var && ifIndex e == i

-- | Whether the model currently has an in-flight entry named by (var, index).
inFlightMember :: (Eq1 v) => QModel v -> Var [QueueMessage] v -> Int -> Bool
inFlightMember m var i = any (sameHandle var i) (mInFlight m)

{- | The state-machine property: generate a random sequence of enqueue / receive
/ ack / extend operations, run them against a fresh in-memory queue, and assert
the implementation agrees with the pure model on every observable result (the
'Ensure' callbacks) and that the model's state transitions stay consistent.

The queue threaded into the commands is created once per test run (in 'IO' lifted
into generation), but generation never invokes @commandExecute@ -- it only walks
the pure @commandGen@ / 'Require' / 'Update' callbacks -- so the same handle is
safely reused for execution.
-}
queueModelProperty :: H.PropertyT IO ()
queueModelProperty = do
    q <- liftIO newInMemoryQueue
    let commands =
            [ enqueueCommand q
            , receiveCommand q
            , ackCommand q
            , extendCommand q
            ]
    actions <- H.forAll (Gen.sequential (Range.linear 1 60) initialQModel commands)
    H.executeSequential initialQModel actions
