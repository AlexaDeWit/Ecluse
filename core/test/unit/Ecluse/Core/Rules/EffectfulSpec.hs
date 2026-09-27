-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The resilience harness around the advisory read, and the engine over rules sharing that read.
module Ecluse.Core.Rules.EffectfulSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, nominalDay)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Breaker (Breaker (..), BreakerReporter (..), noBreakerReporter)
import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange), CveLookup (cveAdvisoriesFor), CveQueryFault (CveQueryFault))
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn, withSlotGeneration)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, Unbounded))
import Ecluse.Core.Package
import Ecluse.Core.Rules (
    AdvisoryDatabase (AdvisoryDatabase),
    PackageRead (..),
    PreparedRule (..),
    RuleDeps (rdAdvisoryDatabase, rdAdvisoryFreshness, rdSourceReporter),
    RuleEval (PerVersion),
    SourceHealth (..),
    SourceReporter (..),
    evalRules,
    newEvaluator,
    noSourceReporter,
    prepResilience,
    prepare,
    withCveLookup,
 )
import Ecluse.Core.Rules.Effectful (
    EffectfulConfig (..),
    ReadFault (..),
    Resilience (..),
    defaultEffectfulConfig,
    newBreaker,
    runResilient,
 )
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryFresh), AdvisoryPublication (PublishedAt), assessAdvisoryAge)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Cve (fakeCveDb, fakeCveLookup)
import Ecluse.Test.Rules (
    admittedBy,
    atDefaultPrecedence,
    blockedBy,
    evalRule,
    inertRuleDeps,
    isApproved,
    isBlockedByDefault,
    isUndecidable,
    mapPackageRead,
    mapResilience,
    packageRule,
    servingRuleDeps,
    withInstallScripts,
 )
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), newTestClock)

import Ecluse.Core.Rules.Types
import Ecluse.Rules.Support (ctx, now, pkg, sixDayLimit)

-- | A config with no retries, so a test never waits on a backoff.
fastConfig :: EffectfulConfig
fastConfig =
    defaultEffectfulConfig
        { ecBackoff = []
        , ecBreakerThreshold = 2
        , ecBreakerCooldown = 30
        }

-- | A fresh policy on the given breaker clock and observer.
resilienceWith :: IO UTCTime -> BreakerReporter -> EffectfulConfig -> IO Resilience
resilienceWith clock reporter cfg = do
    breaker <- newBreaker
    pure (Resilience cfg breaker reporter clock)

-- | A fresh policy at the fixed instant, observed by nothing.
resilience :: EffectfulConfig -> IO Resilience
resilience = resilienceWith (pure now) noBreakerReporter

-- | A resilient advisory rule whose read runs the effect, then gives every version the verdict.
mkRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> IO () -> RuleVerdict -> IO PreparedRule
mkRule name prec cfg align effect verdict = do
    res <- resilience cfg
    pure (packageRule name prec align (Just res) effect verdict)

-- | A resilient advisory rule whose read always succeeds.
constRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> RuleVerdict -> IO PreparedRule
constRule name prec cfg align = mkRule name prec cfg align pass

-- | A resilient advisory rule whose read always throws (its source is down).
failingRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> IO PreparedRule
failingRule name prec cfg align = mkRule name prec cfg align (throwIO TestSourceUnavailable) (NoDecision "unreached")

-- | A built-in rule at a precedence, decided per version.
pureAt :: Int -> Rule -> PreparedRule
pureAt prec rule =
    PreparedRule
        { prepName = ruleName rule
        , prepPrecedence = prec
        , prepEval = PerVersion (\evalCtx -> evalRule inertRuleDeps evalCtx rule)
        }

-- | A capturing breaker reporter appending each reported state to its log (oldest first).
capturingBreakerReporter :: IO (IORef [Breaker], BreakerReporter)
capturingBreakerReporter = do
    breakerLog <- newIORef []
    pure (breakerLog, BreakerReporter (\b -> modifyIORef' breakerLog (<> [b])))

-- | A source reporter collecting reports in the returned ref, newest first.
capturingSourceReporter :: IO (IORef [SourceHealth], SourceReporter)
capturingSourceReporter = do
    captured <- newIORef []
    pure (captured, noSourceReporter{reportSource = \h -> modifyIORef' captured (h :)})

{- | The three decisive outcomes that compete in an equal-precedence tie: an allow, a deny,
and a fail-closed 'CannotVet'.
-}
genTieOutcome :: Gen RuleVerdict
genTieOutcome =
    Gen.element
        [ Allow "vetted clean"
        , Deny Nothing "known-bad version"
        , CannotVet FailDeny "no advisory database loaded"
        ]

-- | Count the calls an action makes, returning the action and the counter.
counting :: IO a -> IO (IO a, IORef Int)
counting act = do
    calls <- newIORef 0
    pure (modifyIORef' calls (+ 1) *> act, calls)

-- | The fault the harness gives up with once its retries are spent.
spentFault :: Text -> ReadFault
spentFault = ReadFault (WillResolve Nothing) "the rule could not be evaluated"

-- | Whether a read was given up after it threw, with the throw in the detail and not the reason.
threwSpent :: Either ReadFault () -> Bool
threwSpent = \case
    Left fault -> fault{rfDetail = ""} == spentFault "" && "the rule threw: TestSourceUnavailable" `T.isPrefixOf` rfDetail fault
    Right () -> False

breakerOpenFault :: ReadFault
breakerOpenFault = ReadFault (WillResolve Nothing) "the rule source circuit breaker is open" "the rule source circuit breaker is open"

spec :: Spec
spec = do
    harnessSpec
    engineSpec
    provenanceSpec
    oncePerRequestSpec
    sharedReadSpec
    faultSpec
    heldThrowSpec
    breakerSpec
    generationSpec

harnessSpec :: Spec
harnessSpec = describe "runResilient, the harness around one read" $ do
    it "pins the documented defaults (timeout, backoff schedule, breaker, no Retry-After)" $ do
        ecTimeout defaultEffectfulConfig `shouldBe` 2_000_000
        ecBackoff defaultEffectfulConfig `shouldBe` [100_000, 250_000]
        ecBreakerThreshold defaultEffectfulConfig `shouldBe` 5
        ecBreakerCooldown defaultEffectfulConfig `shouldBe` 30
        ecRetryAfter defaultEffectfulConfig `shouldBe` Nothing

    it "times out a hanging read and gives it up as a retryable fault" $ do
        res <- resilience fastConfig{ecTimeout = 5_000}
        runResilient res (threadDelay 1_000_000) `shouldReturn` Left (spentFault "the attempt timed out")

    it "retries a transiently failing read and succeeds within the budget" $ do
        attempts <- newIORef (0 :: Int)
        res <- resilience fastConfig{ecBackoff = [0]}
        outcome <- runResilient res $ do
            n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
            if n < 2 then throwIO TestSourceUnavailable else pure ("recovered" :: Text)
        outcome `shouldBe` Right "recovered"
        readIORef attempts `shouldReturn` 2

    it "gives up after the retry budget is spent" $ do
        (down, attempts) <- counting (throwIO TestSourceUnavailable :: IO ())
        res <- resilience fastConfig{ecBackoff = [0, 0]}
        runResilient res down >>= (`shouldSatisfy` threwSpent)
        readIORef attempts `shouldReturn` 3

    it "trips the breaker after the threshold, then fast-fails without running the read" $ do
        (down, attempts) <- counting (throwIO TestSourceUnavailable :: IO ())
        res <- resilience fastConfig{ecBreakerThreshold = 2}
        replicateM_ 2 (runResilient res down)
        readIORef attempts `shouldReturn` 2
        runResilient res down `shouldReturn` Left breakerOpenFault
        readIORef attempts `shouldReturn` 2

    it "absorbs the advisory handle's confined CveQueryFault, keeping its detail for the operator" $ do
        (faulting, attempts) <- counting (throwIO (CveQueryFault "advisories-for" "SQLite3 returned ErrorIO") :: IO ())
        res <- resilience fastConfig{ecBreakerThreshold = 2}
        runResilient res faulting >>= \case
            Left fault -> rfDetail fault `shouldSatisfy` T.isInfixOf "SQLite3 returned ErrorIO"
            Right () -> expectationFailure "expected the read to be given up"
        void (runResilient res faulting)
        runResilient res faulting `shouldReturn` Left breakerOpenFault
        readIORef attempts `shouldReturn` 2

    it "carries the configured Retry-After on a fault" $ do
        res <- resilience fastConfig{ecRetryAfter = Just (RetryAfter 15)}
        runResilient res (throwIO TestSourceUnavailable :: IO ()) >>= \case
            Left fault -> rfTransience fault `shouldBe` WillResolve (Just (RetryAfter 15))
            Right () -> expectationFailure "expected the read to be given up"

    it "takes a returned value at face value: never retried, never trips the breaker" $ do
        -- An absent database is a returned value, so it must not trip the breaker before the first sync.
        (unloaded, readCount) <- counting (pure (Nothing :: Maybe ()))
        res <- resilience fastConfig{ecBackoff = [0, 0], ecBreakerThreshold = 2}
        replicateM 4 (runResilient res unloaded) `shouldReturn` replicate 4 (Right Nothing)
        readIORef readCount `shouldReturn` 4

    it "half-opens after the cooldown and recovers on a successful probe" $ do
        (clock, setClock) <- newTestClock now
        failing <- newIORef True
        res <- resilienceWith clock noBreakerReporter fastConfig{ecBreakerThreshold = 2}
        let flaky = readIORef failing >>= \bad -> if bad then throwIO TestSourceUnavailable else pure ("now reachable" :: Text)
        replicateM_ 2 (runResilient res flaky)
        writeIORef failing False
        setClock (addUTCTime 31 now)
        runResilient res flaky `shouldReturn` Right "now reachable"

    it "re-opens the breaker when the half-open probe also fails" $ do
        (clock, setClock) <- newTestClock now
        (down, attempts) <- counting (throwIO TestSourceUnavailable :: IO ())
        res <- resilienceWith clock noBreakerReporter fastConfig{ecBreakerThreshold = 2}
        replicateM_ 2 (runResilient res down)
        -- Past the first cooldown (opened until now + 30): the next call half-opens.
        setClock (addUTCTime 31 now)
        void (runResilient res down)
        readIORef attempts `shouldReturn` 3
        -- The failed probe re-opened until now + 61. This call is still inside that window.
        void (runResilient res down)
        readIORef attempts `shouldReturn` 3

    it "opens the cooldown from the failure-commit instant, not the attempt start" $ do
        -- The retry run consumes wall-clock time, so the breaker opens its cooldown from the
        -- failure-commit instant. The pre-retry instant would half-open it early.
        (clock, setClock) <- newTestClock now
        (slow, attempts) <- counting (setClock (addUTCTime 10 now) *> throwIO TestSourceUnavailable :: IO ())
        res <- resilienceWith clock noBreakerReporter fastConfig{ecBreakerThreshold = 1, ecBreakerCooldown = 5}
        void (runResilient res slow)
        -- now + 12 is past the pre-retry window (now + 5) but inside the real one (now + 15).
        setClock (addUTCTime 12 now)
        runResilient res slow `shouldReturn` Left breakerOpenFault
        readIORef attempts `shouldReturn` 1

    it "retries then succeeds under the shipped default config (real backoff)" $ do
        attempts <- newIORef (0 :: Int)
        res <- resilience defaultEffectfulConfig
        outcome <- runResilient res $ do
            n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
            if n < 2 then throwIO TestSourceUnavailable else pure ("recovered" :: Text)
        outcome `shouldBe` Right "recovered"
        readIORef attempts `shouldReturn` 2

    it "exhausts under the shipped default config (no suggested Retry-After)" $ do
        res <- resilience defaultEffectfulConfig{ecBackoff = []}
        runResilient res (throwIO TestSourceUnavailable :: IO ()) >>= (`shouldSatisfy` threwSpent)

    it "reports the breaker trip, probe, and reset transitions through its reporter" $ do
        (clock, setClock) <- newTestClock now
        (breakerLog, reporter) <- capturingBreakerReporter
        recovered <- newIORef False
        res <- resilienceWith clock reporter fastConfig{ecBreakerThreshold = 1}
        let probe = readIORef recovered >>= \ok -> if ok then pure () else throwIO TestSourceUnavailable
        void (runResilient res probe)
        readIORef breakerLog `shouldReturn` [Open (addUTCTime 30 now)]
        writeIORef recovered True
        setClock (addUTCTime 31 now)
        runResilient res probe `shouldReturn` Right ()
        readIORef breakerLog `shouldReturn` [Open (addUTCTime 30 now), HalfOpen, Closed 0]

engineSpec :: Spec
engineSpec = do
    describe "evalRules -- one engine over per-version and advisory rules" $ do
        it "never reads for a rule below a decisive per-version rule" $ do
            (down, readCount) <- counting (throwIO TestSourceUnavailable)
            effLater <- mkRule "EffAfter" 200 fastConfig FailDeny down (NoDecision "unreached")
            decision <- evalRules ctx [effLater, pureAt 300 DenyInstallTimeExecution] (withInstallScripts (pkg Nothing 0))
            blockedBy decision `shouldBe` Just "DenyInstallTimeExecution"
            readIORef readCount `shouldReturn` 0

        it "an effectful deny outranks a lower per-version allow (boot order decides)" $ do
            rule <- constRule "EffDeny" 300 fastConfig FailDeny (Deny Nothing "known-bad version")
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            blockedBy decision `shouldBe` Just "EffDeny"

        it "a lower-ranked effectful rule never displaces a higher per-version allow" $ do
            (reading, readCount) <- counting pass
            rule <- mkRule "EffDeny" 100 fastConfig FailDeny reading (Deny Nothing "blocked")
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            admittedBy decision `shouldBe` Just "AllowScope"
            readIORef readCount `shouldReturn` 0

        it "an effectful allow lifts a version the per-version rules would deny by default" $ do
            rule <- constRule "EffAllow" 500 fastConfig FailNoDecision (Allow "remediates an advisory")
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg Nothing 0)
            admittedBy decision `shouldBe` Just "EffAllow"

    describe "evalRules -- deny-by-default with reasons in boot order" $ do
        it "collects each rule's own reason from the shared read, highest precedence first" $ do
            high <- constRule "EffHigh" 300 fastConfig FailNoDecision (NoDecision "high no opinion")
            mid <- constRule "EffMid" 200 fastConfig FailNoDecision (NoDecision "mid no opinion")
            decision <- evalRules ctx [mid, pureAt 100 (AllowScope (mkScope "myorg")), high] (pkg Nothing 0)
            decision `shouldBe` BlockedByDefault ["high no opinion", "mid no opinion", "scope is not the allow-listed @myorg"]

        it "names each rule on a shared fault, so the trail says which checks could not run" $ do
            high <- failingRule "EffHigh" 300 fastConfig FailNoDecision
            mid <- constRule "EffMid" 200 fastConfig FailNoDecision (NoDecision "mid no opinion")
            decision <- evalRules ctx [mid, pureAt 100 (AllowScope (mkScope "myorg")), high] (pkg Nothing 0)
            decision
                `shouldBe` BlockedByDefault
                    [ "EffHigh: the rule could not be evaluated"
                    , "EffMid: the rule could not be evaluated"
                    , "scope is not the allow-listed @myorg"
                    ]

    describe "evalRules -- fail-closed vs fail-open alignment" $ do
        it "a failing FailDeny rule that could decide is Undecidable (fail-closed)" $ do
            rule <- failingRule "EffDeny" 300 fastConfig FailDeny
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            decision `shouldSatisfy` isUndecidable

        it "an Undecidable preserves a transient (WillResolve) cause with the configured Retry-After" $ do
            rule <- failingRule "EffDeny" 300 fastConfig{ecRetryAfter = Just (RetryAfter 15)} FailDeny
            evalRules ctx [rule] (pkg (Just "myorg") 0) >>= \case
                Undecidable transience _ -> transience `shouldBe` WillResolve (Just (RetryAfter 15))
                other -> expectationFailure ("expected Undecidable, got " <> show other)

        it "a failing FailNoDecision rule is a no-op (fail-open), leaving a per-version allow standing" $ do
            rule <- failingRule "EffAllow" 300 fastConfig FailNoDecision
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            admittedBy decision `shouldBe` Just "AllowScope"

        it "a failing FailNoDecision rule never admits on its own" $ do
            rule <- failingRule "EffAllow" 300 fastConfig FailNoDecision
            decision <- evalRules ctx [rule] (pkg Nothing 0)
            isApproved decision `shouldBe` False

        it "a fail-closed undecidable is not admitted (no survivor)" $ do
            rule <- failingRule "EffDeny" 300 fastConfig FailDeny
            decision <- evalRules ctx [rule] (pkg Nothing 0)
            isApproved decision `shouldBe` False

    describe "evalRules -- a per-version rule that throws refuses" $
        it "resolves fail-closed as Undecidable naming the rule" $ do
            let bomb =
                    PreparedRule
                        { prepName = "DirectBomb"
                        , prepPrecedence = 300
                        , prepEval = PerVersion (\_ _ -> throwIO (TestContractEscape "the rule threw"))
                        }
            evalRules ctx [bomb, pureAt 200 (AllowScope (mkScope "myorg"))] (pkg (Just "myorg") 0) >>= \case
                Undecidable transience reason -> do
                    transience `shouldBe` WillResolve Nothing
                    reason `shouldSatisfy` T.isPrefixOf "DirectBomb"
                other -> expectationFailure ("expected the fail-closed Undecidable, got " <> show other)

    describe "evalRules -- precedence, not timing, decides" $ do
        it "credits the earliest-in-boot-order decisive rule, not the fastest" $ do
            slowDeny <- mkRule "EffDeny" 300 fastConfig FailDeny (threadDelay 40_000) (Deny Nothing "slow deny")
            fastAllow <- constRule "EffAllow" 200 fastConfig FailNoDecision (Allow "fast allow")
            decision <- evalRules ctx [fastAllow, slowDeny] (pkg Nothing 0)
            blockedBy decision `shouldBe` Just "EffDeny"

        it "never runs a later rule's own read once an earlier one decides" $ do
            (lagging, readCount) <- counting pass
            winner <- constRule "EffWinner" 300 fastConfig FailDeny (Deny Nothing "blocked")
            laggard <- mkRule "EffLaggard" 200 fastConfig FailNoDecision lagging (Allow "too late")
            decision <- evalRules ctx [laggard, winner] (pkg Nothing 0)
            blockedBy decision `shouldBe` Just "EffWinner"
            readIORef readCount `shouldReturn` 0

    describe "evalRules -- order-independent boot order" $ do
        it "an equal-precedence effectful deny and unavailable resolve to the same decision regardless of order" $ do
            let mk =
                    sequence
                        [ constRule "EffDeny" 300 fastConfig FailDeny (Deny Nothing "known-bad version")
                        , failingRule "EffUnavail" 300 fastConfig FailDeny
                        ]
            forward <- mk >>= \rules -> evalRules ctx rules (pkg Nothing 0)
            backward <- mk >>= \rules -> evalRules ctx (reverse rules) (pkg Nothing 0)
            forward `shouldBe` backward
            forward `shouldSatisfy` (\d -> isJust (blockedBy d) || isUndecidable d)

        it "the decision is invariant under shuffling equal-precedence effectful rules" $
            hedgehog $ do
                outcomes <- forAll (Gen.list (Range.linear 2 6) genTieOutcome)
                let tagged = zip [0 :: Int ..] outcomes
                perm <- forAll (Gen.shuffle tagged)
                let build = traverse (\(i, o) -> constRule ("eff" <> show i) 300 fastConfig FailDeny o)
                    decide rs = build rs >>= \rules -> evalRules ctx rules (pkg Nothing 0)
                original <- liftIO (decide tagged)
                shuffled <- liftIO (decide perm)
                original === shuffled

    describe "properties" $ do
        it "a failing FailDeny rule that could decide is always fail-closed (Undecidable)" $
            hedgehog $ do
                effPrec <- forAll (Gen.int (Range.linear 201 1000))
                rule <- liftIO (failingRule "Eff" effPrec fastConfig FailDeny)
                decision <- liftIO (evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0))
                H.assert (isUndecidable decision)

        it "a non-decisive effectful rule below a per-version allow never changes the decision" $
            hedgehog $ do
                ageDays <- forAll (Gen.integral (Range.linear 0 3650))
                effPrec <- forAll (Gen.int (Range.linear 0 199))
                outcome <- forAll (Gen.element [NoDecision "x", CannotVet FailNoDecision "u"])
                rule <- liftIO (constRule "Eff" effPrec fastConfig FailNoDecision outcome)
                decision <- liftIO (evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") ageDays))
                admittedBy decision === Just "AllowScope"

data TestSourceUnavailable = TestSourceUnavailable
    deriving stock (Show)

instance Exception TestSourceUnavailable

provenanceSpec :: Spec
provenanceSpec = describe "advisory evidence" $
    it "retains the successful retry's acquired generation" $ do
        attempts <- newIORef (0 :: Int)
        let failed = advisoryRuleDeps "failed" "FAILED" (throwIO TestSourceUnavailable)
            retried = advisoryRuleDeps "retry" "RETRY" pass
            deps =
                inertRuleDeps
                    { rdAdvisoryDatabase = AdvisoryDatabase $ \use -> do
                        attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                        withCveLookup (if attempt == 0 then failed else retried) use
                    }
        rules <- withKnobs fastConfig{ecBackoff = [0]} <$> prepare deps [atDefaultPrecedence cveRule]
        evalRules ctx rules (pkg Nothing 0)
            `shouldReturn` Blocked "DenyIfCve" (Just (DbEtag "retry")) "affected by RETRY (CVSS >= 7.0)"
        readIORef attempts `shouldReturn` 2

advisoryRuleDeps :: Text -> Text -> IO () -> RuleDeps
advisoryRuleDeps etag identifier beforeQuery = servingRuleDeps (DbEtag etag) lookup'
  where
    original = fakeCveLookup [("thing", AdvisoryRange identifier (Just 9.8) (Just "0") Unbounded (Just 0.95))]
    lookup' = original{cveAdvisoriesFor = \name -> beforeQuery *> cveAdvisoriesFor original name}

cveRule :: Rule
cveRule = DenyIfCve (DenyIfCveParams 7.0 FailDeny)

-- | Version @v@ of the fixture package, published long enough ago for the quarantine to admit it.
thingAt :: Text -> RuleEvidence
thingAt v = (pkg Nothing 30){evVersion = mkVersion Npm v}

-- | One request's worth of versions of the fixture package.
requestVersions :: [RuleEvidence]
requestVersions = [thingAt ("1." <> show minor <> "." <> show patch) | minor <- [0 .. 7 :: Int], patch <- [0 .. 4 :: Int]]

-- | Rows that affect every version of the fixture package, above every threshold under test.
affectingEvery :: [(Text, AdvisoryRange)]
affectingEvery = [("thing", AdvisoryRange "GHSA-every-0001" (Just 9.8) Nothing Unbounded (Just 0.95))]

-- | Each advisory rule under its shipped or configured alignment, the set the per-request tests cover.
advisoryRules :: [Rule]
advisoryRules =
    [ AllowIfRemediatesCve
    , DenyIfCve (DenyIfCveParams 7.0 FailDeny)
    , DenyIfCve (DenyIfCveParams 7.0 FailNoDecision)
    , DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)
    , DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision)
    ]

-- | The label a per-rule case carries, with the alignment a deny is configured with.
ruleLabel :: Rule -> String
ruleLabel = \case
    DenyIfCve params -> "DenyIfCve " <> show (dicOnUnavailable params)
    DenyIfEpss params -> "DenyIfEpss " <> show (dieOnUnavailable params)
    other -> toString (ruleName other)

-- | The quarantine every per-request policy carries, which admits the fixture's versions.
quarantine :: Rule
quarantine = AllowIfOlderThan (7 * nominalDay)

-- | What one request did to the advisory source: push-age reads, generation pins, and row reads.
data SourceCalls = SourceCalls
    { scFreshness :: IORef Int
    , scPins :: IORef Int
    , scReads :: IORef Int
    }

-- | Rule capabilities over a fake generation of the given rows, counting each call the rules make.
countingDeps :: [(Text, AdvisoryRange)] -> IO (RuleDeps, SourceCalls)
countingDeps rows = do
    calls <- SourceCalls <$> newIORef 0 <*> newIORef 0 <*> newIORef 0
    let served = fakeCveLookup rows
        counted = served{cveAdvisoriesFor = \name -> modifyIORef' (scReads calls) (+ 1) *> cveAdvisoriesFor served name}
        deps =
            inertRuleDeps
                { rdAdvisoryDatabase = AdvisoryDatabase (\use -> modifyIORef' (scPins calls) (+ 1) *> use (Just (DbEtag "counted", counted)))
                , rdAdvisoryFreshness = AdvisoryFresh <$ modifyIORef' (scFreshness calls) (+ 1)
                }
    pure (deps, calls)

callCounts :: SourceCalls -> IO (Int, Int, Int)
callCounts calls = (,,) <$> readIORef (scFreshness calls) <*> readIORef (scPins calls) <*> readIORef (scReads calls)

-- | Decide every version of one request through one evaluator.
decideRequest :: [PreparedRule] -> [RuleEvidence] -> IO [Decision]
decideRequest rules versions = newEvaluator ctx rules >>= \decide -> traverse decide versions

-- | Set every prepared read's knobs and pin its breaker clock to 'now'.
withKnobs :: EffectfulConfig -> [PreparedRule] -> [PreparedRule]
withKnobs cfg = map (mapResilience (\res -> res{resConfig = cfg, resClock = pure now}))

-- | Prepare each rule on its own breaker clock, returning the rules and each clock's read count.
withClockCounts :: RuleDeps -> [Rule] -> IO ([PreparedRule], [IORef Int])
withClockCounts deps rules = fmap (\pairs -> (concatMap fst pairs, map snd pairs)) . forM rules $ \rule -> do
    clockReads <- newIORef 0
    prepared <- map (mapResilience (\res -> res{resClock = now <$ modifyIORef' clockReads (+ 1)})) <$> prepare deps [atDefaultPrecedence rule]
    pure (prepared, clockReads)

oncePerRequestSpec :: Spec
oncePerRequestSpec = describe "one advisory read per request" $ do
    for_ advisoryRules $ \rule ->
        it (ruleLabel rule <> " reads the push age, a generation, and the rows once for every version") $ do
            (deps, calls) <- countingDeps affectingEvery
            (rules, clocks) <- withClockCounts deps [rule]
            void (decideRequest rules requestVersions)
            callCounts calls `shouldReturn` (1, 1, 1)
            -- One breaker admission and one settle, each reading the breaker clock once.
            traverse readIORef clocks `shouldReturn` [2]

    it "reads again for the next request" $ do
        (deps, calls) <- countingDeps affectingEvery
        rules <- prepare deps [atDefaultPrecedence cveRule]
        replicateM_ 2 (decideRequest rules requestVersions)
        callCounts calls `shouldReturn` (2, 2, 2)

    it "applies the retry budget to the one read, then decides every version from it" $ do
        attempts <- newIORef (0 :: Int)
        let served = fakeCveLookup affectingEvery
            flaky = served{cveAdvisoriesFor = \name -> atomicModifyIORef' attempts (\n -> (n + 1, n)) >>= \n -> if n < 2 then throwIO TestSourceUnavailable else cveAdvisoriesFor served name}
        rules <- withKnobs fastConfig{ecBackoff = [0, 0]} <$> prepare (servingRuleDeps (DbEtag "flaky") flaky) [atDefaultPrecedence cveRule]
        decisions <- decideRequest rules requestVersions
        decisions `shouldSatisfy` all ((== Just "DenyIfCve") . blockedBy)
        readIORef attempts `shouldReturn` 3

    it "reads another package's rows rather than borrowing the first package's" $ do
        (deps, calls) <- countingDeps (affectingEvery <> [("other", AdvisoryRange "GHSA-other-0001" (Just 9.8) Nothing (FixedBefore "0.1.0") Nothing)])
        rules <- prepare deps [atDefaultPrecedence cveRule]
        decide <- newEvaluator ctx rules
        blockedBy <$> decide (thingAt "1.0.0") `shouldReturn` Just "DenyIfCve"
        decide ((thingAt "1.0.0"){evName = mkPackageName Npm Nothing "other"}) >>= (`shouldSatisfy` isBlockedByDefault)
        callCounts calls `shouldReturn` (2, 2, 2)

sharedReadSpec :: Spec
sharedReadSpec = describe "one read shared by every advisory rule" $ do
    it "makes one query per request with all three rules on, through the first-reached rule's breaker" $ do
        (deps, calls) <- countingDeps affectingEvery
        (rules, clocks) <- withClockCounts deps [DenyIfEpss (DenyIfEpssParams 0.99 FailDeny), AllowIfRemediatesCve, DenyIfCve (DenyIfCveParams 9.9 FailDeny)]
        decisions <- decideRequest (pureAt 100 quarantine : rules) requestVersions
        decisions `shouldSatisfy` all ((== Just "AllowIfOlderThan") . admittedBy)
        callCounts calls `shouldReturn` (1, 1, 1)
        -- DenyIfCve ties DenyIfEpss and sorts first, so it reads and only its breaker runs.
        traverse readIORef clocks `shouldReturn` [0, 0, 2]

    it "decides each rule from the shared rows under its own threshold" $ do
        (deps, _) <- countingDeps [("thing", AdvisoryRange "GHSA-mid-0001" (Just 6.5) Nothing Unbounded (Just 0.6))]
        rules <- prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailDeny), DenyIfEpss (DenyIfEpssParams 0.5 FailDeny), quarantine])
        decideRequest rules requestVersions
            >>= (`shouldSatisfy` all (== Blocked "DenyIfEpss" (Just (DbEtag "counted")) "affected by GHSA-mid-0001 (EPSS >= 0.5)"))

    it "has every rule reached report once per request, a shared fault with its detail" $ do
        (captured, reporter) <- capturingSourceReporter
        let deps = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\_ -> throwIO (TestContractEscape "advisory database exploded")), rdSourceReporter = reporter}
        rules <- withKnobs fastConfig <$> prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), quarantine])
        void (decideRequest rules requestVersions)
        readIORef captured >>= \reports -> case reverse reports of
            [SourceUnavailable "DenyIfCve" cveDetail, SourceUnavailable "DenyIfEpss" epssDetail] -> do
                cveDetail `shouldSatisfy` T.isInfixOf "advisory database exploded"
                epssDetail `shouldBe` cveDetail
            other -> expectationFailure ("expected one report per rule reached, got " <> show other)

    it "leaves a reusing rule's breaker untouched when the shared read fails" $ do
        let deps = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\_ -> throwIO TestSourceUnavailable)}
        rules <- withKnobs fastConfig{ecBreakerThreshold = 5} <$> prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), AllowIfRemediatesCve, quarantine])
        void (decideRequest rules requestVersions)
        traverse (readTVarIO . resBreaker) (mapMaybe prepResilience rules) `shouldReturn` [Closed 1, Closed 0, Closed 0]

    it "reads nothing with no database, and decides the policy as before the first sync" $ do
        freshness <- newIORef (0 :: Int)
        let policy = map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), AllowIfRemediatesCve, quarantine]
            counted = inertRuleDeps{rdAdvisoryFreshness = AdvisoryFresh <$ modifyIORef' freshness (+ 1)}
        rules <- prepare counted policy
        map (isJust . prepResilience) rules `shouldBe` replicate 4 False
        decisions <- decideRequest rules requestVersions
        readIORef freshness `shouldReturn` 1
        unloaded <- prepare inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\use -> use Nothing)} policy
        traverse (evalRules ctx unloaded) requestVersions `shouldReturn` decisions
        map skippedChecks decisions
            `shouldSatisfy` all
                ( ==
                    [ SkippedUnavailable "DenyIfCve" "no advisory database loaded"
                    , SkippedUnavailable "DenyIfEpss" "no advisory database loaded"
                    ]
                )

-- | How a read can fail to answer: it hangs, it throws, the breaker is open, or the push expired.
data Fault = Hangs | Throws | BreakerOpen | PushExpired
    deriving stock (Bounded, Enum, Show)

-- | A policy prepared so its advisory read meets the given fault, with the pins the read takes.
faultedPolicy :: Fault -> [Rule] -> IO ([PreparedRule], IORef Int)
faultedPolicy fault policy = do
    pins <- newIORef 0
    let serving = servingRuleDeps (DbEtag "faulted") (fakeCveLookup affectingEvery)
        deps = case fault of
            Hangs -> serving{rdAdvisoryDatabase = countedDatabase pins (\use -> threadDelay 1_000_000 *> withCveLookup serving use)}
            Throws -> serving{rdAdvisoryDatabase = countedDatabase pins (\_ -> throwIO TestSourceUnavailable)}
            BreakerOpen -> serving{rdAdvisoryDatabase = countedDatabase pins (withCveLookup serving)}
            PushExpired -> serving{rdAdvisoryDatabase = countedDatabase pins (withCveLookup serving), rdAdvisoryFreshness = pure expired}
        expired = assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate (9 * nominalDay)) now))
        knobs = withKnobs fastConfig{ecTimeout = 5_000, ecBackoff = [0, 0], ecBreakerThreshold = 5}
    rules <- knobs <$> prepare deps (map atDefaultPrecedence policy)
    opened <- case fault of
        BreakerOpen -> traverse openBreaker rules
        Hangs -> pure rules
        Throws -> pure rules
        PushExpired -> pure rules
    pure (opened, pins)

-- | A database whose every borrow counts one pin before running the given access.
countedDatabase :: IORef Int -> (forall a. (Maybe (DbEtag, CveLookup) -> IO a) -> IO a) -> AdvisoryDatabase
countedDatabase pins access = AdvisoryDatabase (\use -> modifyIORef' pins (+ 1) *> access use)

-- | The same rule with its breaker open until well after 'now'.
openBreaker :: PreparedRule -> IO PreparedRule
openBreaker rule = do
    tripped <- newTVarIO (Open (addUTCTime 30 now))
    pure (mapResilience (\res -> res{resBreaker = tripped}) rule)

-- | The alignment a rule resolves a fault to: fixed on an expired push, else as configured.
faultAlignment :: Fault -> Rule -> FailureAlignment
faultAlignment fault = \case
    AllowIfRemediatesCve -> FailNoDecision
    DenyIfCve params -> onRead (dicOnUnavailable params)
    DenyIfEpss params -> onRead (dieOnUnavailable params)
    _ -> FailDeny
  where
    onRead configured = case fault of
        PushExpired -> FailDeny
        Hangs -> configured
        Throws -> configured
        BreakerOpen -> configured

-- | The generation pins a fault leaves: every attempt for a read that runs, none for one that never starts.
faultPins :: Fault -> Int
faultPins = \case
    Hangs -> 3
    Throws -> 3
    BreakerOpen -> 0
    PushExpired -> 0

{- | The decision a policy of advisory rules above the quarantine reaches when the read faults: the
first fail-closed rule refuses, and otherwise the quarantine admits with every rule skipped.
-}
expectedUnder :: Fault -> [Rule] -> Decision -> Bool
expectedUnder fault rules decision = case find ((== FailDeny) . faultAlignment fault) rules of
    Just refusing -> case decision of
        Undecidable _ reason -> (ruleName refusing <> ": ") `T.isPrefixOf` reason
        _ -> False
    Nothing -> admittedBy decision == Just "AllowIfOlderThan" && map skippedName (skippedChecks decision) == map (Just . ruleName) rules
  where
    skippedName = \case
        SkippedUnavailable rule _ -> Just rule
        Unreached _ -> Nothing

faultSpec :: Spec
faultSpec = describe "a faulted advisory read resolves every version to each rule's own alignment" $
    for_ [minBound .. maxBound :: Fault] $ \fault -> do
        for_ advisoryRules $ \rule ->
            it (ruleLabel rule <> " alone when the read " <> show fault) $ do
                (rules, pins) <- faultedPolicy fault [rule, quarantine]
                decisions <- decideRequest rules requestVersions
                decisions `shouldSatisfy` all (expectedUnder fault [rule])
                readIORef pins `shouldReturn` faultPins fault
        for_ [(cve, epss) | cve <- [FailDeny, FailNoDecision], epss <- [FailDeny, FailNoDecision]] $ \(cve, epss) -> do
            let shared = [DenyIfCve (DenyIfCveParams 7.0 cve), DenyIfEpss (DenyIfEpssParams 0.5 epss), AllowIfRemediatesCve]
            it ("all three rules, DenyIfCve " <> show cve <> " and DenyIfEpss " <> show epss <> ", when the shared read " <> show fault) $ do
                (rules, pins) <- faultedPolicy fault (shared <> [quarantine])
                decisions <- decideRequest rules requestVersions
                decisions `shouldSatisfy` all (expectedUnder fault shared)
                readIORef pins `shouldReturn` faultPins fault

heldThrowSpec :: Spec
heldThrowSpec = describe "a throw outside the harness is held for the request" $ do
    it "refuses every version when the push-age reading throws, reading it once" $ do
        (throwing, readings) <- counting (throwIO (TestContractEscape "clock gone"))
        rule <- mapPackageRead (\packageRead -> packageRead{prFreshness = throwing}) <$> constRule "GateBomb" 300 fastConfig FailNoDecision (Allow "unreached")
        decisions <- decideRequest [rule, pureAt 200 (AllowScope (mkScope "myorg"))] requestVersions
        decisions `shouldSatisfy` all (\case Undecidable _ reason -> "GateBomb: the rule threw" `T.isPrefixOf` reason; _ -> False)
        readIORef readings `shouldReturn` 1

    it "refuses every version when the source report throws, reporting once" $ do
        (throwing, reports) <- counting (throwIO (TestContractEscape "reporter gone"))
        rule <- mapPackageRead (\packageRead -> packageRead{prReporter = noSourceReporter{reportSource = const throwing}}) <$> constRule "ReportBomb" 300 fastConfig FailNoDecision (Allow "unreached")
        decisions <- decideRequest [rule] requestVersions
        decisions `shouldSatisfy` all (\case Undecidable _ reason -> "ReportBomb: the rule threw" `T.isPrefixOf` reason; _ -> False)
        readIORef reports `shouldReturn` 1

breakerSpec :: Spec
breakerSpec = describe "the breaker counts requests" $ do
    it "counts a request of many versions as one failure" $ do
        rules <- faultingDeny
        void (decideRequest rules requestVersions)
        breakerOf rules `shouldReturn` Just (Closed 1)

    it "trips after five consecutive failed requests, then fast-fails the next without reading" $ do
        readCount <- newIORef (0 :: Int)
        rules <- withKnobs defaultEffectfulConfig{ecBackoff = []} <$> prepare (faultingDeps readCount) [atDefaultPrecedence cveRule]
        replicateM_ 4 (decideRequest rules requestVersions)
        breakerOf rules `shouldReturn` Just (Closed 4)
        void (decideRequest rules requestVersions)
        breakerOf rules `shouldReturn` Just (Open (addUTCTime 30 now))
        readIORef readCount `shouldReturn` 5
        decisions <- decideRequest rules requestVersions
        decisions `shouldSatisfy` all (== Undecidable (WillResolve Nothing) "DenyIfCve: the rule source circuit breaker is open")
        readIORef readCount `shouldReturn` 5
  where
    faultingDeps readCount = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\_ -> modifyIORef' readCount (+ 1) *> throwIO TestSourceUnavailable)}
    faultingDeny = newIORef (0 :: Int) >>= \readCount -> withKnobs defaultEffectfulConfig{ecBackoff = []} <$> prepare (faultingDeps readCount) [atDefaultPrecedence cveRule]
    breakerOf rules = traverse (readTVarIO . resBreaker) (listToMaybe (mapMaybe prepResilience rules))

generationSpec :: Spec
generationSpec = describe "one response reads one generation" $ do
    it "decides every version against the generation its first read pinned, across a swap" $ do
        slot <- newCveSlot
        swapIn slot (DbEtag "first") Nothing (fakeCveDb [("thing", AdvisoryRange "FIRST" (Just 9.8) Nothing Unbounded Nothing)])
        rules <- prepare inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (withSlotGeneration slot)} [atDefaultPrecedence cveRule]
        decide <- newEvaluator ctx rules
        let first' = Blocked "DenyIfCve" (Just (DbEtag "first")) "affected by FIRST (CVSS >= 7.0)"
        decide (thingAt "1.0.0") `shouldReturn` first'
        swapIn slot (DbEtag "second") Nothing (fakeCveDb [("thing", AdvisoryRange "SECOND" (Just 9.8) Nothing Unbounded Nothing)])
        traverse decide requestVersions >>= (`shouldBe` (first' <$ requestVersions))
        -- The next request pins the generation now serving.
        evalRules ctx rules (thingAt "1.0.0")
            `shouldReturn` Blocked "DenyIfCve" (Just (DbEtag "second")) "affected by SECOND (CVSS >= 7.0)"

    it "reads one generation across rules when a swap lands between them" $ do
        -- Generation a alone denies on EPSS, b alone on CVSS. CVSS from a with EPSS from b admits.
        slot <- newCveSlot
        swapIn slot (DbEtag "a") Nothing (fakeCveDb [("thing", AdvisoryRange "A-0001" (Just 6.5) Nothing Unbounded (Just 0.6))])
        swapped <- newIORef False
        let swapOnce _ =
                unlessM (atomicModifyIORef' swapped (True,)) $
                    swapIn slot (DbEtag "b") Nothing (fakeCveDb [("thing", AdvisoryRange "B-0001" (Just 7.5) Nothing Unbounded (Just 0.4))])
            deps = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (withSlotGeneration slot), rdSourceReporter = noSourceReporter{reportSource = swapOnce}}
        rules <- prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailDeny), DenyIfEpss (DenyIfEpssParams 0.5 FailDeny), quarantine])
        decideRequest rules requestVersions
            >>= (`shouldSatisfy` all (== Blocked "DenyIfEpss" (Just (DbEtag "a")) "affected by A-0001 (EPSS >= 0.5)"))
        readIORef swapped `shouldReturn` True
        evalRules ctx rules (thingAt "1.0.0")
            `shouldReturn` Blocked "DenyIfCve" (Just (DbEtag "b")) "affected by B-0001 (CVSS >= 7.0)"
