-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The resilience harness around the advisory read, and the engine over rules sharing that read.
module Ecluse.Core.Rules.EffectfulSpec (spec) where

import Control.Concurrent.STM (check)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, nominalDay)
import UnliftIO (cancel, mapConcurrently, timeout, wait, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Breaker (Breaker, BreakerPhase (..), BreakerReporter (..), admit, breakerPhase, initialBreaker, noBreakerReporter, recordFailure)
import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange), CveLookup (cveAdvisoriesFor), CveQueryFault (CveQueryFault))
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, Unbounded))
import Ecluse.Core.Package
import Ecluse.Core.Rules (
    AdvisoryDatabase,
    PackageRead (..),
    PreparedRule (..),
    RuleDeps (rdAdvisoryDatabase, rdAdvisoryFreshness, rdSourceReporter),
    RuleEval (PerVersion),
    SourceHealth (..),
    SourceReporter (..),
    bootOrder,
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
import Ecluse.Core.Rules.Render (renderInability, renderReason)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), meterSnapshot, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Admission.Weighted (AdmissionObservers (..), newWeightedAdmission, withWeightedAdmission)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Cve (fakeCveDb, fakeCveLookup)
import Ecluse.Test.Port (noopMetricsPort)
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
    remediation,
    revocation,
    sentences,
    servingRuleDeps,
    slotRuleDeps,
    staticAdvisoryDatabase,
    withAdvisoryFreshness,
    withInstallScripts,
 )
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), newTestClock)

import Ecluse.Core.Rules.Types
import Ecluse.Rules.Support (ctx, now, pkg, sixDayLimit)

fastConfig :: EffectfulConfig
fastConfig =
    defaultEffectfulConfig
        { ecBackoff = []
        , ecBreakerThreshold = 2
        , ecBreakerCooldown = 30
        }

resilienceWith :: IO UTCTime -> BreakerReporter -> EffectfulConfig -> IO Resilience
resilienceWith clock reporter cfg = do
    breaker <- newBreaker
    pure (Resilience cfg breaker reporter clock)

resilience :: EffectfulConfig -> IO Resilience
resilience = resilienceWith (pure now) noBreakerReporter

mkRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> IO () -> RuleVerdict -> IO PreparedRule
mkRule name prec cfg align effect verdict = do
    res <- resilience cfg
    pure (packageRule name prec align (Just res) effect verdict)

constRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> RuleVerdict -> IO PreparedRule
constRule name prec cfg align = mkRule name prec cfg align pass

failingRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> IO PreparedRule
failingRule name prec cfg align = mkRule name prec cfg align (throwIO TestSourceUnavailable) (NoDecision FixesNoAdvisory)

pureAt :: Int -> Rule -> PreparedRule
pureAt prec rule =
    PreparedRule
        { prepName = ruleName rule
        , prepPrecedence = prec
        , prepEval = PerVersion (\evalCtx -> evalRule inertRuleDeps evalCtx rule)
        }

capturingBreakerReporter :: IO (IORef [Breaker], BreakerReporter)
capturingBreakerReporter = do
    breakerLog <- newIORef []
    pure (breakerLog, BreakerReporter (\b -> modifyIORef' breakerLog (<> [b])))

capturingSourceReporter :: IO (IORef [SourceHealth], SourceReporter)
capturingSourceReporter = do
    captured <- newIORef []
    pure (captured, noSourceReporter{reportSource = \h -> modifyIORef' captured (h :)})

genTieOutcome :: Gen RuleVerdict
genTieOutcome =
    Gen.element
        [ Allow remediation
        , Deny Nothing revocation
        , CannotVet FailDeny NoDatabaseLoaded
        ]

counting :: IO a -> IO (IO a, IORef Int)
counting act = do
    calls <- newIORef 0
    pure (modifyIORef' calls (+ 1) *> act, calls)

spentFault :: Inability -> ReadFault
spentFault = ReadFault (WillResolve Nothing) EvaluationFailed

threwSpent :: Either ReadFault () -> Bool
threwSpent = \case
    Left fault ->
        (rfTransience fault, rfReason fault) == (WillResolve Nothing, EvaluationFailed)
            && "the rule threw: TestSourceUnavailable" `T.isPrefixOf` renderInability (rfDetail fault)
    Right () -> False

breakerOpenFault :: ReadFault
breakerOpenFault = ReadFault (WillResolve Nothing) SourceBreakerOpen SourceBreakerOpen

shouldResolve :: IO Decision -> (Decision, [Text]) -> Expectation
shouldResolve decided (decision, reasons) = do
    actual <- decided
    actual `shouldBe` decision
    sentences actual `shouldBe` reasons

shouldAllResolve :: [Decision] -> (Decision, [Text]) -> Expectation
shouldAllResolve decisions (decision, reasons) = do
    decisions `shouldSatisfy` all (== decision)
    map sentences decisions `shouldSatisfy` all (== reasons)

deniedBy :: Text -> Text -> AdvisoryScore -> Double -> Text -> Decision
deniedBy rule etag score threshold advisory = Blocked rule (Just (DbEtag etag)) (AffectedBy score threshold (mkAdvisoryIds (advisory :| [])))

spec :: Spec
spec = do
    harnessSpec
    recoverySpec
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
        outcome <- runResilient res (threadDelay 1_000_000)
        outcome `shouldBe` Left (spentFault AttemptTimedOut)
        first (renderInability . rfDetail) outcome `shouldBe` Left "the attempt timed out"

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
            Left fault -> renderInability (rfDetail fault) `shouldSatisfy` T.isInfixOf "SQLite3 returned ErrorIO"
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
        (map breakerPhase <$> readIORef breakerLog) `shouldReturn` [Open (addUTCTime 30 now)]
        writeIORef recovered True
        setClock (addUTCTime 31 now)
        runResilient res probe `shouldReturn` Right ()
        (map breakerPhase <$> readIORef breakerLog) `shouldReturn` [Open (addUTCTime 30 now), HalfOpen, Closed 0]

engineSpec :: Spec
engineSpec = do
    describe "evalRules -- one engine over per-version and advisory rules" $ do
        it "never reads for a rule below a decisive per-version rule" $ do
            (down, readCount) <- counting (throwIO TestSourceUnavailable)
            effLater <- mkRule "EffAfter" 200 fastConfig FailDeny down (NoDecision FixesNoAdvisory)
            decision <- evalRules ctx [effLater, pureAt 300 DenyInstallTimeExecution] (withInstallScripts (pkg Nothing 0))
            blockedBy decision `shouldBe` Just "DenyInstallTimeExecution"
            readIORef readCount `shouldReturn` 0

        it "an effectful deny outranks a lower per-version allow (boot order decides)" $ do
            rule <- constRule "EffDeny" 300 fastConfig FailDeny (Deny Nothing revocation)
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            blockedBy decision `shouldBe` Just "EffDeny"

        it "a lower-ranked effectful rule never displaces a higher per-version allow" $ do
            (reading, readCount) <- counting pass
            rule <- mkRule "EffDeny" 100 fastConfig FailDeny reading (Deny Nothing revocation)
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            admittedBy decision `shouldBe` Just "AllowScope"
            readIORef readCount `shouldReturn` 0

        it "an effectful allow lifts a version the per-version rules would deny by default" $ do
            rule <- constRule "EffAllow" 500 fastConfig FailNoDecision (Allow remediation)
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg Nothing 0)
            admittedBy decision `shouldBe` Just "EffAllow"

    describe "evalRules -- deny-by-default with reasons in boot order" $ do
        it "collects each rule's own reason from the shared read, highest precedence first" $ do
            high <- constRule "EffHigh" 300 fastConfig FailNoDecision (NoDecision (NotAffectedAtThreshold Cvss))
            mid <- constRule "EffMid" 200 fastConfig FailNoDecision (NoDecision (NotAffectedAtThreshold Epss))
            decision <- evalRules ctx [mid, pureAt 100 (AllowScope (mkScope "myorg")), high] (pkg Nothing 0)
            decision `shouldBe` BlockedByDefault [NotAffectedAtThreshold Cvss, NotAffectedAtThreshold Epss, ScopeNotAllowListed (mkScope "myorg")]
            sentences decision
                `shouldBe` [ "no advisory at or above the CVSS threshold affects this version"
                           , "no advisory at or above the EPSS threshold affects this version"
                           , "scope is not the allow-listed @myorg"
                           ]

        it "names each rule on a shared fault, so the trail says which checks could not run" $ do
            high <- failingRule "EffHigh" 300 fastConfig FailNoDecision
            mid <- constRule "EffMid" 200 fastConfig FailNoDecision (NoDecision FixesNoAdvisory)
            decision <- evalRules ctx [mid, pureAt 100 (AllowScope (mkScope "myorg")), high] (pkg Nothing 0)
            decision `shouldBe` BlockedByDefault [RuleUnable "EffHigh" EvaluationFailed, RuleUnable "EffMid" EvaluationFailed, ScopeNotAllowListed (mkScope "myorg")]
            sentences decision
                `shouldBe` [ "EffHigh: the rule could not be evaluated"
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
                decision@(Undecidable transience (RuleUnable rule (RuleThrew thrown))) -> do
                    transience `shouldBe` WillResolve Nothing
                    rule `shouldBe` "DirectBomb"
                    thrown `shouldSatisfy` T.isPrefixOf "TestContractEscape \"the rule threw\""
                    sentences decision `shouldSatisfy` all (T.isPrefixOf "DirectBomb: the rule threw: TestContractEscape \"the rule threw\"")
                other -> expectationFailure ("expected the fail-closed Undecidable, got " <> show other)

    describe "evalRules -- precedence, not timing, decides" $ do
        it "credits the earliest-in-boot-order decisive rule, not the fastest" $ do
            slowDeny <- mkRule "EffDeny" 300 fastConfig FailDeny (threadDelay 40_000) (Deny Nothing revocation)
            fastAllow <- constRule "EffAllow" 200 fastConfig FailNoDecision (Allow remediation)
            decision <- evalRules ctx [fastAllow, slowDeny] (pkg Nothing 0)
            blockedBy decision `shouldBe` Just "EffDeny"

        it "credits the earlier decisive advisory rule, and the later rule runs no read of its own" $ do
            (lagging, readCount) <- counting pass
            winner <- constRule "EffWinner" 300 fastConfig FailDeny (Deny Nothing revocation)
            laggard <- mkRule "EffLaggard" 200 fastConfig FailNoDecision lagging (Allow remediation)
            decision <- evalRules ctx [laggard, winner] (pkg Nothing 0)
            blockedBy decision `shouldBe` Just "EffWinner"
            readIORef readCount `shouldReturn` 0

    describe "evalRules -- order-independent boot order" $ do
        it "an equal-precedence effectful deny and unavailable resolve to the same decision regardless of order" $ do
            let mk =
                    sequence
                        [ constRule "EffDeny" 300 fastConfig FailDeny (Deny Nothing revocation)
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
                outcome <- forAll (Gen.element [NoDecision FixesNoAdvisory, CannotVet FailNoDecision NoDatabaseLoaded])
                rule <- liftIO (constRule "Eff" effPrec fastConfig FailNoDecision outcome)
                decision <- liftIO (evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") ageDays))
                admittedBy decision === Just "AllowScope"

data TestSourceUnavailable = TestSourceUnavailable
    deriving stock (Eq, Show)

instance Exception TestSourceUnavailable

provenanceSpec :: Spec
provenanceSpec = describe "advisory evidence" $
    it "retains the successful retry's acquired generation" $ do
        attempts <- newIORef (0 :: Int)
        let failed = advisoryRuleDeps "failed" "FAILED" (throwIO TestSourceUnavailable)
            retried = advisoryRuleDeps "retry" "RETRY" pass
            deps =
                inertRuleDeps
                    { rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) $ \use -> do
                        attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                        withCveLookup (if attempt == 0 then failed else retried) use
                    }
        rules <- withKnobs fastConfig{ecBackoff = [0]} <$> prepare deps [atDefaultPrecedence cveRule]
        evalRules ctx rules (pkg Nothing 0)
            `shouldResolve` (deniedBy "DenyIfCve" "retry" Cvss 7.0 "RETRY", ["affected by RETRY (CVSS >= 7.0)"])
        readIORef attempts `shouldReturn` 2

advisoryRuleDeps :: Text -> Text -> IO () -> RuleDeps
advisoryRuleDeps etag identifier beforeQuery = servingRuleDeps (DbEtag etag) lookup'
  where
    original = fakeCveLookup [("thing", AdvisoryRange identifier (Just 9.8) (Just "0") Unbounded (Just 0.95))]
    lookup' = original{cveAdvisoriesFor = \name -> beforeQuery *> cveAdvisoriesFor original name}

cveRule :: Rule
cveRule = DenyIfCve (DenyIfCveParams 7.0 FailDeny)

thingAt :: Text -> RuleEvidence
thingAt v = (pkg Nothing 30){evVersion = mkVersion Npm v}

requestVersions :: [RuleEvidence]
requestVersions = [thingAt ("1." <> show minor <> "." <> show patch) | minor <- [0 .. 7 :: Int], patch <- [0 .. 4 :: Int]]

affectingEvery :: [(Text, AdvisoryRange)]
affectingEvery = [("thing", AdvisoryRange "GHSA-every-0001" (Just 9.8) Nothing Unbounded (Just 0.95))]

advisoryRules :: [Rule]
advisoryRules =
    [ AllowIfRemediatesCve
    , DenyIfCve (DenyIfCveParams 7.0 FailDeny)
    , DenyIfCve (DenyIfCveParams 7.0 FailNoDecision)
    , DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)
    , DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision)
    ]

ruleLabel :: Rule -> String
ruleLabel = \case
    DenyIfCve params -> "DenyIfCve " <> show (dicOnUnavailable params)
    DenyIfEpss params -> "DenyIfEpss " <> show (dieOnUnavailable params)
    other -> toString (ruleName other)

quarantine :: Rule
quarantine = AllowIfOlderThan (7 * nominalDay)

data SourceCalls = SourceCalls
    { scFreshness :: IORef Int
    , scPins :: IORef Int
    , scReads :: IORef Int
    }

countingDeps :: [(Text, AdvisoryRange)] -> IO (RuleDeps, SourceCalls)
countingDeps rows = do
    calls <- SourceCalls <$> newIORef 0 <*> newIORef 0 <*> newIORef 0
    let served = fakeCveLookup rows
        counted = served{cveAdvisoriesFor = \name -> modifyIORef' (scReads calls) (+ 1) *> cveAdvisoriesFor served name}
        deps =
            inertRuleDeps
                { rdAdvisoryDatabase = staticAdvisoryDatabase (AdvisoryFresh <$ modifyIORef' (scFreshness calls) (+ 1)) (\use -> modifyIORef' (scPins calls) (+ 1) *> use (Just (DbEtag "counted", counted)))
                , rdAdvisoryFreshness = AdvisoryFresh <$ modifyIORef' (scFreshness calls) (+ 1)
                }
    pure (deps, calls)

callCounts :: SourceCalls -> IO (Int, Int, Int)
callCounts calls = (,,) <$> readIORef (scFreshness calls) <*> readIORef (scPins calls) <*> readIORef (scReads calls)

decideRequest :: [PreparedRule] -> [RuleEvidence] -> IO [Decision]
decideRequest rules versions = newEvaluator ctx rules >>= \decide -> traverse decide versions

withKnobs :: EffectfulConfig -> [PreparedRule] -> [PreparedRule]
withKnobs cfg = map (mapResilience (\res -> res{resConfig = cfg, resClock = pure now}))

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
            >>= (`shouldAllResolve` (deniedBy "DenyIfEpss" "counted" Epss 0.5 "GHSA-mid-0001", ["affected by GHSA-mid-0001 (EPSS >= 0.5)"]))

    it "has every rule reached report once per request, a shared fault with its detail" $ do
        (captured, reporter) <- capturingSourceReporter
        let deps = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\_ -> throwIO (TestContractEscape "advisory database exploded")), rdSourceReporter = reporter}
        rules <- withKnobs fastConfig <$> prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), quarantine])
        void (decideRequest rules requestVersions)
        readIORef captured >>= \reports -> case reverse reports of
            [SourceUnavailable "DenyIfCve" cveDetail, SourceUnavailable "DenyIfEpss" epssDetail] -> do
                renderInability cveDetail `shouldSatisfy` T.isInfixOf "advisory database exploded"
                epssDetail `shouldBe` cveDetail
            other -> expectationFailure ("expected one report per rule reached, got " <> show other)

    it "leaves a reusing rule's breaker untouched when the shared read fails" $ do
        let deps = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\_ -> throwIO TestSourceUnavailable)}
        rules <- withKnobs fastConfig{ecBreakerThreshold = 5} <$> prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), AllowIfRemediatesCve, quarantine])
        void (decideRequest rules requestVersions)
        traverse (fmap breakerPhase . readTVarIO . resBreaker) (mapMaybe prepResilience rules) `shouldReturn` [Closed 1, Closed 0, Closed 0]

    it "reads nothing with no database, and decides the policy as before the first sync" $ do
        freshness <- newIORef (0 :: Int)
        let policy = map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), AllowIfRemediatesCve, quarantine]
            counted = inertRuleDeps{rdAdvisoryFreshness = AdvisoryFresh <$ modifyIORef' freshness (+ 1)}
        rules <- prepare counted policy
        map (isJust . prepResilience) rules `shouldBe` replicate 4 False
        decisions <- decideRequest rules requestVersions
        readIORef freshness `shouldReturn` 1
        unloaded <- prepare inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> use Nothing)} policy
        traverse (evalRules ctx unloaded) requestVersions `shouldReturn` decisions
        map skippedChecks decisions
            `shouldSatisfy` all
                ( ==
                    [ SkippedUnavailable "DenyIfCve" NoDatabaseLoaded
                    , SkippedUnavailable "DenyIfEpss" NoDatabaseLoaded
                    ]
                )

data Fault = Hangs | Throws | BreakerOpen | PushExpired
    deriving stock (Bounded, Enum, Show)

faultedPolicy :: Fault -> [PrecededRule] -> IO ([PreparedRule], IORef Int)
faultedPolicy fault policy = do
    pins <- newIORef 0
    let serving = servingRuleDeps (DbEtag "faulted") (fakeCveLookup affectingEvery)
        deps = case fault of
            Hangs -> serving{rdAdvisoryDatabase = countedDatabase pins (\use -> threadDelay 1_000_000 *> withCveLookup serving use)}
            Throws -> serving{rdAdvisoryDatabase = countedDatabase pins (\_ -> throwIO TestSourceUnavailable)}
            BreakerOpen -> serving{rdAdvisoryDatabase = countedDatabase pins (withCveLookup serving)}
            PushExpired -> withAdvisoryFreshness (pure expired) serving{rdAdvisoryDatabase = countedDatabase pins (withCveLookup serving)}
        expired = assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate (9 * nominalDay)) now))
        knobs = withKnobs fastConfig{ecTimeout = 5_000, ecBackoff = [0, 0], ecBreakerThreshold = 5}
    rules <- knobs <$> prepare deps (policy <> [atDefaultPrecedence quarantine])
    opened <- case fault of
        BreakerOpen -> openReaderBreaker rules
        Hangs -> pure rules
        Throws -> pure rules
        PushExpired -> pure rules
    pure (opened, pins)

countedDatabase :: IORef Int -> (forall a. (Maybe (DbEtag, CveLookup) -> IO a) -> IO a) -> AdvisoryDatabase
countedDatabase pins access = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> modifyIORef' pins (+ 1) *> access use)

openReaderBreaker :: [PreparedRule] -> IO [PreparedRule]
openReaderBreaker rules = do
    tripped <-
        newTVarIO
            ( case admit now initialBreaker of
                (Just ticket, claimed) -> recordFailure 1 30 now ticket claimed
                (Nothing, unchanged) -> unchanged
            )
    let firstReader = prepName <$> find (isJust . prepResilience) (bootOrder rules)
        open rule
            | Just (prepName rule) == firstReader = mapResilience (\res -> res{resBreaker = tripped}) rule
            | otherwise = rule
    pure (map open rules)

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

faultPins :: Fault -> Int
faultPins = \case
    Hangs -> 3
    Throws -> 3
    BreakerOpen -> 0
    PushExpired -> 0

expectedUnder :: Fault -> [Rule] -> Decision -> Bool
expectedUnder fault rules decision = case find ((== FailDeny) . faultAlignment fault) rules of
    Just refusing -> case decision of
        Undecidable _ reason -> (ruleName refusing <> ": ") `T.isPrefixOf` renderReason reason
        _ -> False
    Nothing -> admittedBy decision == Just "AllowIfOlderThan" && map skippedName (skippedChecks decision) == map (Just . ruleName) rules
  where
    skippedName = \case
        SkippedUnavailable rule _ -> Just rule
        Unreached _ -> Nothing

faultPolicies :: [[PrecededRule]]
faultPolicies =
    map (pure . atDefaultPrecedence) advisoryRules
        <> [[cve a, epss b] | a <- alignments, b <- alignments]
        <> concat [[[deny, fastLaneLast], [fastLaneFirst, deny]] | deny <- map cve alignments <> map epss alignments]
        <> concat [[[cve a, epss b, fastLaneLast], [fastLaneFirst, cve a, epss b]] | a <- alignments, b <- alignments]
  where
    alignments = [FailDeny, FailNoDecision]
    cve a = atDefaultPrecedence (DenyIfCve (DenyIfCveParams 7.0 a))
    epss b = atDefaultPrecedence (DenyIfEpss (DenyIfEpssParams 0.5 b))
    fastLaneLast = atDefaultPrecedence AllowIfRemediatesCve
    fastLaneFirst = PrecededRule 300 AdmissionAndRevocation AllowIfRemediatesCve

policyLabel :: [PrecededRule] -> String
policyLabel = intercalate ", " . map (\(PrecededRule prec _ rule) -> ruleLabel rule <> " at " <> show prec)

faultSpec :: Spec
faultSpec = describe "a faulted advisory read resolves every version to each rule's own alignment" $
    for_ [minBound .. maxBound :: Fault] $ \fault ->
        for_ faultPolicies $ \policy ->
            it (policyLabel policy <> ", when the read " <> show fault) $ do
                (rules, pins) <- faultedPolicy fault policy
                decisions <- decideRequest rules requestVersions
                decisions `shouldSatisfy` all (expectedUnder fault (map prRule policy))
                readIORef pins `shouldReturn` faultPins fault

heldThrowSpec :: Spec
heldThrowSpec = describe "a throw outside the harness is held for the request" $ do
    it "refuses every version when the push-age reading throws, reading it once" $ do
        (throwing, readings) <- counting (throwIO (TestContractEscape "clock gone"))
        rule <- mapPackageRead (\packageRead -> packageRead{prRead = \_ use -> throwing >>= \freshness -> use freshness (pure Nothing)}) <$> constRule "GateBomb" 300 fastConfig FailNoDecision (Allow remediation)
        decisions <- decideRequest [rule, pureAt 200 (AllowScope (mkScope "myorg"))] requestVersions
        decisions `shouldSatisfy` all (\case Undecidable _ reason -> "GateBomb: the rule threw" `T.isPrefixOf` renderReason reason; _ -> False)
        readIORef readings `shouldReturn` 1

    it "refuses every version when the source report throws, reporting once" $ do
        (throwing, reports) <- counting (throwIO (TestContractEscape "reporter gone"))
        rule <- mapPackageRead (\packageRead -> packageRead{prReporter = noSourceReporter{reportSource = const throwing}}) <$> constRule "ReportBomb" 300 fastConfig FailNoDecision (Allow remediation)
        decisions <- decideRequest [rule] requestVersions
        decisions `shouldSatisfy` all (\case Undecidable _ reason -> "ReportBomb: the rule threw" `T.isPrefixOf` renderReason reason; _ -> False)
        readIORef reports `shouldReturn` 1

breakerSpec :: Spec
breakerSpec = describe "the breaker counts requests" $ do
    it "counts a request of many versions as one failure" $ do
        rules <- faultingDeny
        void (decideRequest rules requestVersions)
        (fmap breakerPhase <$> breakerOf rules) `shouldReturn` Just (Closed 1)

    it "trips after five consecutive failed requests, then fast-fails the next without reading" $ do
        readCount <- newIORef (0 :: Int)
        rules <- withKnobs defaultEffectfulConfig{ecBackoff = []} <$> prepare (faultingDeps readCount) [atDefaultPrecedence cveRule]
        replicateM_ 4 (decideRequest rules requestVersions)
        (fmap breakerPhase <$> breakerOf rules) `shouldReturn` Just (Closed 4)
        void (decideRequest rules requestVersions)
        (fmap breakerPhase <$> breakerOf rules) `shouldReturn` Just (Open (addUTCTime 30 now))
        readIORef readCount `shouldReturn` 5
        decisions <- decideRequest rules requestVersions
        decisions `shouldAllResolve` (Undecidable (WillResolve Nothing) (RuleUnable "DenyIfCve" SourceBreakerOpen), ["DenyIfCve: the rule source circuit breaker is open"])
        readIORef readCount `shouldReturn` 5
  where
    faultingDeps readCount = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\_ -> modifyIORef' readCount (+ 1) *> throwIO TestSourceUnavailable)}
    faultingDeny = newIORef (0 :: Int) >>= \readCount -> withKnobs defaultEffectfulConfig{ecBackoff = []} <$> prepare (faultingDeps readCount) [atDefaultPrecedence cveRule]
    breakerOf rules = traverse (readTVarIO . resBreaker) (listToMaybe (mapMaybe prepResilience rules))

generationSpec :: Spec
generationSpec = forM_ [False, True] $ \recovering -> describe ("one response reads one generation, recovery = " <> show recovering) $ do
    it "decides every version against the generation its first read pinned, across a swap" $ do
        slot <- newCveSlot
        swapIn slot (DbEtag "first") Nothing (fakeCveDb [("thing", AdvisoryRange "FIRST" (Just 9.8) Nothing Unbounded Nothing)])
        prepared <- prepare (slotRuleDeps slot) [atDefaultPrecedence cveRule]
        rules <- recoveryRules recovering prepared
        decide <- newEvaluator ctx rules
        let first' = deniedBy "DenyIfCve" "first" Cvss 7.0 "FIRST"
        decide (thingAt "1.0.0") `shouldResolve` (first', ["affected by FIRST (CVSS >= 7.0)"])
        swapIn slot (DbEtag "second") Nothing (fakeCveDb [("thing", AdvisoryRange "SECOND" (Just 9.8) Nothing Unbounded Nothing)])
        traverse decide requestVersions >>= (`shouldBe` (first' <$ requestVersions))
        evalRules ctx rules (thingAt "1.0.0")
            `shouldResolve` (deniedBy "DenyIfCve" "second" Cvss 7.0 "SECOND", ["affected by SECOND (CVSS >= 7.0)"])

    it "reads one generation across rules when a swap lands between them" $ do
        -- Generation a alone denies on EPSS, b alone on CVSS. CVSS from a with EPSS from b admits.
        slot <- newCveSlot
        swapIn slot (DbEtag "a") Nothing (fakeCveDb [("thing", AdvisoryRange "A-0001" (Just 6.5) Nothing Unbounded (Just 0.6))])
        swapped <- newIORef False
        let swapOnce _ =
                unlessM (atomicModifyIORef' swapped (True,)) $
                    swapIn slot (DbEtag "b") Nothing (fakeCveDb [("thing", AdvisoryRange "B-0001" (Just 7.5) Nothing Unbounded (Just 0.4))])
            deps = (slotRuleDeps slot){rdSourceReporter = noSourceReporter{reportSource = swapOnce}}
        prepared <- prepare deps (map atDefaultPrecedence [DenyIfCve (DenyIfCveParams 7.0 FailDeny), DenyIfEpss (DenyIfEpssParams 0.5 FailDeny), quarantine])
        rules <- recoveryRules recovering prepared
        decideRequest rules requestVersions
            >>= (`shouldAllResolve` (deniedBy "DenyIfEpss" "a" Epss 0.5 "A-0001", ["affected by A-0001 (EPSS >= 0.5)"]))
        readIORef swapped `shouldReturn` True
        evalRules ctx rules (thingAt "1.0.0")
            `shouldResolve` (deniedBy "DenyIfCve" "b" Cvss 7.0 "B-0001", ["affected by B-0001 (CVSS >= 7.0)"])

recoverySpec :: Spec
recoverySpec = describe "owned advisory recovery" $ do
    it "elects one recovery owner when concurrent callers cross the cooldown together" $ do
        (res, _) <- recoveryResilience []
        ready <- newTVarIO (0 :: Int)
        start <- newEmptyTMVarIO
        entered <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        completed <- newTVarIO []
        calls <- newTVarIO (0 :: Int)
        let probe = atomically (modifyTVar' calls (+ 1) >> putTMVar entered ()) >> atomically (readTMVar gate)
            request = do
                atomically (modifyTVar' ready (+ 1))
                atomically (readTMVar start)
                outcome <- runResilient res probe
                atomically (modifyTVar' completed (outcome :))
                pure outcome
        withAsync (mapConcurrently (const request) [1 .. 8 :: Int]) $ \workers -> do
            timeout 1_000_000 (atomically (readTVar ready >>= check . (== 8))) `shouldReturn` Just ()
            atomically (putTMVar start ())
            timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
            timeout 1_000_000 (atomically (readTVar completed >>= check . ((== 7) . length))) `shouldReturn` Just ()
            readTVarIO calls `shouldReturn` 1
            readTVarIO completed `shouldReturn` replicate 7 (Left breakerOpenFault)
            atomically (putTMVar gate ())
            wait workers >>= (`shouldMatchList` (Right () : replicate 7 (Left breakerOpenFault)))

    it "admits one blocked probe and refuses a concurrent burst before the winner completes" $ do
        (res, _) <- recoveryResilience []
        started <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        calls <- newIORef (0 :: Int)
        let probe = modifyIORef' calls (+ 1) >> atomically (putTMVar started ()) >> atomically (takeTMVar gate)
        withAsync (runResilient res probe) $ \winner -> do
            timeout 1_000_000 (atomically (takeTMVar started)) `shouldReturn` Just ()
            timeout 1_000_000 (mapConcurrently (const (runResilient res (pure ()))) [1 .. 8 :: Int])
                `shouldReturn` Just (replicate 8 (Left breakerOpenFault))
            readIORef calls `shouldReturn` 1
            atomically (putTMVar gate ())
            wait winner `shouldReturn` Right ()
        assertHealthyConcurrency res

    it "owns the complete retry run and counts its exhausted fault once" $ do
        (res, _) <- recoveryResilience [0, 0]
        entered <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        calls <- newIORef (0 :: Int)
        let probe = do
                n <- atomicModifyIORef' calls (\count -> (count + 1, count + 1))
                atomically (putTMVar entered n)
                atomically (takeTMVar gate)
                throwIO TestSourceUnavailable :: IO ()
        withAsync (runResilient res probe) $ \winner -> do
            forM_ [1 .. 3] $ \attempt -> do
                timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just attempt
                runResilient res (pure ()) `shouldReturn` Left breakerOpenFault
                atomically (putTMVar gate ())
            wait winner >>= (`shouldSatisfy` threwSpent)
        readIORef calls `shouldReturn` 3
        breakerPhase <$> readTVarIO (resBreaker res) `shouldReturn` Open (addUTCTime 60 now)

    forM_ [False, True] $ \afterFault ->
        it ("releases cancelled recovery without a failure, after retry fault = " <> show afterFault) $ do
            (res, _) <- recoveryResilience [0]
            entered <- newEmptyTMVarIO
            gate <- newEmptyTMVarIO
            calls <- newIORef (0 :: Int)
            let probe :: IO ()
                probe = do
                    n <- atomicModifyIORef' calls (\count -> (count + 1, count + 1))
                    if afterFault && n == 1
                        then throwIO TestSourceUnavailable
                        else atomically (putTMVar entered ()) >> atomically (takeTMVar gate)
            withAsync (runResilient res probe) $ \winner -> do
                timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
                cancel winner
                waitCatch winner >>= (`shouldSatisfy` isLeft)
            breakerPhase <$> readTVarIO (resBreaker res) `shouldReturn` RecoveryReady
            runResilient res (pure ()) `shouldReturn` Right ()
            breakerPhase <$> readTVarIO (resBreaker res) `shouldReturn` Closed 0

    it "releases cancellation at admission reporting before evaluation" $ do
        (res, _) <- recoveryResilience []
        reached <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        calls <- newIORef (0 :: Int)
        let reporter = BreakerReporter $ \breaker ->
                when (breakerPhase breaker == HalfOpen) $
                    atomically (putTMVar reached ()) >> atomically (takeTMVar gate)
        withAsync (runResilient res{resBreakerReporter = reporter} (modifyIORef' calls (+ 1))) $ \winner -> do
            timeout 1_000_000 (atomically (takeTMVar reached)) `shouldReturn` Just ()
            cancel winner
        readIORef calls `shouldReturn` 0
        breakerPhase <$> readTVarIO (resBreaker res) `shouldReturn` RecoveryReady
        runResilient res (pure ()) `shouldReturn` Right ()

    it "releases a throwing reporter without converting it to a source fault" $ do
        (res, _) <- recoveryResilience []
        let reporter = BreakerReporter (const (throwIO TestSourceUnavailable))
        runResilient res{resBreakerReporter = reporter} (pure ()) `shouldThrow` (== TestSourceUnavailable)
        breakerPhase <$> readTVarIO (resBreaker res) `shouldReturn` RecoveryReady
        runResilient res (pure ()) `shouldReturn` Right ()

    it "returns refused followers' CPU and memory capacity while recovery remains blocked" $ do
        (res, _) <- recoveryResilience []
        meter <- newMemoryMeter MeterSettings{msBudgetBytes = 20, msStepBytes = 10, msEntryRoom = 0, msEntryWaitMicros = 0}
        cpu <- newWeightedAdmission 2 0 0
        inFlight <- newIORef (0 :: Int)
        let observers = AdmissionObservers pass pass (\delta -> modifyIORef' inFlight (+ delta))
            admitted action = withMemoryEntry noopMetricsPort meter (const (withWeightedAdmission observers cpu 1 action))
        entered <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        withAsync (admitted (runResilient res (atomically (putTMVar entered ()) >> atomically (takeTMVar gate)))) $ \winner -> do
            timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
            replicateM_ 8 $ admitted (runResilient res (pure ())) `shouldReturn` Just (Just (Left breakerOpenFault))
            snChargedBytes <$> meterSnapshot meter `shouldReturn` 10
            readIORef inFlight `shouldReturn` 1
            admitted (pure ()) `shouldReturn` Just (Just ())
            cancel winner
        snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
        readIORef inFlight `shouldReturn` 0
        admitted (runResilient res (pure ())) `shouldReturn` Just (Just (Right ()))

    it "reuses one recovering read across versions and keeps each rule's refusal alignment" $ do
        entered <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        readCount <- newIORef (0 :: Int)
        let query = modifyIORef' readCount (+ 1) >> atomically (putTMVar entered ()) >> atomically (takeTMVar gate)
            deps = advisoryRuleDeps "recovering" "RECOVERED" query
            policy = [DenyIfCve (DenyIfCveParams 7.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)]
        prepared <- prepare deps (map atDefaultPrecedence policy)
        rules <- recoveryRules True prepared
        case mapMaybe prepResilience rules of
            readerRes : later -> do
                forM_ later $ \res -> atomically (writeTVar (resBreaker res) initialBreaker)
                withAsync (decideRequest rules requestVersions) $ \winner -> do
                    timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
                    timeout 1_000_000 (decideRequest rules requestVersions) >>= \case
                        Just decisions -> do
                            decisions
                                `shouldSatisfy` all
                                    ( \case
                                        Undecidable _ (RuleUnable "DenyIfEpss" SourceBreakerOpen) -> True
                                        _ -> False
                                    )
                        Nothing -> expectationFailure "followers waited behind recovery"
                    readIORef readCount `shouldReturn` 1
                    atomically (putTMVar gate ())
                    decisions <- wait winner
                    decisions `shouldSatisfy` all ((== Just "DenyIfCve") . blockedBy)
                readIORef readCount `shouldReturn` 1
                breakerPhase <$> readTVarIO (resBreaker readerRes) `shouldReturn` Closed 0
                forM_ later $ \res -> breakerPhase <$> readTVarIO (resBreaker res) `shouldReturn` Closed 0
            [] -> expectationFailure "expected prepared advisory breakers"

    it "emits no source failure report when an engine recovery read is cancelled" $ do
        entered <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        armed <- newIORef True
        (reports, reporter) <- capturingSourceReporter
        let query = whenM (readIORef armed) (atomically (putTMVar entered ()) >> atomically (takeTMVar gate))
            deps = (advisoryRuleDeps "cancelled" "RESUMED" query){rdSourceReporter = reporter}
        prepared <- prepare deps [atDefaultPrecedence cveRule]
        rules <- recoveryRules True prepared
        withAsync (decideRequest rules requestVersions) $ \winner -> do
            timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
            cancel winner
        readIORef reports `shouldReturn` []
        writeIORef armed False
        decisions <- decideRequest rules requestVersions
        decisions `shouldSatisfy` all ((== Just "DenyIfCve") . blockedBy)
        readIORef reports `shouldReturn` [SourceAnswered "DenyIfCve"]

    it "leaves healthy reads concurrent before any failure" $ do
        res <- resilience fastConfig
        assertHealthyConcurrency res

    it "does not claim another prepared rule or another mount's recovery" $ do
        (clock, setClock) <- newTestClock now
        let deps = advisoryRuleDeps "isolation" "ISOLATED" pass
        firstMount <- prepare deps (map atDefaultPrecedence [cveRule, DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)])
        secondMount <- prepare deps [atDefaultPrecedence cveRule]
        let policies = mapMaybe prepResilience (firstMount <> secondMount)
        forM_ policies $ \policy -> atomically (writeTVar (resBreaker policy) (trippedRuleBreaker now))
        setClock (addUTCTime 30 now)
        let configured = map (\policy -> policy{resClock = clock, resConfig = fastConfig}) policies
        case configured of
            owner : others -> do
                entered <- newEmptyTMVarIO
                gate <- newEmptyTMVarIO
                withAsync (runResilient owner (atomically (putTMVar entered ()) >> atomically (takeTMVar gate))) $ \winner -> do
                    timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
                    forM_ others $ \policy -> runResilient policy (pure ()) `shouldReturn` Right ()
                    atomically (putTMVar gate ())
                    wait winner `shouldReturn` Right ()
            [] -> expectationFailure "expected per-rule prepared breakers"

recoveryResilience :: [Int] -> IO (Resilience, IO UTCTime)
recoveryResilience backoff = do
    (clock, setClock) <- newTestClock now
    res <- resilienceWith clock noBreakerReporter fastConfig{ecBreakerThreshold = 1, ecBackoff = backoff}
    void (runResilient res (throwIO TestSourceUnavailable :: IO ()))
    setClock (addUTCTime 30 now)
    pure (res, clock)

trippedRuleBreaker :: UTCTime -> Breaker
trippedRuleBreaker clock = case admit clock initialBreaker of
    (Just ticket, claimed) -> recordFailure 1 30 clock ticket claimed
    (Nothing, unchanged) -> unchanged

assertHealthyConcurrency :: Resilience -> Expectation
assertHealthyConcurrency res = do
    entered <- newTVarIO (0 :: Int)
    gate <- newEmptyTMVarIO
    let readBody = atomically (modifyTVar' entered (+ 1)) >> atomically (readTMVar gate)
    withAsync (mapConcurrently (const (runResilient res readBody)) [1 .. 4 :: Int]) $ \workers -> do
        timeout 1_000_000 (atomically (readTVar entered >>= check . (== 4))) `shouldReturn` Just ()
        atomically (putTMVar gate ())
        wait workers `shouldReturn` replicate 4 (Right ())

recoveryRules :: Bool -> [PreparedRule] -> IO [PreparedRule]
recoveryRules False rules = pure rules
recoveryRules True rules = do
    forM_ (mapMaybe prepResilience rules) $ \res -> atomically (writeTVar (resBreaker res) (trippedRuleBreaker now))
    pure (map (mapResilience (\res -> res{resClock = pure (addUTCTime 30 now)})) rules)
