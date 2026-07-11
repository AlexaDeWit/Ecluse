module Ecluse.Rules.EffectfulSpec (spec) where

import Control.Retry (simulatePolicy)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, throwString)

import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Breaker (Breaker (..), BreakerReporter (..), noBreakerReporter)
import Ecluse.Core.Cve (CveQueryFault (CveQueryFault))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package
import Ecluse.Core.Rules (
    PreparedRule (..),
    evalRule,
    evalRules,
    inertRuleDeps,
    runEffectfulRule,
 )
import Ecluse.Core.Rules.Effectful (
    EffectfulConfig (..),
    Resilience (..),
    backoffPolicy,
    defaultEffectfulConfig,
    newBreaker,
 )

-- This spec builds 'PreparedRule's directly -- with a fake 'prepEval' and a chosen
-- 'prepName' -- to exercise the resilience harness and the parallel engine without any
-- evaluation closure on the closed 'Rule' data.
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (mkVersion)

{- | A fixed "now": the age-rule snapshot, and the base instant the injected breaker
clock starts from, so both age and cooldown arithmetic are deterministic.
-}
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

{- | An 'EvalContext' at a given instant: the request snapshot the age rules read. The
breaker no longer reads it -- its clock is injected separately (see 'newClock' and
'mkRuleClock'), so a cooldown test advances that clock rather than this context.
-}
ctxAt :: UTCTime -> EvalContext
ctxAt t = EvalContext t Nothing

ctx :: EvalContext
ctx = ctxAt now

-- | A typed stand-in for a direct rule breaking its no-effects contract.
newtype DirectRuleEscape = DirectRuleEscape Text
    deriving stock (Eq, Show)

instance Exception DirectRuleEscape

{- | An IORef-backed clock a breaker test advances by hand: the read action and a setter,
sharing one ref, so a test can simulate wall-clock time elapsing (during a retry run,
or across a cooldown) without sleeping. Mirrors the credential refresh spec's clock.
-}
newClock :: UTCTime -> IO (IO UTCTime, UTCTime -> IO ())
newClock start = do
    ref <- newIORef start
    pure (readIORef ref, writeIORef ref)

-- | A single inert artifact; the rules under test do not inspect artifacts.
sampleArtifact :: Artifact
sampleArtifact =
    Artifact
        { artFilename = "thing-1.0.0.tgz"
        , artUrl = "https://example.test/thing-1.0.0.tgz"
        , artKind = Tarball
        , artHashes = []
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | A package version under an optional npm scope, published @ageDays@ days before
'now'. The pure-rule signals (scope, age, install-code) are the axes evaluation gates
on; everything else is fixed.
-}
pkg :: Maybe Text -> Integer -> PackageDetails
pkg mScope ageDays =
    PackageDetails
        { pkgName = mkPackageName Npm (mkScope <$> mScope) "thing"
        , pkgVersion = mkVersion Npm "1.0.0"
        , pkgPublishedAt = Just (addUTCTime (negate (fromInteger ageDays * nominalDay)) now)
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = sampleArtifact :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        }

-- | A config with no retries, so a test never waits on a backoff.
fastConfig :: EffectfulConfig
fastConfig =
    defaultEffectfulConfig
        { ecBackoff = []
        , ecBreakerThreshold = 2
        , ecBreakerCooldown = 30
        }

{- | Build a resilient (effectful) prepared rule with a fresh breaker, the given
precedence, config, failure alignment, and (fake) evaluator, observed through the given
breaker reporter. The evaluator ignores the evaluation context (the rules under test
read only the package). This is the engine's injection point -- an arbitrary 'prepEval'
and a chosen 'prepName', without widening the closed 'Rule' vocabulary.
-}
mkRuleR ::
    BreakerReporter -> Text -> Int -> EffectfulConfig -> FailureAlignment -> (PackageDetails -> IO RuleVerdict) -> IO PreparedRule
mkRuleR = mkRuleClocked (pure now)

{- | As 'mkRuleR', but with an injected breaker clock: a rule whose 'resClock' is the
given action, so a cooldown test drives the breaker's timing directly (through
'newClock') rather than through the request context. The plain builders default the
clock to @'pure' 'now'@, so their breaker trips at 'now' and existing trip assertions
hold.
-}
mkRuleClocked ::
    IO UTCTime -> BreakerReporter -> Text -> Int -> EffectfulConfig -> FailureAlignment -> (PackageDetails -> IO RuleVerdict) -> IO PreparedRule
mkRuleClocked clock reporter name prec cfg align eval = do
    breaker <- newBreaker
    pure
        PreparedRule
            { prepName = name
            , prepPrecedence = prec
            , prepResilience = Just (Resilience cfg align breaker reporter clock)
            , prepEval = \_ pd -> eval pd
            }

-- | As 'mkRule', but with an injected breaker clock (through the inert default reporter).
mkRuleClock :: IO UTCTime -> Text -> Int -> EffectfulConfig -> FailureAlignment -> (PackageDetails -> IO RuleVerdict) -> IO PreparedRule
mkRuleClock clock = mkRuleClocked clock noBreakerReporter

-- | As 'mkRuleR', through the inert default reporter.
mkRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> (PackageDetails -> IO RuleVerdict) -> IO PreparedRule
mkRule = mkRuleR noBreakerReporter

-- | An effectful rule that always returns the given verdict (no IO failure).
constRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> RuleVerdict -> IO PreparedRule
constRule name prec cfg align outcome = mkRule name prec cfg align (\_ -> pure outcome)

-- | An effectful rule whose IO always throws (its source is down).
failingRule :: Text -> Int -> EffectfulConfig -> FailureAlignment -> IO PreparedRule
failingRule name prec cfg align = mkRule name prec cfg align (\_ -> throwString "source down")

-- | A built-in rule prepared (no resilience) at a precedence, evaluated via 'evalRule'.
pureAt :: Int -> Rule -> PreparedRule
pureAt prec rule =
    PreparedRule
        { prepName = ruleName rule
        , prepPrecedence = prec
        , prepResilience = Nothing
        , prepEval = \evalCtx -> evalRule inertRuleDeps evalCtx rule
        }

-- | A capturing breaker reporter appending each reported state to its log (oldest first).
capturingBreakerReporter :: IO (IORef [Breaker], BreakerReporter)
capturingBreakerReporter = do
    breakerLog <- newIORef []
    pure (breakerLog, BreakerReporter (\b -> modifyIORef' breakerLog (<> [b])))

-- | Mark the version as running code on install, so the install-script deny fires.
withInstallScripts :: PackageDetails -> PackageDetails
withInstallScripts pd = pd{pkgInstallCode = RunsCodeOnInstall "postinstall hook"}

admittedBy :: Decision -> Maybe Text
admittedBy (Admitted name _) = Just name
admittedBy _ = Nothing

blockedBy :: Decision -> Maybe Text
blockedBy (Blocked name _) = Just name
blockedBy _ = Nothing

isAdmitted :: Decision -> Bool
isAdmitted = isJust . admittedBy

isUndecidable :: Decision -> Bool
isUndecidable = \case
    Undecidable{} -> True
    _ -> False

isUnavailable :: RuleEvaluation -> Bool
isUnavailable = \case
    Unavailable{} -> True
    _ -> False

{- | An outcome for the equal-precedence tie tests: an allow, a deny, or a
deterministic fail-closed 'CannotVet' -- the three decisive positions that compete in
the boot order.
-}
genTieOutcome :: Gen RuleVerdict
genTieOutcome =
    Gen.element
        [ Allow "vetted clean"
        , Deny "known-bad version"
        , CannotVet FailDeny "no advisory database loaded"
        ]

spec :: Spec
spec = do
    describe "defaultEffectfulConfig -- the shipped resilience knobs" $
        it "pins the documented defaults (timeout, backoff schedule, breaker, no Retry-After)" $ do
            -- The shipped policy a caller inherits when it overrides only the eval:
            -- a 2s per-attempt timeout, two backoffs (100ms, 250ms), a breaker
            -- tripping after 5 failures and cooling for 30s, and no suggested delay.
            ecTimeout defaultEffectfulConfig `shouldBe` 2_000_000
            ecBackoff defaultEffectfulConfig `shouldBe` [100_000, 250_000]
            ecBreakerThreshold defaultEffectfulConfig `shouldBe` 5
            ecBreakerCooldown defaultEffectfulConfig `shouldBe` 30
            ecRetryAfter defaultEffectfulConfig `shouldBe` Nothing

    describe "backoffPolicy -- the compiled retry schedule" $ do
        -- 'simulatePolicy' walks the policy without sleeping, so the schedule the
        -- harness drives the retry loop with is asserted directly: the n-th retry
        -- waits the n-th 'ecBackoff' delay, and the policy stops (yields 'Nothing')
        -- once the list is exhausted -- its length being the retry budget.
        it "the default schedule retries twice, at 100ms then 250ms, then stops" $ do
            delays <- simulatePolicy 2 (backoffPolicy [100_000, 250_000])
            map snd delays `shouldBe` [Just 100_000, Just 250_000, Nothing]

        it "an empty schedule admits no retry (the single initial attempt only)" $ do
            delays <- simulatePolicy 0 (backoffPolicy [])
            map snd delays `shouldBe` [Nothing]

    describe "evalRules -- one engine over pure and effectful rules" $ do
        it "an effectful rule below a pure decisive prefix is never launched (short-circuit)" $ do
            -- A pure deny at precedence 300 decides first; an effectful rule ranked
            -- below it is mooted, so its IO must never run -- the counter (and its
            -- throw) prove it.
            ran <- newIORef (0 :: Int)
            effLater <- mkRule "EffAfter" 200 fastConfig FailDeny $ \_ -> do
                modifyIORef' ran (+ 1)
                throwString "should never run"
            decision <- evalRules ctx [effLater, pureAt 300 DenyInstallTimeExecution] (withInstallScripts (pkg Nothing 0))
            blockedBy decision `shouldBe` Just "DenyInstallTimeExecution"
            readIORef ran `shouldReturn` 0

        it "an effectful deny outranks a lower pure allow (boot order decides)" $ do
            rule <- constRule "EffDeny" 300 fastConfig FailDeny (Deny "known-bad version")
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            blockedBy decision `shouldBe` Just "EffDeny"

        it "a lower-ranked effectful rule never displaces a higher pure allow" $ do
            ran <- newIORef (0 :: Int)
            rule <- mkRule "EffDeny" 100 fastConfig FailDeny $ \_ -> do
                modifyIORef' ran (+ 1)
                pure (Deny "blocked")
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            admittedBy decision `shouldBe` Just "AllowScope"
            readIORef ran `shouldReturn` 0

        it "an effectful allow lifts a version the pure rules would deny by default" $ do
            rule <- constRule "EffAllow" 500 fastConfig FailNoDecision (Allow "remediates an advisory")
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg Nothing 0)
            admittedBy decision `shouldBe` Just "EffAllow"

    describe "evalRules -- deny-by-default with reasons in boot order" $
        it "collects every non-decisive reason, highest precedence first" $ do
            -- A losing fail-open unavailability (its source down), a NoDecision, and a
            -- pure NoDecision: none is decisive, so the package is BlockedByDefault
            -- carrying each reason in boot order (300, 200, 100) -- including the
            -- fail-open Unavailable's, so a fail-open loss is still surfaced.
            high <- failingRule "EffHigh" 300 fastConfig FailNoDecision
            mid <- constRule "EffMid" 200 fastConfig FailNoDecision (NoDecision "mid no opinion")
            decision <- evalRules ctx [mid, pureAt 100 (AllowScope (mkScope "myorg")), high] (pkg Nothing 0)
            case decision of
                BlockedByDefault reasons ->
                    reasons
                        `shouldBe` [ "EffHigh: the rule could not be evaluated"
                                   , "mid no opinion"
                                   , "scope is not the allow-listed @myorg"
                                   ]
                other -> expectationFailure ("expected BlockedByDefault, got " <> show other)

    describe "evalRules -- fail-closed vs fail-open alignment" $ do
        it "a failing FailDeny rule that could decide is Undecidable (fail-closed)" $ do
            rule <- failingRule "EffDeny" 300 fastConfig FailDeny
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            decision `shouldSatisfy` isUndecidable

        it "an Undecidable preserves a transient (WillResolve) cause with the configured Retry-After" $ do
            rule <- failingRule "EffDeny" 300 fastConfig{ecRetryAfter = Just (RetryAfter 15)} FailDeny
            decision <- evalRules ctx [rule] (pkg (Just "myorg") 0)
            case decision of
                Undecidable transience _ -> transience `shouldBe` WillResolve (Just (RetryAfter 15))
                other -> expectationFailure ("expected Undecidable, got " <> show other)

        it "a failing FailNoDecision rule is a no-op (fail-open), leaving a pure allow standing" $ do
            rule <- failingRule "EffAllow" 300 fastConfig FailNoDecision
            decision <- evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] (pkg (Just "myorg") 0)
            admittedBy decision `shouldBe` Just "AllowScope"

        it "a failing FailNoDecision rule never admits on its own" $ do
            -- Fail-open means 'does not fire', not 'admit': with no allow the version
            -- still falls back to deny-by-default, never admitted blind.
            rule <- failingRule "EffAllow" 300 fastConfig FailNoDecision
            decision <- evalRules ctx [rule] (pkg Nothing 0)
            isAdmitted decision `shouldBe` False

        it "a fail-closed undecidable is not admitted (no survivor)" $ do
            rule <- failingRule "EffDeny" 300 fastConfig FailDeny
            decision <- evalRules ctx [rule] (pkg Nothing 0)
            isAdmitted decision `shouldBe` False

    describe "evalRules -- the direct-rule never-throws absorption" $ do
        it "a throwing direct rule resolves fail-closed as Undecidable naming the rule" $ do
            -- A direct rule declares no effects, so a throw is an invariant break;
            -- the engine must absorb it (fail-closed, symmetric with the effectful
            -- harness's fail-deny Unavailable) rather than let one request's
            -- evaluation escape the serve path. The lower-precedence allow proves
            -- the absorption is decisive: it is never consulted.
            let bomb =
                    PreparedRule
                        { prepName = "DirectBomb"
                        , prepPrecedence = 300
                        , prepResilience = Nothing
                        , prepEval = \_ _ -> throwIO (DirectRuleEscape "the rule threw")
                        }
            decision <- evalRules ctx [bomb, pureAt 200 (AllowScope (mkScope "myorg"))] (pkg (Just "myorg") 0)
            case decision of
                Undecidable transience reason -> do
                    transience `shouldBe` WillResolve Nothing
                    reason `shouldSatisfy` \r -> "DirectBomb" `T.isPrefixOf` r
                other -> expectationFailure ("expected the fail-closed Undecidable, got " <> show other)

    describe "evalRules -- deterministic speculative parallelism" $ do
        it "credits the earliest-in-boot-order decisive rule, not the first to return" $ do
            -- The higher-precedence deny is slow; the lower-precedence allow returns
            -- first in wall-clock time. The decision must still be the deny (earliest
            -- in boot order), so the result never depends on evaluation timing.
            slowDeny <- mkRule "EffDeny" 300 fastConfig FailDeny (\_ -> threadDelay 40_000 >> pure (Deny "slow deny"))
            fastAllow <- constRule "EffAllow" 200 fastConfig FailNoDecision (Allow "fast allow")
            decision <- evalRules ctx [fastAllow, slowDeny] (pkg Nothing 0)
            blockedBy decision `shouldBe` Just "EffDeny"

        it "cancels a strictly-later evaluation once the winner is known" $ do
            -- The winner (precedence 300) decides immediately; a strictly-later rule
            -- (200) would take seconds and only then set its 'done' flag. The engine
            -- must cancel it the moment the winner is known, so 'done' stays False.
            done <- newIORef False
            winner <- constRule "EffWinner" 300 fastConfig FailDeny (Deny "blocked")
            laggard <- mkRule "EffLaggard" 200 fastConfig FailNoDecision $ \_ -> do
                threadDelay 10_000_000
                writeIORef done True
                pure (Allow "too late")
            decision <- evalRules ctx [laggard, winner] (pkg Nothing 0)
            blockedBy decision `shouldBe` Just "EffWinner"
            readIORef done `shouldReturn` False

    describe "evalRules -- order-independent boot order (carried from #377/#378)" $ do
        it "an equal-precedence effectful deny and unavailable resolve to the same decision regardless of order" $ do
            -- The sharpest case from the original bug: an effectful Deny (a permanent
            -- 403) and an effectful fail-closed Unavailable (a retryable 503) tie on
            -- precedence. The boot order settles it by name, so reversing the
            -- configured list cannot flip the decision.
            let mk =
                    sequence
                        [ constRule "EffDeny" 300 fastConfig FailDeny (Deny "known-bad version")
                        , failingRule "EffUnavail" 300 fastConfig FailDeny
                        ]
            forward <- mk >>= \rules -> evalRules ctx rules (pkg Nothing 0)
            backward <- mk >>= \rules -> evalRules ctx (reverse rules) (pkg Nothing 0)
            forward `shouldBe` backward
            forward `shouldSatisfy` (\d -> isJust (blockedBy d) || isUndecidable d)

        it "the decision is invariant under shuffling equal-precedence effectful rules" $
            hedgehog $ do
                -- The analogue of the pure tier's shuffle property, now over effectful
                -- rules: a list of equal-precedence rules (a mix of allow/deny/
                -- unavailable) with a distinct name each. Shuffling the configured set
                -- yields the same boot order, hence the same 'Decision'.
                outcomes <- forAll (Gen.list (Range.linear 2 6) genTieOutcome)
                let tagged = zip [0 :: Int ..] outcomes
                perm <- forAll (Gen.shuffle tagged)
                let build = traverse (\(i, o) -> constRule ("eff" <> show i) 300 fastConfig FailDeny o)
                    decide rs = build rs >>= \rules -> evalRules ctx rules (pkg Nothing 0)
                original <- liftIO (decide tagged)
                shuffled <- liftIO (decide perm)
                original === shuffled

    describe "runEffectfulRule -- the per-rule resilience wrapper" $ do
        it "runs a pure rule directly (no resilience)" $ do
            outcome <- runEffectfulRule ctx (pureAt 200 (AllowScope (mkScope "myorg"))) (pkg (Just "myorg") 0)
            outcome `shouldSatisfy` (\case Decided Allow{} -> True; _ -> False)

        it "times out a hanging rule and resolves per alignment (fail-closed)" $ do
            -- A 5ms timeout against a rule that sleeps far longer: the attempt is a
            -- failure, so a FailDeny rule yields Unavailable.
            rule <- mkRule "Slow" 1 fastConfig{ecTimeout = 5_000} FailDeny (\_ -> threadDelay 1_000_000 >> pure (Allow "late"))
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldSatisfy` isUnavailable

        it "retries a transiently failing rule and succeeds within the budget" $ do
            attempts <- newIORef (0 :: Int)
            rule <- mkRule "Flaky" 1 fastConfig{ecBackoff = [0]} FailDeny $ \_ -> do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
                if n < 2 then throwString "blip" else pure (Allow "recovered")
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldBe` Decided (Allow "recovered")
            readIORef attempts `shouldReturn` 2 -- the initial attempt plus one retry
        it "gives up after the retry budget is spent" $ do
            attempts <- newIORef (0 :: Int)
            rule <- mkRule "Down" 1 fastConfig{ecBackoff = [0, 0]} FailDeny $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwString "still down"
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldSatisfy` isUnavailable
            readIORef attempts `shouldReturn` 3 -- the initial attempt plus two retries
        it "trips the breaker after the threshold, then fast-fails without running the rule" $ do
            attempts <- newIORef (0 :: Int)
            rule <- mkRule "Down" 1 fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30} FailDeny $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwString "down"
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            readIORef attempts `shouldReturn` 2
            -- Breaker open now: the next evaluation fast-fails without an attempt, and
            -- its reason names the open breaker (and the rule), at the rule's alignment.
            fastFail <- runEffectfulRule ctx rule (pkg Nothing 0)
            fastFail `shouldBe` Unavailable (WillResolve Nothing) FailDeny "Down: the rule source circuit breaker is open"
            readIORef attempts `shouldReturn` 2

        it "absorbs the advisory handle's confined CveQueryFault: Unavailable, breaker advanced" $ do
            -- The advisory lookup's query fault is a confined typed exception whose
            -- one absorption boundary is THIS harness ('Ecluse.Core.Cve.CveQueryFault').
            -- It must resolve like any infrastructural fault -- the rule's aligned
            -- Unavailable -- and count towards the breaker, so a broken advisory
            -- database degrades to fast-fail rather than throwing through evalRules.
            attempts <- newIORef (0 :: Int)
            rule <- mkRule "DenyCve" 1 fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30} FailDeny $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwIO (CveQueryFault "advisories-for" "SQLite3 returned ErrorIO")
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldSatisfy` isUnavailable
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            -- Two committed faults reached the threshold: the breaker is open, so the
            -- next evaluation fast-fails without running the rule's IO again.
            fastFail <- runEffectfulRule ctx rule (pkg Nothing 0)
            fastFail `shouldBe` Unavailable (WillResolve Nothing) FailDeny "DenyCve: the rule source circuit breaker is open"
            readIORef attempts `shouldReturn` 2

        it "a deterministic CannotVet is taken at face value -- never retried, never trips the breaker" $ do
            -- The no-advisory-database verdict is deterministic and in-process, so the
            -- harness must take it at face value: no in-process retry could change it,
            -- and it must not count towards the breaker. A genuine fault (an exception,
            -- a timeout) still would; a returned verdict never does. Regressing this is
            -- a self-inflicted 503 outage before the first advisory sync lands.
            evals <- newIORef (0 :: Int)
            rule <- mkRule "DenyCve" 1 fastConfig{ecBackoff = [0, 0], ecBreakerThreshold = 2} FailDeny $ \_ -> do
                modifyIORef' evals (+ 1)
                pure (CannotVet FailDeny "no advisory database loaded")
            -- Run well past the breaker threshold. Were the verdict retried, each call
            -- would evaluate three times; were it counted as a failure, the breaker
            -- would open and later calls would fast-fail without evaluating.
            outcomes <- replicateM 4 (runEffectfulRule ctx rule (pkg Nothing 0))
            outcomes `shouldBe` replicate 4 (Decided (CannotVet FailDeny "no advisory database loaded"))
            readIORef evals `shouldReturn` 4 -- one evaluation per call: no retry, no fast-fail
        it "half-opens after the cooldown and recovers on a successful probe" $ do
            (clock, setClock) <- newClock now
            attempts <- newIORef (0 :: Int)
            failRef <- newIORef True
            rule <- mkRuleClock clock "Recover" 1 fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30} FailDeny $ \_ -> do
                modifyIORef' attempts (+ 1)
                bad <- readIORef failRef
                if bad then throwString "down" else pure (Deny "now reachable")
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            writeIORef failRef False
            setClock (addUTCTime 31 now)
            recovered <- runEffectfulRule ctx rule (pkg Nothing 0)
            recovered `shouldBe` Decided (Deny "now reachable")

        it "an exhausted FailNoDecision rule resolves to a fail-open Unavailable with a named reason" $ do
            rule <- failingRule "EffAllow" 1 fastConfig FailNoDecision
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldBe` Unavailable (WillResolve Nothing) FailNoDecision "EffAllow: the rule could not be evaluated"

        it "re-opens the breaker when the half-open probe also fails" $ do
            (clock, setClock) <- newClock now
            attempts <- newIORef (0 :: Int)
            rule <- mkRuleClock clock "Down" 1 fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30} FailDeny $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwString "still down"
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            -- Past the first cooldown (opened until now + 30): the next call half-opens.
            setClock (addUTCTime 31 now)
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            readIORef attempts `shouldReturn` 3 -- two trips plus the half-open probe
            -- The failed probe re-opened until now + 61; still inside that, so no attempt.
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            readIORef attempts `shouldReturn` 3 -- re-opened: no further attempt
        it "opens the cooldown from the failure-commit instant, not the attempt start (#705)" $ do
            -- The retry run consumes wall-clock time, so the injected clock advances during
            -- the attempt. A tripped breaker must open for its cooldown measured from when
            -- the failure commits; were the pre-retry instant reused, the elapsed retry time
            -- would be subtracted from the effective cooldown and half-open the breaker early.
            (clock, setClock) <- newClock now
            attempts <- newIORef (0 :: Int)
            rule <- mkRuleClock clock "Slow" 1 fastConfig{ecBreakerThreshold = 1, ecBreakerCooldown = 5} FailDeny $ \_ -> do
                modifyIORef' attempts (+ 1)
                setClock (addUTCTime 10 now) -- the retry run outlasts the 5s cooldown
                throwString "down"
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            readIORef attempts `shouldReturn` 1 -- tripped: opens until (now + 10) + 5 = now + 15
            -- now + 12 is past the buggy window (now + 5) but inside the real one (now + 15):
            -- the breaker must still fast-fail rather than admit a half-open probe.
            setClock (addUTCTime 12 now)
            fastFail <- runEffectfulRule ctx rule (pkg Nothing 0)
            fastFail `shouldBe` Unavailable (WillResolve Nothing) FailDeny "Slow: the rule source circuit breaker is open"
            readIORef attempts `shouldReturn` 1 -- still open: no probe attempted
        it "retries then succeeds under the shipped default config (real backoff)" $ do
            attempts <- newIORef (0 :: Int)
            rule <- mkRule "Flaky" 1 defaultEffectfulConfig FailDeny $ \_ -> do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
                if n < 2 then throwString "blip" else pure (Allow "recovered")
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldBe` Decided (Allow "recovered")
            readIORef attempts `shouldReturn` 2

        it "exhausts under the shipped default config (no suggested Retry-After)" $ do
            rule <- mkRule "Down" 1 defaultEffectfulConfig{ecBackoff = []} FailDeny (\_ -> throwString "down")
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldBe` Unavailable (WillResolve Nothing) FailDeny "Down: the rule could not be evaluated"

        it "reports the breaker trip → probe → reset transitions through its reporter" $ do
            (clock, setClock) <- newClock now
            (breakerLog, reporter) <- capturingBreakerReporter
            recovered <- newIORef False
            rule <- mkRuleClocked clock reporter "Down" 1 fastConfig{ecBreakerThreshold = 1, ecBreakerCooldown = 30} FailDeny $ \_ ->
                readIORef recovered >>= \case
                    False -> throwString "down"
                    True -> pure (Allow "recovered")
            _ <- runEffectfulRule ctx rule (pkg Nothing 0)
            readIORef breakerLog `shouldReturn` [Open (addUTCTime 30 now)]
            writeIORef recovered True
            setClock (addUTCTime 31 now)
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldBe` Decided (Allow "recovered")
            readIORef breakerLog `shouldReturn` [Open (addUTCTime 30 now), HalfOpen, Closed 0]

        it "records nothing through the default no-op reporter, still resolving fail-closed" $ do
            rule <- mkRule "Down" 1 fastConfig{ecBreakerThreshold = 1} FailDeny (\_ -> throwString "down")
            outcome <- runEffectfulRule ctx rule (pkg Nothing 0)
            outcome `shouldSatisfy` isUnavailable

    describe "properties" $ do
        it "a failing FailDeny rule that could decide is always fail-closed (Undecidable)" $
            hedgehog $ do
                effPrec <- forAll (Gen.int (Range.linear 201 1000)) -- strictly above the pure allow
                let p = pkg (Just "myorg") 0
                rule <- liftIO (failingRule "Eff" effPrec fastConfig FailDeny)
                decision <- liftIO (evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] p)
                H.assert (isUndecidable decision)

        it "a non-decisive effectful rule below a pure allow never changes the decision" $
            hedgehog $ do
                ageDays <- forAll (Gen.integral (Range.linear 0 3650))
                effPrec <- forAll (Gen.int (Range.linear 0 199)) -- strictly below 200
                outcome <-
                    forAll $
                        Gen.element
                            [NoDecision "x", CannotVet FailNoDecision "u"]
                let p = pkg (Just "myorg") ageDays
                rule <- liftIO (constRule "Eff" effPrec fastConfig FailNoDecision outcome)
                decision <- liftIO (evalRules ctx [pureAt 200 (AllowScope (mkScope "myorg")), rule] p)
                admittedBy decision === Just "AllowScope"
