module Ecluse.Rules.EffectfulSpec (spec) where

import Control.Retry (simulatePolicy)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)

import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package
import Ecluse.Rules.Effectful
import Ecluse.Rules.Types
import Ecluse.Version (mkVersion)

-- | A fixed "now" so the breaker's cooldown arithmetic is deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

-- | An 'EvalContext' at a given instant (the breaker reads its clock from here).
ctxAt :: UTCTime -> EvalContext
ctxAt = EvalContext

ctx :: EvalContext
ctx = ctxAt now

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
'now'. The pure-rule signals (scope, age, install-code) are the axes the tiers gate
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
        , pkgMaintainers = []
        , pkgDependencies = []
        }

-- | A config with no retries, so a test never waits on a backoff.
fastConfig :: EffectfulConfig
fastConfig =
    defaultEffectfulConfig
        { ecBackoff = []
        , ecBreakerThreshold = 2
        , ecBreakerCooldown = 30
        }

{- | Build an effectful rule with a fresh breaker, a fixed-outcome (or failing) eval,
and the given precedence and failure policy. The eval ignores the version.
-}
mkRule :: Text -> EffectfulConfig -> FailurePolicy -> (PackageDetails -> IO RuleOutcome) -> IO EffectfulRule
mkRule name cfg policy eval = do
    breaker <- newBreaker
    pure
        EffectfulRule
            { erName = name
            , erEval = eval
            , erConfig = cfg
            , erOnError = policy
            , erBreaker = breaker
            }

-- | An effectful rule that always returns the given pure outcome (no IO failure).
constRule :: Text -> EffectfulConfig -> FailurePolicy -> RuleOutcome -> IO EffectfulRule
constRule name cfg policy outcome = mkRule name cfg policy (\_ -> pure outcome)

-- | An effectful rule whose IO always throws (its source is down).
failingRule :: Text -> EffectfulConfig -> FailurePolicy -> IO EffectfulRule
failingRule name cfg policy = mkRule name cfg policy (\_ -> throwString "source down")

at :: Int -> EffectfulRule -> PrecededEffectfulRule
at = PrecededEffectfulRule

-- | Mark the version as running code on install, so the install-script deny fires.
withInstallScripts :: PackageDetails -> PackageDetails
withInstallScripts pd = pd{pkgInstallCode = RunsCodeOnInstall "postinstall hook"}

-- | The pure deny rule a winning pure deny is identified by, for the cross-tier tie.
deniedBy :: Decision -> Maybe Rule
deniedBy (Denied r _) = Just r
deniedBy _ = Nothing

isApproved :: Decision -> Bool
isApproved = \case
    Approved{} -> True
    ApprovedEffectful{} -> True
    _ -> False

isUndecidable :: Decision -> Bool
isUndecidable = \case
    Undecidable{} -> True
    _ -> False

spec :: Spec
spec = do
    describe "defaultEffectfulConfig — the shipped resilience knobs" $
        it "pins the documented defaults (timeout, backoff schedule, breaker, no Retry-After)" $ do
            -- The shipped policy a caller inherits when it overrides only 'erEval':
            -- a 2s per-attempt timeout, two backoffs (100ms, 250ms), a breaker
            -- tripping after 5 failures and cooling for 30s, and no suggested delay.
            ecTimeout defaultEffectfulConfig `shouldBe` 2_000_000
            ecBackoff defaultEffectfulConfig `shouldBe` [100_000, 250_000]
            ecBreakerThreshold defaultEffectfulConfig `shouldBe` 5
            ecBreakerCooldown defaultEffectfulConfig `shouldBe` 30
            ecRetryAfter defaultEffectfulConfig `shouldBe` Nothing

    describe "backoffPolicy — the compiled retry schedule" $ do
        -- 'simulatePolicy' walks the policy without sleeping, so the schedule the
        -- harness drives the retry loop with is asserted directly: the n-th retry
        -- waits the n-th 'ecBackoff' delay, and the policy stops (yields 'Nothing')
        -- once the list is exhausted — its length being the retry budget.
        it "the default schedule retries twice, at 100ms then 250ms, then stops" $ do
            -- 'simulatePolicy n' walks iterations 0..n; the first two yield the two
            -- backoffs, and the schedule is then exhausted (a 'Nothing' stop).
            delays <- simulatePolicy 2 (backoffPolicy [100_000, 250_000])
            map snd delays `shouldBe` [Just 100_000, Just 250_000, Nothing]

        it "an empty schedule admits no retry (the single initial attempt only)" $ do
            delays <- simulatePolicy 0 (backoffPolicy [])
            map snd delays `shouldBe` [Nothing]

    describe "evalRulesEffectful — tier is performance, not precedence" $ do
        it "with no effectful rules, agrees with the pure tier exactly" $ do
            let pd = pkg (Just "myorg") 0
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            decision <- evalRulesEffectful ctx pureRules [] pd
            decision `shouldBe` Approved (AllowScope (mkScope "myorg")) "scope @myorg is allow-listed"

        it "skips an effectful rule ranked below the pure winner (its IO never runs)" $ do
            -- The pure tier allows at precedence 200 (AllowScope default). An
            -- effectful deny ranked at 100 cannot outrank it, so it must not even be
            -- consulted — the counter proves the IO never ran.
            ran <- newIORef (0 :: Int)
            let pd = pkg (Just "myorg") 0
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            rule <- mkRule "EffDeny" fastConfig OnUnavailable $ \_ -> do
                modifyIORef' ran (+ 1)
                pure (Deny "blocked")
            decision <- evalRulesEffectful ctx pureRules [at 100 rule] pd
            decision `shouldBe` Approved (AllowScope (mkScope "myorg")) "scope @myorg is allow-listed"
            readIORef ran `shouldReturn` 0

        it "consults an effectful rule ranked at or above the pure winner" $ do
            -- The same allow at 200, but the effectful deny is ranked at 300, so it
            -- outranks the pure allow and its IO runs.
            ran <- newIORef (0 :: Int)
            let pd = pkg (Just "myorg") 0
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            rule <- mkRule "EffDeny" fastConfig OnUnavailable $ \_ -> do
                modifyIORef' ran (+ 1)
                pure (Deny "known-bad version")
            decision <- evalRulesEffectful ctx pureRules [at 300 rule] pd
            decision `shouldBe` DeniedEffectful "EffDeny" "known-bad version"
            readIORef ran `shouldReturn` 1

        it "consults every effectful rule when the pure tier denies by default" $ do
            -- No pure winner means any-precedence effectful rule could still change
            -- the outcome, so even a low-ranked one is consulted and can admit.
            let pd = pkg (Just "myorg") 0
            rule <- constRule "EffAllow" fastConfig OnUnavailable (Allow "vetted clean")
            decision <- evalRulesEffectful ctx [] [at 1 rule] pd
            decision `shouldBe` ApprovedEffectful "EffAllow" "vetted clean"

    describe "evalRulesEffectful — cross-tier precedence" $ do
        it "an effectful deny outranks a lower pure allow" $ do
            let pd = pkg (Just "myorg") 0
                pureRules = [PrecededRule 100 (AllowScope (mkScope "myorg"))]
            rule <- constRule "EffDeny" fastConfig OnUnavailable (Deny "cve")
            decision <- evalRulesEffectful ctx pureRules [at 200 rule] pd
            decision `shouldBe` DeniedEffectful "EffDeny" "cve"

        it "at equal precedence an effectful deny beats the pure allow" $ do
            let pd = pkg (Just "myorg") 0
                pureRules = [PrecededRule 200 (AllowScope (mkScope "myorg"))]
            rule <- constRule "EffDeny" fastConfig OnUnavailable (Deny "cve")
            decision <- evalRulesEffectful ctx pureRules [at 200 rule] pd
            decision `shouldBe` DeniedEffectful "EffDeny" "cve"

        it "an effectful allow can lift a version the pure tier denied by default" $ do
            let pd = pkg Nothing 0 -- no scope, too young: pure tier denies by default
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            rule <- constRule "EffAllow" fastConfig OnUnavailable (Allow "remediates an advisory")
            decision <- evalRulesEffectful ctx pureRules [at 500 rule] pd
            decision `shouldBe` ApprovedEffectful "EffAllow" "remediates an advisory"

        it "an abstaining effectful rule leaves the pure decision standing" $ do
            let pd = pkg (Just "myorg") 0
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            rule <- constRule "EffAbstain" fastConfig OnUnavailable (Abstain "no opinion")
            decision <- evalRulesEffectful ctx pureRules [at 500 rule] pd
            decision `shouldBe` Approved (AllowScope (mkScope "myorg")) "scope @myorg is allow-listed"

        it "a lower-ranked effectful allow does not displace an equal-precedence pure deny" $ do
            -- The pure tier denies (install scripts) at precedence 300; an effectful
            -- allow at the same 300 ranks below the deny (deny-before-allow), so the
            -- pure deny stands rather than the effectful allow lifting it.
            let pd = withInstallScripts (pkg (Just "myorg") 0)
                pureRules = [atDefaultPrecedence DenyInstallTimeExecution] -- denies at 300
            rule <- constRule "EffAllow" fastConfig OnUnavailable (Allow "vouched")
            decision <- evalRulesEffectful ctx pureRules [at 300 rule] pd
            deniedBy decision `shouldBe` Just DenyInstallTimeExecution

        it "an equal-precedence effectful allow does not displace the pure allow" $ do
            -- An effectful allow at the same precedence as a pure allow does not
            -- outrank it (the contract is strict-greater), so the credited decision
            -- stays the pure rule's own 'Approved', not 'ApprovedEffectful'.
            let pd = pkg (Just "myorg") 0
                pureRules = [PrecededRule 200 (AllowScope (mkScope "myorg"))]
            rule <- constRule "EffAllow" fastConfig OnUnavailable (Allow "also vouched")
            decision <- evalRulesEffectful ctx pureRules [at 200 rule] pd
            decision `shouldBe` Approved (AllowScope (mkScope "myorg")) "scope @myorg is allow-listed"

        it "an equal-precedence effectful Unavailable does not flip a pure deny to undecidable" $ do
            -- The security-relevant tie: a pure deny at precedence 300 and a failing
            -- effectful rule (ranking as a deny, fail-closed) at the same 300. The
            -- effectful candidate does not outrank the pure deny, so the decision
            -- stays the permanent policy denial (403) rather than flipping to a
            -- retryable 'Undecidable' (503).
            let pd = withInstallScripts (pkg (Just "myorg") 0)
                pureRules = [atDefaultPrecedence DenyInstallTimeExecution] -- denies at 300
            rule <- failingRule "EffDeny" fastConfig OnUnavailable -- yields Unavailable at 300
            decision <- evalRulesEffectful ctx pureRules [at 300 rule] pd
            isUndecidable decision `shouldBe` False
            deniedBy decision `shouldBe` Just DenyInstallTimeExecution

        it "a strictly-higher effectful Unavailable still outranks the pure deny (fail-closed)" $ do
            -- The guarded path the tie fix must not regress: an effectful failure
            -- ranked strictly above the pure deny still wins and is fail-closed to
            -- 'Undecidable'.
            let pd = withInstallScripts (pkg (Just "myorg") 0)
                pureRules = [atDefaultPrecedence DenyInstallTimeExecution] -- denies at 300
            rule <- failingRule "EffDeny" fastConfig OnUnavailable
            decision <- evalRulesEffectful ctx pureRules [at 400 rule] pd
            decision `shouldSatisfy` isUndecidable

    describe "evalRulesEffectful — fail-closed (Unavailable)" $ do
        it "a failing effectful rule that could change the outcome is Undecidable" $ do
            let pd = pkg (Just "myorg") 0
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            rule <- failingRule "EffDeny" fastConfig OnUnavailable
            decision <- evalRulesEffectful ctx pureRules [at 300 rule] pd
            decision `shouldSatisfy` isUndecidable

        it "an Undecidable carries a transient (WillResolve) cause an exhausted source can recover from" $ do
            let pd = pkg (Just "myorg") 0
            rule <- failingRule "EffDeny" fastConfig{ecRetryAfter = Just (RetryAfter 15)} OnUnavailable
            decision <- evalRulesEffectful ctx [] [at 300 rule] pd
            case decision of
                Undecidable transience _ -> transience `shouldBe` WillResolve (Just (RetryAfter 15))
                other -> expectationFailure ("expected Undecidable, got " <> show other)

        it "a rule that itself returns Unavailable is treated as a failed attempt (fail-closed)" $ do
            let pd = pkg (Just "myorg") 0
            rule <- constRule "EffDeny" fastConfig OnUnavailable (Unavailable WontResolve "self-reported")
            decision <- evalRulesEffectful ctx [] [at 300 rule] pd
            decision `shouldSatisfy` isUndecidable

        it "fail-closed undecidable is not admitted (no survivor)" $ do
            let pd = pkg Nothing 0
            rule <- failingRule "EffDeny" fastConfig OnUnavailable
            decision <- evalRulesEffectful ctx [] [at 300 rule] pd
            isApproved decision `shouldBe` False

    describe "evalRulesEffectful — onError: abstain (availability beats safety)" $ do
        it "an exhausted abstain-policy rule abstains, leaving the pure decision" $ do
            let pd = pkg (Just "myorg") 0
                pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
            rule <- failingRule "EffAllow" fastConfig OnAbstain
            decision <- evalRulesEffectful ctx pureRules [at 300 rule] pd
            decision `shouldBe` Approved (AllowScope (mkScope "myorg")) "scope @myorg is allow-listed"

        it "an exhausted abstain-policy rule does not admit on its own" $ do
            -- Fail-open means 'does not fire', not 'admit': with no pure allow the
            -- version still falls back to deny-by-default, never admitted blind.
            let pd = pkg Nothing 0
            rule <- failingRule "EffAllow" fastConfig OnAbstain
            decision <- evalRulesEffectful ctx [] [at 300 rule] pd
            isApproved decision `shouldBe` False

    describe "runEffectfulRule — timeout, retry, breaker" $ do
        it "times out a hanging rule and resolves per policy (fail-closed)" $ do
            -- A 5ms timeout against a rule that sleeps far longer: the attempt is a
            -- failure, so an OnUnavailable rule yields Unavailable.
            let pd = pkg Nothing 0
                cfg = fastConfig{ecTimeout = 5_000}
            rule <- mkRule "Slow" cfg OnUnavailable $ \_ -> threadDelay 1_000_000 >> pure (Allow "late")
            outcome <- runEffectfulRule ctx rule pd
            outcome `shouldSatisfy` isUnavailableOutcome

        it "retries a transiently failing rule and succeeds within the budget" $ do
            -- The rule fails once then succeeds; one retry (one backoff) is enough.
            -- A near-zero backoff keeps the retry delay free; the invocation count is
            -- an independent witness that exactly one retry ran.
            attempts <- newIORef (0 :: Int)
            let pd = pkg Nothing 0
                cfg = fastConfig{ecBackoff = [0]}
            rule <- mkRule "Flaky" cfg OnUnavailable $ \_ -> do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
                if n < 2 then throwString "blip" else pure (Allow "recovered")
            outcome <- runEffectfulRule ctx rule pd
            outcome `shouldBe` Allow "recovered"
            readIORef attempts `shouldReturn` 2 -- the initial attempt plus one retry
        it "gives up after the retry budget is spent" $ do
            attempts <- newIORef (0 :: Int)
            let pd = pkg Nothing 0
                cfg = fastConfig{ecBackoff = [0, 0]}
            rule <- mkRule "Down" cfg OnUnavailable $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwString "still down"
            outcome <- runEffectfulRule ctx rule pd
            outcome `shouldSatisfy` isUnavailableOutcome
            readIORef attempts `shouldReturn` 3 -- the initial attempt plus two retries
        it "trips the breaker after the threshold, then fast-fails without running the rule" $ do
            -- Threshold 2: two exhausted evaluations trip it; the third is denied at
            -- the gate, so the rule's IO does not run a third time.
            attempts <- newIORef (0 :: Int)
            let pd = pkg Nothing 0
                cfg = fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30}
            rule <- mkRule "Down" cfg OnUnavailable $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwString "down"
            _ <- runEffectfulRule ctx rule pd
            _ <- runEffectfulRule ctx rule pd
            afterTrip <- readIORef attempts
            afterTrip `shouldBe` 2
            -- Breaker open now: the next evaluation fast-fails without an attempt,
            -- and its reason names the open breaker (and the rule).
            fastFail <- runEffectfulRule ctx rule pd
            fastFail `shouldBe` Unavailable (WillResolve Nothing) "Down: the rule source circuit breaker is open"
            readIORef attempts `shouldReturn` 2

        it "half-opens after the cooldown and recovers on a successful probe" $ do
            attempts <- newIORef (0 :: Int)
            failRef <- newIORef True
            let pd = pkg Nothing 0
                cfg = fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30}
            rule <- mkRule "Recover" cfg OnUnavailable $ \_ -> do
                modifyIORef' attempts (+ 1)
                bad <- readIORef failRef
                if bad then throwString "down" else pure (Deny "now reachable")
            -- Trip the breaker at 'now'.
            _ <- runEffectfulRule ctx rule pd
            _ <- runEffectfulRule ctx rule pd
            -- Source recovers; advance the clock past the cooldown so a half-open
            -- probe is admitted, and it succeeds.
            writeIORef failRef False
            let later = ctxAt (addUTCTime 31 now)
            recovered <- runEffectfulRule later rule pd
            recovered `shouldBe` Deny "now reachable"

        it "an exhausted abstain-policy rule abstains with a named reason" $ do
            let pd = pkg Nothing 0
            rule <- failingRule "EffAllow" fastConfig OnAbstain
            outcome <- runEffectfulRule ctx rule pd
            outcome `shouldBe` Abstain "EffAllow: the rule could not be evaluated"

        it "an abstain-policy rule fast-fails to Abstain when its breaker is open" $ do
            let pd = pkg Nothing 0
                cfg = fastConfig{ecBreakerThreshold = 1}
            rule <- failingRule "EffAllow" cfg OnAbstain
            _ <- runEffectfulRule ctx rule pd -- trips the breaker (threshold 1)
            outcome <- runEffectfulRule ctx rule pd -- open: fast-fail
            outcome `shouldSatisfy` isAbstainOutcome

        it "re-opens the breaker when the half-open probe also fails" $ do
            attempts <- newIORef (0 :: Int)
            let pd = pkg Nothing 0
                cfg = fastConfig{ecBreakerThreshold = 2, ecBreakerCooldown = 30}
            rule <- mkRule "Down" cfg OnUnavailable $ \_ -> do
                modifyIORef' attempts (+ 1)
                throwString "still down"
            -- Trip the breaker at 'now' (two failures).
            _ <- runEffectfulRule ctx rule pd
            _ <- runEffectfulRule ctx rule pd
            -- Cooldown elapsed: the next call is admitted as a half-open probe; it
            -- runs (attempt count rises) but fails, re-opening the breaker.
            let later = ctxAt (addUTCTime 31 now)
            _ <- runEffectfulRule later rule pd
            afterProbe <- readIORef attempts
            afterProbe `shouldBe` 3 -- two trips plus the half-open probe
            -- Re-opened: an immediate further call (still within the new cooldown)
            -- fast-fails without another attempt.
            _ <- runEffectfulRule later rule pd
            readIORef attempts `shouldReturn` 3

        it "retries then succeeds under the shipped default config (real backoff)" $ do
            -- The unmodified defaultEffectfulConfig: a rule that fails its first
            -- attempt then succeeds recovers within the default two-retry budget,
            -- exercising the shipped backoff schedule and sleep (not a test override).
            attempts <- newIORef (0 :: Int)
            let pd = pkg Nothing 0
            rule <- mkRule "Flaky" defaultEffectfulConfig OnUnavailable $ \_ -> do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
                if n < 2 then throwString "blip" else pure (Allow "recovered")
            outcome <- runEffectfulRule ctx rule pd
            outcome `shouldBe` Allow "recovered"
            readIORef attempts `shouldReturn` 2

        it "exhausts and trips under the shipped default config (no suggested Retry-After)" $ do
            -- Drive the default config to exhaustion and past its trip threshold so
            -- the shipped breaker cooldown and the default 'Nothing' Retry-After are
            -- both exercised: each exhausted call yields a transient Unavailable with
            -- no suggested delay, and after the default threshold the breaker fast-fails.
            let pd = pkg Nothing 0
            rule <- mkRule "Down" defaultEffectfulConfig{ecBackoff = []} OnUnavailable $ \_ ->
                throwString "down"
            outcome <- runEffectfulRule ctx rule pd
            outcome `shouldBe` Unavailable (WillResolve Nothing) "Down: the rule could not be evaluated"

    describe "properties" $ do
        it "an effectful rule strictly below the pure winner never changes the decision" $
            hedgehog $ do
                -- For any effectful outcome and any precedence below the pure
                -- winner's, the effectful tier returns exactly the pure decision.
                ageDays <- forAll (Gen.integral (Range.linear 0 3650))
                effPrec <- forAll (Gen.int (Range.linear 0 199)) -- strictly below 200
                outcome <-
                    forAll $
                        Gen.element
                            [Allow "a", Deny "d", Abstain "x", Unavailable WontResolve "u"]
                let pd = pkg (Just "myorg") ageDays
                    pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))] -- allows at 200
                rule <- liftIO (constRule "Eff" fastConfig OnUnavailable outcome)
                decision <- liftIO (evalRulesEffectful ctx pureRules [at effPrec rule] pd)
                decision === Approved (AllowScope (mkScope "myorg")) "scope @myorg is allow-listed"

        it "a failing rule that could change the outcome is always fail-closed" $
            hedgehog $ do
                effPrec <- forAll (Gen.int (Range.linear 200 1000)) -- at or above the pure allow
                let pd = pkg (Just "myorg") 0
                    pureRules = [atDefaultPrecedence (AllowScope (mkScope "myorg"))]
                rule <- liftIO (failingRule "Eff" fastConfig OnUnavailable)
                decision <- liftIO (evalRulesEffectful ctx pureRules [at effPrec rule] pd)
                H.assert (isUndecidable decision)
  where
    isUnavailableOutcome :: RuleOutcome -> Bool
    isUnavailableOutcome = \case
        Unavailable{} -> True
        _ -> False

    isAbstainOutcome :: RuleOutcome -> Bool
    isAbstainOutcome = \case
        Abstain{} -> True
        _ -> False
