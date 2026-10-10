-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The sentence each typed rule result renders to. The pins are the evidence: the recorded
sentences, written out in full. The properties compare the render with a second copy of the same
construction, so they catch an edit to one copy and not an error both copies share.
-}
module Ecluse.Core.Rules.RenderSpec (spec) where

import Data.Fixed (Fixed (MkFixed))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), fromGregorian, nominalDay, nominalDiffTimeToSeconds, picosecondsToDiffTime, secondsToNominalDiffTime)
import Hedgehog (Gen, PropertyT, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Package (Scope, mkScope, renderScope)
import Ecluse.Core.Rules.Render
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (renderIso8601Utc)
import Ecluse.Rules.Support (pkg)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)

spec :: Spec
spec = do
    reasonSpec
    inabilitySpec
    durationSpec
    decisionSpec
    readBackSpec
    oracleSpec

-- | Every 'Reason' constructor, with its facts at their edges, beside the sentence it reads as.
reasonPins :: [(Reason, Text)]
reasonPins =
    [ (ScopeAllowListed (mkScope "myorg"), "scope @myorg is allow-listed")
    , (ScopeNotAllowListed (mkScope "myorg"), "scope is not the allow-listed @myorg")
    , (PublishedLongEnough (30 * nominalDay) (7 * nominalDay), "published 30 days ago (at least 7 days old)")
    , (PublishedLongEnough (7 * nominalDay) (7 * nominalDay), "published 7 days ago (at least 7 days old)")
    , (PublishedLongEnough 0 0, "published 0 seconds ago (at least 0 seconds old)")
    , (PublishedLongEnough 1 1, "published 1 second ago (at least 1 second old)")
    , (PublishedLongEnough 0 (negate nominalDay), "published 0 seconds ago (at least 0 seconds old)")
    , (PublishedLongEnough 1_000_000_000_000_000 (7 * nominalDay), "published 11574074074 days 1 hour ago (at least 7 days old)")
    , (PublishedTooRecently nominalDay (7 * nominalDay), "published only 1 day ago, minimum age is 7 days")
    , (PublishedTooRecently 0 (7 * nominalDay), "published only 0 seconds ago, minimum age is 7 days")
    , (PublishedTooRecently (negate 3600) (7 * nominalDay), "published only 0 seconds ago, minimum age is 7 days")
    , (PublishedTooRecently 604799 604800, "published only 6 days 23 hours ago, minimum age is 7 days")
    , (PublishedTooRecently 90 3600, "published only 1 minute 30 seconds ago, minimum age is 1 hour")
    , (PublishedTooRecently 0.4 60, "published only 0 seconds ago, minimum age is 1 minute")
    , (PublishTimeUnknown, "publish time is unknown")
    , (RunsOnInstall "postinstall hook", "runs code on install: postinstall hook")
    , (RunsOnInstall "", "runs code on install: ")
    , (NothingRunsOnInstall, "no install-time code execution")
    , (InstallCodeUndetermined, "install-time code execution not yet determined")
    , (IdentityRevoked "thing@1.0.0", "identity thing@1.0.0 is revoked by operator")
    , (IdentityNotRevoked "thing@1.0.0", "identity is not the revoked thing@1.0.0")
    , (IdentityAllowListed "@myorg/thing", "identity @myorg/thing is allow-listed by operator")
    , (IdentityNotAllowListed "Flask_Thing", "identity is not the allow-listed Flask_Thing")
    , (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| [])), "remediates GHSA-fixed-0001")
    , (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| ["GHSA-fixed-0002"])), "remediates GHSA-fixed-0001, GHSA-fixed-0002")
    , (FixesButStillAffected (mkAdvisoryIds ("GHSA-fixed-0001" :| [])) (mkAdvisoryIds ("GHSA-open-0002" :| [])), "fixes GHSA-fixed-0001 but is still affected by GHSA-open-0002")
    , (FixesButStillAffected (mkAdvisoryIds ("GHSA-a" :| ["GHSA-b"])) (mkAdvisoryIds ("GHSA-c" :| ["MAL-d"])), "fixes GHSA-a, GHSA-b but is still affected by GHSA-c, MAL-d")
    , (FixesNoAdvisory, "no advisory names this version as its fix")
    , (NoDatabaseToRemediate, "no advisory database is loaded")
    , (AffectedBy Cvss 8.0 (mkAdvisoryIds ("GHSA-affect-0001" :| [])), "affected by GHSA-affect-0001 (CVSS >= 8.0)")
    , (AffectedBy Epss 0.5 (mkAdvisoryIds ("GHSA-affect-0001" :| [])), "affected by GHSA-affect-0001 (EPSS >= 0.5)")
    , (AffectedBy Cvss 7.0 (mkAdvisoryIds ("CVE-2026-0001" :| ["GHSA-aaaa-bbbb-cccc"])), "affected by CVE-2026-0001, GHSA-aaaa-bbbb-cccc (CVSS >= 7.0)")
    , (AffectedBy Cvss 0 (mkAdvisoryIds ("MAL-2026-1" :| [])), "affected by MAL-2026-1 (CVSS >= 0.0)")
    , (AffectedBy Cvss 10 (mkAdvisoryIds ("MAL-2026-1" :| [])), "affected by MAL-2026-1 (CVSS >= 10.0)")
    , (AffectedBy Epss 1 (mkAdvisoryIds ("CVE-2026-10002" :| [])), "affected by CVE-2026-10002 (EPSS >= 1.0)")
    , (AffectedBy Epss 0.05 (mkAdvisoryIds ("CVE-2026-10002" :| [])), "affected by CVE-2026-10002 (EPSS >= 5.0e-2)")
    , (NotAffectedAtThreshold Cvss, "no advisory at or above the CVSS threshold affects this version")
    , (NotAffectedAtThreshold Epss, "no advisory at or above the EPSS threshold affects this version")
    , (RuleUnable "AllowIfOlderThan" PublishTimeUnread, "AllowIfOlderThan: the publish time is not available")
    , (RuleUnable "DenyInstallTimeExecution" InstallSignalUnread, "DenyInstallTimeExecution: the install-time execution signal is not available")
    , (RuleUnable "DenyIfCve" NoDatabaseLoaded, "DenyIfCve: no advisory database loaded")
    , (RuleUnable "DenyIfEpss" NoDatabaseLoaded, "DenyIfEpss: no advisory database loaded")
    , (RuleUnable "DenyIfCve" SourceBreakerOpen, "DenyIfCve: the rule source circuit breaker is open")
    , (RuleUnable "DenyIfCve" EvaluationFailed, "DenyIfCve: the rule could not be evaluated")
    , (RuleUnable "DirectBomb" (RuleThrew "TestContractEscape \"the rule threw\""), "DirectBomb: the rule threw: TestContractEscape \"the rule threw\"")
    ]

-- | Every 'Inability' constructor beside the sentence it reads as, without a rule's name.
inabilityPins :: [(Inability, Text)]
inabilityPins =
    [ (PublishTimeUnread, "the publish time is not available")
    , (InstallSignalUnread, "the install-time execution signal is not available")
    , (NoDatabaseLoaded, "no advisory database loaded")
    , (PushPastMaximum (AdvisoryAge pushedAt (9 * nominalDay) (6 * nominalDay)), "the advisory push is 9 days old, past the maximum of 6 days (pushed at 2026-06-11T00:00:00Z)")
    , (PushPastMaximum (AdvisoryAge pushedAt (6 * nominalDay + 1) (6 * nominalDay)), "the advisory push is 6 days 1 second old, past the maximum of 6 days (pushed at 2026-06-11T00:00:00Z)")
    , (PushPastMaximum (AdvisoryAge pushedMidSecond 3601 3600), "the advisory push is 1 hour 1 second old, past the maximum of 1 hour (pushed at 2026-06-11T12:34:56.5Z)")
    , (PushPastMaximum (AdvisoryAge pushedAt 1 0), "the advisory push is 1 second old, past the maximum of 0 seconds (pushed at 2026-06-11T00:00:00Z)")
    , (PushUndated, "the object store reported no publication time for the serving advisory artifact")
    , (SourceBreakerOpen, "the rule source circuit breaker is open")
    , (EvaluationFailed, "the rule could not be evaluated")
    , (RuleThrew "advisory database exploded", "the rule threw: advisory database exploded")
    , (RuleThrew "", "the rule threw: ")
    , (AttemptTimedOut, "the attempt timed out")
    ]

pushedAt :: UTCTime
pushedAt = UTCTime (fromGregorian 2026 6 11) 0

pushedMidSecond :: UTCTime
pushedMidSecond = UTCTime (fromGregorian 2026 6 11) (picosecondsToDiffTime 45_296_500_000_000_000)

{- | One tag for each 'Reason' constructor. 'reasonTag' matches every constructor, so a new one
does not compile until it has a tag, and then fails here until it has a pin and a generator arm.
-}
data ReasonTag
    = ScopeAllowListedTag
    | ScopeNotAllowListedTag
    | PublishedLongEnoughTag
    | PublishedTooRecentlyTag
    | PublishTimeUnknownTag
    | RunsOnInstallTag
    | NothingRunsOnInstallTag
    | InstallCodeUndeterminedTag
    | IdentityRevokedTag
    | IdentityNotRevokedTag
    | IdentityAllowListedTag
    | IdentityNotAllowListedTag
    | RemediatesTag
    | FixesButStillAffectedTag
    | FixesNoAdvisoryTag
    | NoDatabaseToRemediateTag
    | AffectedByTag
    | NotAffectedAtThresholdTag
    | RuleUnableTag
    deriving stock (Bounded, Enum, Eq, Ord, Show)

reasonTag :: Reason -> ReasonTag
reasonTag = \case
    ScopeAllowListed{} -> ScopeAllowListedTag
    ScopeNotAllowListed{} -> ScopeNotAllowListedTag
    PublishedLongEnough{} -> PublishedLongEnoughTag
    PublishedTooRecently{} -> PublishedTooRecentlyTag
    PublishTimeUnknown -> PublishTimeUnknownTag
    RunsOnInstall{} -> RunsOnInstallTag
    NothingRunsOnInstall -> NothingRunsOnInstallTag
    InstallCodeUndetermined -> InstallCodeUndeterminedTag
    IdentityRevoked{} -> IdentityRevokedTag
    IdentityNotRevoked{} -> IdentityNotRevokedTag
    IdentityAllowListed{} -> IdentityAllowListedTag
    IdentityNotAllowListed{} -> IdentityNotAllowListedTag
    Remediates{} -> RemediatesTag
    FixesButStillAffected{} -> FixesButStillAffectedTag
    FixesNoAdvisory -> FixesNoAdvisoryTag
    NoDatabaseToRemediate -> NoDatabaseToRemediateTag
    AffectedBy{} -> AffectedByTag
    NotAffectedAtThreshold{} -> NotAffectedAtThresholdTag
    RuleUnable{} -> RuleUnableTag

-- | One tag for each 'Inability' constructor, under the same guard as 'ReasonTag'.
data InabilityTag
    = PublishTimeUnreadTag
    | InstallSignalUnreadTag
    | NoDatabaseLoadedTag
    | PushPastMaximumTag
    | PushUndatedTag
    | SourceBreakerOpenTag
    | EvaluationFailedTag
    | RuleThrewTag
    | AttemptTimedOutTag
    deriving stock (Bounded, Enum, Eq, Ord, Show)

inabilityTag :: Inability -> InabilityTag
inabilityTag = \case
    PublishTimeUnread -> PublishTimeUnreadTag
    InstallSignalUnread -> InstallSignalUnreadTag
    NoDatabaseLoaded -> NoDatabaseLoadedTag
    PushPastMaximum{} -> PushPastMaximumTag
    PushUndated -> PushUndatedTag
    SourceBreakerOpen -> SourceBreakerOpenTag
    EvaluationFailed -> EvaluationFailedTag
    RuleThrew{} -> RuleThrewTag
    AttemptTimedOut -> AttemptTimedOutTag

-- | Require that the run drew every tag, each in at least one case in a hundred.
coverEvery :: (Bounded tag, Enum tag, Eq tag, Show tag) => tag -> PropertyT IO ()
coverEvery drawn = for_ [minBound .. maxBound] $ \tag -> cover 1 (show tag) (drawn == tag)

reasonSpec :: Spec
reasonSpec = describe "renderReason" $ do
    for_ reasonPins $ \(reason, sentence) ->
        it ("reads " <> show reason <> " as its recorded sentence") $
            renderReason reason `shouldBe` sentence
    it "has a recorded sentence for every constructor" $
        ordNub (sort (map (reasonTag . fst) reasonPins)) `shouldBe` [minBound .. maxBound]

inabilitySpec :: Spec
inabilitySpec = describe "renderInability" $ do
    for_ inabilityPins $ \(inability, sentence) ->
        it ("reads " <> show inability <> " as its recorded sentence") $
            renderInability inability `shouldBe` sentence
    it "has a recorded sentence for every constructor" $
        ordNub (sort (map (inabilityTag . fst) inabilityPins)) `shouldBe` [minBound .. maxBound]

durationSpec :: Spec
durationSpec = describe "renderDuration" $ do
    it "renders a whole unit as that unit alone" $ do
        renderDuration 604800 `shouldBe` "7 days"
        renderDuration 86400 `shouldBe` "1 day"
        renderDuration 60 `shouldBe` "1 minute"
    it "renders the two most-significant non-zero units" $ do
        renderDuration 90 `shouldBe` "1 minute 30 seconds"
        renderDuration 3661 `shouldBe` "1 hour 1 minute"
        renderDuration 86700 `shouldBe` "1 day 5 minutes"
    it "distinguishes a value just short of a threshold from the threshold" $ do
        renderDuration 89 `shouldBe` "1 minute 29 seconds"
        renderDuration 90 `shouldBe` "1 minute 30 seconds"
    it "pluralises only non-unit counts" $ do
        renderDuration 1 `shouldBe` "1 second"
        renderDuration 2 `shouldBe` "2 seconds"
    it "renders a zero or sub-second duration as zero seconds" $ do
        renderDuration 0 `shouldBe` "0 seconds"
        renderDuration 0.4 `shouldBe` "0 seconds"
    it "clamps a negative duration to zero" $
        renderDuration (negate 5) `shouldBe` "0 seconds"
    it "renders one and two of every unit, alone and beside the next unit down" $
        map (first renderDuration) unitEdges `shouldBe` map (\(_, sentence) -> (sentence, sentence)) unitEdges
    it "rounds a half second to the even second" $ do
        renderDuration 0.5 `shouldBe` "0 seconds"
        renderDuration 1.5 `shouldBe` "2 seconds"
        renderDuration 59.5 `shouldBe` "1 minute"
    it "keeps the day count whole for the largest durations" $ do
        renderDuration 8_640_000_000 `shouldBe` "100000 days"
        renderDuration 1_000_000_000_000_000 `shouldBe` "11574074074 days 1 hour"

-- | One and two of each unit, and each unit beside one of the next unit down.
unitEdges :: [(NominalDiffTime, Text)]
unitEdges =
    [ (1, "1 second")
    , (2, "2 seconds")
    , (59, "59 seconds")
    , (60, "1 minute")
    , (61, "1 minute 1 second")
    , (120, "2 minutes")
    , (3599, "59 minutes 59 seconds")
    , (3600, "1 hour")
    , (3601, "1 hour 1 second")
    , (3660, "1 hour 1 minute")
    , (7200, "2 hours")
    , (86399, "23 hours 59 minutes")
    , (86400, "1 day")
    , (86401, "1 day 1 second")
    , (86460, "1 day 1 minute")
    , (90000, "1 day 1 hour")
    , (90061, "1 day 1 hour")
    , (172800, "2 days")
    ]

decisionSpec :: Spec
decisionSpec = describe "renderDecision" $ do
    -- The whole line, not a substring: it reaches an operator, so the subject, the verb,
    -- the rule, and the reason each have to stay where they are.
    let pd = pkg (Just "myorg") 0
    it "renders an admission naming the rule and its reason" $
        renderDecision pd (Admitted "AllowScope" (ScopeAllowListed (mkScope "myorg")) [])
            `shouldBe` "@myorg/thing@1.0.0 was approved by AllowScope: scope @myorg is allow-listed"
    it "renders an admission's skipped and unreached checks as a parenthetical after its reason" $
        renderDecision pd (Admitted "AllowScope" (ScopeAllowListed (mkScope "myorg")) [SkippedUnavailable "DenyIfCve" NoDatabaseLoaded, Unreached "DenyIfEpss"])
            `shouldBe` "@myorg/thing@1.0.0 was approved by AllowScope: scope @myorg is allow-listed (skipped for unavailability: DenyIfCve (no advisory database loaded); not reached: DenyIfEpss)"
    it "renders a PyPI subject by its project name as written" $
        renderDecision (identityEvidence (unscopedPyPI "Flask_Thing") (pypiVersion "1.0.0")) (Admitted "AllowByIdentity" (IdentityAllowListed "Flask_Thing") [])
            `shouldBe` "Flask_Thing@1.0.0 was approved by AllowByIdentity: identity Flask_Thing is allow-listed by operator"
    it "renders a block naming the rule and its reason" $
        renderDecision pd (Blocked "DenyIfCve" (Just (DbEtag "etag-1")) (AffectedBy Cvss 8.0 (mkAdvisoryIds ("GHSA-affect-0001" :| []))))
            `shouldBe` "@myorg/thing@1.0.0 was denied by DenyIfCve: affected by GHSA-affect-0001 (CVSS >= 8.0)"
    it "renders a deny-by-default explaining no rule allowed it, then every reason" $
        renderDecision pd (BlockedByDefault [ScopeNotAllowListed (mkScope "myorg"), PublishedTooRecently nominalDay (7 * nominalDay), RuleUnable "DenyIfCve" NoDatabaseLoaded])
            `shouldBe` "@myorg/thing@1.0.0 was denied (no rule allowed it): scope is not the allow-listed @myorg; published only 1 day ago, minimum age is 7 days; DenyIfCve: no advisory database loaded"
    it "renders a deny-by-default with no reasons as the verdict alone" $
        renderDecision pd (BlockedByDefault []) `shouldBe` "@myorg/thing@1.0.0 was denied (no rule allowed it)"
    it "renders an undecidable outcome explaining it could not be evaluated" $
        renderDecision pd (Undecidable (WillResolve Nothing) (RuleUnable "DenyIfCve" SourceBreakerOpen))
            `shouldBe` "@myorg/thing@1.0.0 could not be evaluated: DenyIfCve: the rule source circuit breaker is open"

readBackSpec :: Spec
readBackSpec = describe "cveIdsInReason -- recovering advisory ids for the denial audit line" $ do
    -- The audit layer reads the ids back from the rendered deny reason, so a reword of either
    -- fails one of these.
    let denyReason = "affected by GHSA-affect-0001 (CVSS >= 8.0)"
    it "recovers the id a DenyIfCve denial named" $
        cveIdsInReason denyReason `shouldBe` ["GHSA-affect-0001"]
    it "recovers the id a DenyIfEpss denial named" $
        cveIdsInReason "affected by GHSA-affect-0001 (EPSS >= 0.5)" `shouldBe` ["GHSA-affect-0001"]
    it "recovers several ids" $
        cveIdsInReason "affected by CVE-2026-0001, GHSA-aaaa-bbbb-cccc (CVSS >= 7.0)"
            `shouldBe` ["CVE-2026-0001", "GHSA-aaaa-bbbb-cccc"]
    it "recovers them from the wrapped decision message the audit line carries" $
        -- The audit layer sees the rendered decision's wrapping, not the raw reason.
        cveIdsInReason ("thing@1.0.0 was denied by DenyIfCve: " <> denyReason)
            `shouldBe` ["GHSA-affect-0001"]
    it "yields nothing for a non-CVE denial" $ do
        cveIdsInReason "runs code on install: postinstall" `shouldBe` []
        cveIdsInReason "thing@1.0.0 was denied by DenyInstallTimeExecution: runs code on install"
            `shouldBe` []
    it "recovers every id an advisory denial names from the decision it renders to" $
        hedgehog $ do
            score <- forAll genScore
            threshold <- forAll genThreshold
            ids <- forAll genIds
            let denial = Blocked "DenyIfCve" (Just (DbEtag "etag-1")) (AffectedBy score threshold ids)
            cveIdsInReason (renderDecision (pkg Nothing 0) denial) === toList (unAdvisoryIds ids)

oracleSpec :: Spec
oracleSpec = describe "properties" . modifyMaxSuccess (const 1000) $ do
    it "renders a generated reason of every constructor as the second copy of its sentence does" $
        hedgehog $ do
            reason <- forAll genReason
            coverEvery (reasonTag reason)
            renderReason reason === oracleReason reason
    it "renders a generated inability of every constructor as the second copy of its sentence does" $
        hedgehog $ do
            inability <- forAll genInability
            coverEvery (inabilityTag inability)
            renderInability inability === oracleInability inability
    it "renders a generated duration of every class as the second copy of the ladder does" $
        hedgehog $ do
            duration <- forAll genDuration
            cover 5 "negative" (duration < 0)
            cover 5 "under a second" (duration > 0 && duration < 1)
            for_ [("minute", 60), ("hour", 3600), ("day", 86400)] $ \(unit, boundary) ->
                cover 2 (fromString ("within a second of one " <> unit)) (abs (duration - boundary) <= 1)
            cover 5 "more than two non-zero units" (length (filter (/= 0) (unitCounts duration)) > 2)
            renderDuration duration === oracleDuration duration

-- | The whole days, hours, minutes and seconds of a duration, for the property's coverage classes.
unitCounts :: NominalDiffTime -> [Integer]
unitCounts duration = [days, hours, minutes, seconds]
  where
    (days, underDay) = max 0 (round (nominalDiffTimeToSeconds duration)) `divMod` 86400
    (hours, underHour) = underDay `divMod` 3600
    (minutes, seconds) = underHour `divMod` 60

{- | A second copy of each reason's sentence. Every constructor is matched, so a new one does not
compile until it has a sentence here.
-}
oracleReason :: Reason -> Text
oracleReason = \case
    ScopeAllowListed scope -> "scope " <> renderScope scope <> " is allow-listed"
    ScopeNotAllowListed scope -> "scope is not the allow-listed " <> renderScope scope
    PublishedLongEnough age minAge -> "published " <> oracleDuration age <> " ago (at least " <> oracleDuration minAge <> " old)"
    PublishedTooRecently age minAge -> "published only " <> oracleDuration age <> " ago, minimum age is " <> oracleDuration minAge
    PublishTimeUnknown -> "publish time is unknown"
    RunsOnInstall how -> "runs code on install: " <> how
    NothingRunsOnInstall -> "no install-time code execution"
    InstallCodeUndetermined -> "install-time code execution not yet determined"
    IdentityRevoked ident -> "identity " <> ident <> " is revoked by operator"
    IdentityNotRevoked ident -> "identity is not the revoked " <> ident
    IdentityAllowListed ident -> "identity " <> ident <> " is allow-listed by operator"
    IdentityNotAllowListed ident -> "identity is not the allow-listed " <> ident
    Remediates ids -> "remediates " <> commaJoined ids
    FixesButStillAffected ids open -> "fixes " <> commaJoined ids <> " but is still affected by " <> commaJoined open
    FixesNoAdvisory -> "no advisory names this version as its fix"
    NoDatabaseToRemediate -> "no advisory database is loaded"
    AffectedBy score threshold ids -> "affected by " <> commaJoined ids <> " (" <> oracleMetric score <> " >= " <> show threshold <> ")"
    NotAffectedAtThreshold score -> "no advisory at or above the " <> oracleMetric score <> " threshold affects this version"
    RuleUnable rule inability -> rule <> ": " <> oracleInability inability

commaJoined :: AdvisoryIds -> Text
commaJoined = T.intercalate ", " . toList . unAdvisoryIds

oracleMetric :: AdvisoryScore -> Text
oracleMetric = \case
    Cvss -> "CVSS"
    Epss -> "EPSS"

oracleInability :: Inability -> Text
oracleInability = \case
    PublishTimeUnread -> "the publish time" <> " is not available"
    InstallSignalUnread -> "the install-time execution signal" <> " is not available"
    NoDatabaseLoaded -> "no advisory database loaded"
    PushPastMaximum observed ->
        "the advisory push is "
            <> oracleDuration (advisoryAge observed)
            <> " old, past the maximum of "
            <> oracleDuration (advisoryMaxAge observed)
            <> " (pushed at "
            <> renderIso8601Utc (advisoryPushedAt observed)
            <> ")"
    PushUndated -> "the object store reported no publication time for the serving advisory artifact"
    SourceBreakerOpen -> "the rule source circuit breaker is open"
    EvaluationFailed -> "the rule could not be evaluated"
    RuleThrew thrown -> "the rule threw: " <> thrown
    AttemptTimedOut -> "the attempt timed out"

{- | A second copy of the two-unit ladder. It is the same algorithm as 'renderDuration', so it
guards the render against a later edit and proves nothing the pins in 'durationSpec' do not.
-}
oracleDuration :: NominalDiffTime -> Text
oracleDuration d = case take 2 (components ladder secs) of
    [] -> "0 seconds"
    parts -> T.unwords [show n <> " " <> unit <> (if n == 1 then "" else "s") | (unit, n) <- parts]
  where
    secs = max 0 (round (nominalDiffTimeToSeconds d)) :: Integer
    ladder = [("day", 86400), ("hour", 3600), ("minute", 60), ("second", 1)] :: [(Text, Integer)]
    components [] _ = []
    components ((unit, size) : rest) r =
        let (q, r') = r `divMod` size
         in [(unit, q) | q > 0] <> components rest r'

genReason :: Gen Reason
genReason =
    Gen.choice
        [ ScopeAllowListed <$> genScope
        , ScopeNotAllowListed <$> genScope
        , PublishedLongEnough <$> genDuration <*> genDuration
        , PublishedTooRecently <$> genDuration <*> genDuration
        , pure PublishTimeUnknown
        , RunsOnInstall <$> genFact
        , pure NothingRunsOnInstall
        , pure InstallCodeUndetermined
        , IdentityRevoked <$> genFact
        , IdentityNotRevoked <$> genFact
        , IdentityAllowListed <$> genFact
        , IdentityNotAllowListed <$> genFact
        , Remediates <$> genIds
        , FixesButStillAffected <$> genIds <*> genIds
        , pure FixesNoAdvisory
        , pure NoDatabaseToRemediate
        , AffectedBy <$> genScore <*> genThreshold <*> genIds
        , NotAffectedAtThreshold <$> genScore
        , RuleUnable <$> genFact <*> genInability
        ]

genInability :: Gen Inability
genInability =
    Gen.choice
        [ pure PublishTimeUnread
        , pure InstallSignalUnread
        , pure NoDatabaseLoaded
        , PushPastMaximum <$> (AdvisoryAge <$> genInstant <*> genDuration <*> genDuration)
        , pure PushUndated
        , pure SourceBreakerOpen
        , pure EvaluationFailed
        , RuleThrew <$> genFact
        , pure AttemptTimedOut
        ]

genScope :: Gen Scope
genScope = mkScope <$> Gen.text (Range.linear 1 12) Gen.alphaNum

-- | Free text a reason carries as it was read: an identity, an install hook, an exception.
genFact :: Gen Text
genFact = Gen.text (Range.linear 0 24) Gen.unicode

genIds :: Gen AdvisoryIds
genIds = mkAdvisoryIds <$> Gen.nonEmpty (Range.linear 1 5) (Gen.text (Range.linear 1 19) (Gen.choice [Gen.alphaNum, pure '-']))

genScore :: Gen AdvisoryScore
genScore = Gen.element [Cvss, Epss]

-- | A threshold in either score's range, with the values whose text is not a plain decimal.
genThreshold :: Gen Double
genThreshold = Gen.choice [Gen.element [0, 0.05, 0.5, 1, 7, 10], Gen.double (Range.linearFrac 0 10)]

{- | A duration in picoseconds. A sixth are negative, a sixth under a second, and a quarter within
a second of a unit boundary. The rest run to a thousand days and to past a million years.
-}
genDuration :: Gen NominalDiffTime
genDuration =
    Gen.frequency
        [ (2, negate . picoseconds <$> Gen.integral (Range.linear 1 86_400_000_000_000_000))
        , (2, picoseconds <$> Gen.integral (Range.linear 1 999_999_999_999))
        , (3, (+) <$> Gen.element [60, 3600, 86400] <*> (picoseconds <$> Gen.integral (Range.linearFrom 0 (negate 1_000_000_000_000) 1_000_000_000_000)))
        , (3, picoseconds <$> Gen.integral (Range.linear 0 86_400_000_000_000_000_000))
        , (1, picoseconds <$> Gen.integral (Range.linear 0 100_000_000_000_000_000_000_000_000))
        , (1, Gen.element [0, 0.5, 1, 1.5, 59.5, 604800])
        ]
  where
    picoseconds = secondsToNominalDiffTime . MkFixed

genInstant :: Gen UTCTime
genInstant =
    UTCTime
        <$> (fromGregorian <$> Gen.integral (Range.linear 1970 2100) <*> Gen.int (Range.linear 1 12) <*> Gen.int (Range.linear 1 28))
        <*> (picosecondsToDiffTime <$> Gen.integral (Range.linear 0 86_399_999_999_999_999))
