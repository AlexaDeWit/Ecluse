-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The rules evaluator and the advisory verdicts as they stood at f5f368d31: the reference the
evaluator's differential properties decide against.
-}
module Ecluse.Rules.FrozenEvaluator (
    frozenEvaluator,
    frozenPrepare,
) where

import Data.Foldable1 qualified as Foldable1
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text qualified as T
import UnliftIO (evaluate, tryAny)
import UnliftIO.MVar (modifyMVar)

import Ecluse.Core.Cve (AdvisoryRange (..), MissingScorePolicy (..), PackageAdvisories, advisoryIdsOf, affecting, fixedAt, keepAdvisories, scoreAtLeast)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Rules (
    AdvisoryAlignment (..),
    AdvisoryRows,
    PackageRead (..),
    PreparedRule (..),
    RuleDeps,
    RuleEval (..),
    SourceHealth (..),
    SourceReporter (..),
    prepare,
    pushInability,
 )
import Ecluse.Core.Rules.Effectful (ReadFault (..), runResilient)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Test.Rules (mapPackageRead)

-- | 'Ecluse.Core.Rules.prepare', with each advisory rule's verdict as it stood.
frozenPrepare :: RuleDeps -> [PrecededRule] -> IO [PreparedRule]
frozenPrepare deps rules = zipWith (frozenVerdict . prRule) rules <$> prepare deps rules

frozenVerdict :: Rule -> PreparedRule -> PreparedRule
frozenVerdict rule = maybe id (\verdict -> mapPackageRead (\packageRead -> packageRead{prVerdict = verdict})) (verdictAsItStood rule)

-- Only the advisory rules read the package. Every other rule keeps its prepared per-version verdict.
verdictAsItStood :: Rule -> Maybe (AdvisoryRows -> RuleEvidence -> RuleVerdict)
verdictAsItStood = \case
    AllowIfRemediatesCve params ->
        let decide = classify (partialVerdict (aircRemaining params))
         in Just $ \case
                Nothing -> const (CannotVet (aircOnUnavailable params) NoDatabaseLoaded)
                Just (_, advisories) -> decide advisories
    DenyIfCve params -> Just $ \case
        Nothing -> const (CannotVet (dicOnUnavailable params) NoDatabaseLoaded)
        Just (etag, advisories) -> advisoryDenyVerdict etag DenyMissingScore Cvss (dicMinCvss params) arSeverity advisories
    DenyIfKnownMalicious alignment -> Just $ \case
        Nothing -> const (CannotVet alignment NoDatabaseLoaded)
        Just (etag, advisories) -> affectedVerdict etag KnownMalicious NoKnownMaliciousAdvisory (keepAdvisories isMalicious advisories)
    DenyIfEpss params -> Just $ \case
        Nothing -> const (CannotVet (dieOnUnavailable params) NoDatabaseLoaded)
        Just (etag, advisories) -> advisoryDenyVerdict etag AbstainMissingScore Epss (dieMinEpss params) arEpss advisories
    _ -> Nothing

advisoryDenyVerdict :: DbEtag -> MissingScorePolicy -> AdvisoryScore -> Double -> (AdvisoryRange -> Maybe Double) -> PackageAdvisories -> RuleEvidence -> RuleVerdict
advisoryDenyVerdict etag missing score threshold scoreOf advisories =
    affectedVerdict etag (AffectedBy score threshold) (NotAffectedAtThreshold score) scored
  where
    scored = keepAdvisories (scoreAtLeast missing threshold . scoreOf) advisories

isMalicious :: AdvisoryRange -> Bool
isMalicious = maybe False (not . T.null) . T.stripPrefix "MAL-" . arCveId

affectedVerdict :: DbEtag -> (AdvisoryIds -> Reason) -> Reason -> PackageAdvisories -> RuleEvidence -> RuleVerdict
affectedVerdict etag affectedReason absentReason advisories ev =
    case advisoryIdsOf (affecting advisories (evVersion ev)) of
        [] -> NoDecision absentReason
        affected : more -> Deny (Just etag) (affectedReason (mkAdvisoryIds (affected :| more)))

data PartialFix = PartialFix
    { pfFixed :: AdvisoryIds
    , pfFixedRows :: [AdvisoryRange]
    , pfRemaining :: AdvisoryIds
    , pfRemainingRows :: Map Text [AdvisoryRange]
    }

classify :: (PartialFix -> RuleVerdict) -> PackageAdvisories -> RuleEvidence -> RuleVerdict
classify onPartial advisories ev = case idsOf (advisoryIdsOf fixedRows) of
    Nothing -> NoDecision FixesNoAdvisory
    Just fixed -> case idsOf openIds of
        Nothing -> Allow (Remediates fixed)
        Just remaining -> case idsOf (advisoryIdsOf genuineRows) of
            Nothing -> NoDecision (FixStillApplies remaining)
            Just genuine -> onPartial (PartialFix genuine genuineRows remaining openByAdvisory)
  where
    fixedRows = fixedAt advisories (evVersion ev)
    openRows = affecting advisories (evVersion ev)
    openIds = advisoryIdsOf openRows
    openByAdvisory = Map.fromListWith (flip (<>)) [(arCveId row, [row]) | row <- openRows]
    genuineRows = filter ((`Map.notMember` openByAdvisory) . arCveId) fixedRows

idsOf :: [Text] -> Maybe AdvisoryIds
idsOf = fmap mkAdvisoryIds . nonEmpty

partialVerdict :: RemainingAdvisories -> PartialFix -> RuleVerdict
partialVerdict = \case
    AbstainOnRemaining -> \partial -> NoDecision (FixesButStillAffected (pfFixed partial) (pfRemaining partial))
    Differential limits ->
        let cvss = scoreCheck Cvss arSeverity (<=) (dlMaxRemainingCvss limits) (dlCvssGap limits)
            epss = scoreCheck Epss arEpss (<) (dlMaxRemainingEpss limits) (dlEpssGap limits)
         in differentialVerdict limits cvss epss

data ScoreCheck = ScoreCheck
    { ckScore :: AdvisoryScore
    , ckRead :: AdvisoryRange -> Maybe Double
    , ckWithinCeiling :: Scientific -> Bool
    , ckGapHolds :: Maybe (Scientific -> Scientific -> Bool)
    }

scoreCheck :: AdvisoryScore -> (AdvisoryRange -> Maybe Double) -> (Scientific -> Scientific -> Bool) -> Double -> ScoreGap -> ScoreCheck
scoreCheck score readScore withinCeiling limit gap =
    ScoreCheck
        { ckScore = score
        , ckRead = readScore
        , ckWithinCeiling = let bound = decimal limit in \highest -> maybe False (highest `withinCeiling`) bound
        , ckGapHolds = case gap of
            NoGapRequired -> Nothing
            GapOfAtLeast least -> let bound = decimal least in Just (\fixed remaining -> maybe False (fixed - remaining >=) bound)
        }

differentialVerdict :: DifferentialLimits -> ScoreCheck -> ScoreCheck -> PartialFix -> RuleVerdict
differentialVerdict limits cvss epss partial = case nonEmpty (cvssShortfalls <> epssShortfalls) of
    Nothing -> Allow (RemediatesWithinLimits comparison)
    Just shortfalls -> NoDecision (RemediationBeyondLimits comparison (mkLimitShortfalls shortfalls))
  where
    (cvssSides, cvssShortfalls) = checkScore cvss partial
    (epssSides, epssShortfalls) = checkScore epss partial
    comparison = ScoreComparison (pfFixed partial) (pfRemaining partial) cvssSides epssSides limits

checkScore :: ScoreCheck -> PartialFix -> (ScoreSides, [LimitShortfall])
checkScore check partial =
    (ScoreSides (reading fixedHighest) (reading remainingHighest), unscored <> aboveCeiling <> gap)
  where
    score = ckScore check
    scoreOf = finite <=< ckRead check
    fixedHighest = viaNonEmpty Foldable1.maximum (mapMaybe scoreOf (pfFixedRows partial))
    perAdvisory = [(advisory, traverse scoreOf (Map.findWithDefault [] advisory (pfRemainingRows partial))) | advisory <- toList (unAdvisoryIds (pfRemaining partial))]
    remainingHighest = viaNonEmpty Foldable1.maximum (concat [scores | (_, Just scores) <- perAdvisory])
    unscored = [RemainingUnscored score (mkAdvisoryIds ids) | Just ids <- [nonEmpty [advisory | (advisory, Nothing) <- perAdvisory]]]
    aboveCeiling = [RemainingAboveCeiling score | Just highest <- [remainingHighest], not (ckWithinCeiling check (fromFloatDigits highest))]
    gap = case (ckGapHolds check, fixedHighest, remainingHighest) of
        (Nothing, _, _) -> []
        (Just _, Nothing, _) -> [FixedUnscored score]
        (Just holds, Just fixed, Just remaining) | not (holds (fromFloatDigits fixed) (fromFloatDigits remaining)) -> [GapBelowMinimum score]
        (Just _, Just _, _) -> []

reading :: Maybe Double -> ScoreReading
reading = maybe Unscored Scored

finite :: Double -> Maybe Double
finite x = x <$ guard (not (isNaN x || isInfinite x))

decimal :: Double -> Maybe Scientific
decimal = fmap fromFloatDigits . finite

-- | 'Ecluse.Core.Rules.newEvaluator' as it stood.
frozenEvaluator :: EvalContext -> [PreparedRule] -> IO (RuleEvidence -> IO Decision)
frozenEvaluator ctx rules = do
    shared <- newPackageCell
    bound <- traverse (\rule -> (rule,) <$> bindRule ctx shared rule) (sortOn (\r -> (Down (prepPrecedence r), prepName r)) rules)
    pure (\ev -> stepRules ev bound [])

type VersionEval = RuleEvidence -> IO (Either Text RuleEvaluation)

bindRule :: EvalContext -> PackageCell (Either Text AdvisoryRead) -> PreparedRule -> IO VersionEval
bindRule ctx shared rule = case prepEval rule of
    PerVersion eval -> pure (caughtEvaluation . fmap Decided . eval ctx)
    PerPackage packageRead -> do
        own <- newPackageCell
        pure $ \ev ->
            heldFor own (evName ev) (decideFromRead (prepName rule) packageRead shared ev)
                >>= either (pure . Left) (caughtEvaluation . pure . ($ ev))

caughtEvaluation :: IO RuleEvaluation -> IO (Either Text RuleEvaluation)
caughtEvaluation action = caught $ do
    result <- action >>= evaluate
    case result of
        Decided (Deny etag _) -> traverse_ evaluate etag
        Unavailable (WillResolve retry) _ _ -> traverse_ evaluate retry
        _ -> pass
    pure result

caught :: IO a -> IO (Either Text a)
caught = fmap (first displayExceptionT) . tryAny

newtype PackageCell a = PackageCell (MVar (Maybe (PackageName, a)))

newPackageCell :: IO (PackageCell a)
newPackageCell = PackageCell <$> newMVar Nothing

heldFor :: PackageCell a -> PackageName -> IO a -> IO a
heldFor (PackageCell held) package compute =
    modifyMVar held $ \case
        Just (known, value) | known == package -> pure (Just (known, value), value)
        _ -> (\value -> (Just (package, value), value)) <$> compute

data AdvisoryRead
    = RowsRead AdvisoryRows
    | PushIneligible Inability
    | ReadGivenUp ReadFault

decideFromRead :: Text -> PackageRead -> PackageCell (Either Text AdvisoryRead) -> RuleEvidence -> IO (Either Text (RuleEvidence -> RuleEvaluation))
decideFromRead name packageRead shared reaching =
    heldFor shared package (caught (readShared packageRead package)) >>= \case
        Left escape -> pure (Left escape)
        Right advisory -> do
            let resolved = resolveRead packageRead advisory
            health <- caught (evaluate (readHealth name advisory (resolved reaching)))
            caught (resolved <$ reportSource (prReporter packageRead) (either (SourceUnavailable name . RuleThrew) id health))
  where
    package = evName reaching

readShared :: PackageRead -> PackageName -> IO AdvisoryRead
readShared packageRead package =
    prRead packageRead package $ \freshness rows -> case pushInability freshness of
        Just why -> pure (PushIneligible why)
        Nothing -> case prResilience packageRead of
            Nothing -> RowsRead <$> rows
            Just res -> either ReadGivenUp RowsRead <$> runResilient res rows

resolveRead :: PackageRead -> AdvisoryRead -> RuleEvidence -> RuleEvaluation
resolveRead packageRead = \case
    RowsRead rows -> Decided . prVerdict packageRead rows
    PushIneligible why -> const (Decided (CannotVet (onExpiredPush alignment) why))
    ReadGivenUp fault -> const (Unavailable (rfTransience fault) (onFaultedRead alignment) (rfReason fault))
  where
    alignment = prAlignment packageRead

readHealth :: Text -> AdvisoryRead -> RuleEvaluation -> SourceHealth
readHealth name advisory resolved = case advisory of
    ReadGivenUp fault -> SourceUnavailable name (rfDetail fault)
    _ -> case resolved of
        Decided (CannotVet _ why) -> SourceUnavailable name why
        Unavailable _ _ why -> SourceUnavailable name why
        Decided _ -> SourceAnswered name

stepRules :: RuleEvidence -> [(PreparedRule, VersionEval)] -> [Passed] -> IO Decision
stepRules _ [] passed = pure (BlockedByDefault (map passedReason (reverse passed)))
stepRules ev ((rule, eval) : rest) passed =
    eval ev >>= \case
        Left escape -> pure (Undecidable (WillResolve Nothing) (RuleUnable (prepName rule) (RuleThrew escape)))
        Right res -> case decisive (prepName rule) res of
            Just d -> pure (withEvidence passed (map fst rest) d)
            Nothing -> stepRules ev rest (Passed (prepName rule) res : passed)

data Passed = Passed Text RuleEvaluation

passedReason :: Passed -> Reason
passedReason (Passed name res) = case res of
    Unavailable _ _ why -> RuleUnable name why
    Decided (CannotVet _ why) -> RuleUnable name why
    Decided (Allow reason) -> reason
    Decided (Deny _ reason) -> reason
    Decided (NoDecision reason) -> reason

skippedOf :: Passed -> Maybe SkippedCheck
skippedOf (Passed name res) = case res of
    Decided (CannotVet alignment why) -> skipped alignment why
    Unavailable _ alignment why -> skipped alignment why
    Decided _ -> Nothing
  where
    skipped FailNoDecision why = Just (SkippedUnavailable name why)
    skipped FailDeny _ = Nothing

withEvidence :: [Passed] -> [PreparedRule] -> Decision -> Decision
withEvidence passed unreached = \case
    Admitted name reason _ -> Admitted name reason (reverse (mapMaybe skippedOf passed) <> map (Unreached . prepName) unreached)
    other -> other

decisive :: Text -> RuleEvaluation -> Maybe Decision
decisive name = \case
    Decided (Allow reason) -> Just (Admitted name reason [])
    Decided (Deny etag reason) -> Just (Blocked name etag reason)
    Decided (NoDecision _) -> Nothing
    Decided (CannotVet FailDeny why) -> Just (Undecidable (WillResolve Nothing) (RuleUnable name why))
    Decided (CannotVet FailNoDecision _) -> Nothing
    Unavailable transience FailDeny why -> Just (Undecidable transience (RuleUnable name why))
    Unavailable _ FailNoDecision _ -> Nothing
