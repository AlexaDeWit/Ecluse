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
frozenVerdict rule = mapPackageRead (\packageRead -> packageRead{prVerdict = verdictAsItStood rule (prVerdict packageRead)})

-- A version no row fixed abstained through its own probe. The remediation verdict's other paths are the live ones.
verdictAsItStood :: Rule -> (AdvisoryRows -> RuleEvidence -> RuleVerdict) -> AdvisoryRows -> RuleEvidence -> RuleVerdict
verdictAsItStood rule current rows = case (rule, rows) of
    (DenyIfKnownMalicious _, Just (etag, advisories)) -> affectedVerdict etag KnownMalicious NoKnownMaliciousAdvisory (keepAdvisories isMalicious advisories)
    (DenyIfCve params, Just (etag, advisories)) -> advisoryDenyVerdict etag DenyMissingScore Cvss (dicMinCvss params) arSeverity advisories
    (DenyIfEpss params, Just (etag, advisories)) -> advisoryDenyVerdict etag AbstainMissingScore Epss (dieMinEpss params) arEpss advisories
    (AllowIfRemediatesCve _, Just (_, advisories)) -> \ev ->
        if null (fixedAt advisories (evVersion ev)) then NoDecision FixesNoAdvisory else current rows ev
    _ -> current rows

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
