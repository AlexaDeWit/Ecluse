-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native measurement evidence stays separate from Hspec execution reports.
module Ecluse.Test.Contract.Performance (
    NativeOutcome (..),
    PerformanceObservation (..),
    NativePerformance (..),
    PerformanceObligation (..),
    PerformanceReceipt (..),
    auditPerformanceReceipts,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Char (isHexDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Test.Contract.Catalogue qualified as Catalogue
import Ecluse.Test.Contract.Result (ExecutionIdentity (..), auditExactIds)

-- | Availability and native verdicts do not substitute for each other.
data NativeOutcome = NativePassed | NativeFailed | NativeUnavailable
    deriving stock (Eq, Show)

instance ToJSON NativeOutcome where
    toJSON =
        toJSON . \case
            NativePassed -> ("passed" :: Text)
            NativeFailed -> "failed"
            NativeUnavailable -> "unavailable"

instance FromJSON NativeOutcome where
    parseJSON = withText "native performance outcome" $ \case
        "passed" -> pure NativePassed
        "failed" -> pure NativeFailed
        "unavailable" -> pure NativeUnavailable
        _ -> fail "unknown native performance outcome"

-- | An actual row or pass, with the harness's own verdict.
data PerformanceObservation = PerformanceObservation
    { observationId :: Text
    , observationOutcome :: NativeOutcome
    }
    deriving stock (Eq, Show)

instance ToJSON PerformanceObservation where
    toJSON observation = object ["id" .= observationId observation, "outcome" .= observationOutcome observation]
instance FromJSON PerformanceObservation where
    parseJSON = withObject "performance observation" $ \o -> PerformanceObservation <$> o .: "id" <*> o .: "outcome"

-- | The harness emits these facts only after measured operations finish.
data NativePerformance = NativePerformance
    { npOutcome :: NativeOutcome
    , npObservations :: [PerformanceObservation]
    , npConditions :: Map Text Text
    }
    deriving stock (Eq, Show)

instance ToJSON NativePerformance where
    toJSON native = object ["outcome" .= npOutcome native, "observations" .= npObservations native, "conditions" .= npConditions native]
instance FromJSON NativePerformance where
    parseJSON = withObject "native performance" $ \o -> NativePerformance <$> o .: "outcome" <*> o .: "observations" <*> o .: "conditions"

-- | Reviewed laws and corpus identities define requirements independently of measured rows.
data PerformanceObligation = PerformanceObligation
    { poKind :: Text
    , poSubject :: Text
    , poRows :: [Text]
    , poConditions :: Map Text Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON PerformanceObligation
instance FromJSON PerformanceObligation

-- | Artifacts are hashed after measurement and checked again when completion consumes them.
data PerformanceReceipt = PerformanceReceipt
    { prSchemaVersion :: Int
    , prIdentity :: ExecutionIdentity
    , prAttempt :: Text
    , prKind :: Text
    , prSubject :: Text
    , prNative :: NativePerformance
    , prArtifacts :: Map FilePath Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON PerformanceReceipt
instance FromJSON PerformanceReceipt

-- | Require every native obligation with matching execution identities, conditions, and artifact digests.
auditPerformanceReceipts :: ExecutionIdentity -> Text -> [PerformanceObligation] -> [PerformanceReceipt] -> [Text]
auditPerformanceReceipts expected attempt obligations receipts =
    ["empty performance obligations" | null obligations]
        <> ["empty performance attempt" | T.null attempt]
        <> ["empty performance identity" | any T.null identityValues]
        <> auditExactIds "receipt" (map obligationKey obligations) (map receiptKey receipts)
        <> duplicates "required receipt" (map obligationKey obligations)
        <> concatMap obligationProblems obligations
        <> concatMap receiptProblems receipts
  where
    identityValues = [identitySource expected, identityImage expected, identityArchitecture expected, identityPlanDigest expected, identityRunId expected]
    obligationKey obligation = poKind obligation <> "/" <> poSubject obligation
    receiptKey receipt = prKind receipt <> "/" <> prSubject receipt
    obligationProblems obligation =
        ["empty performance obligation: " <> obligationKey obligation | any T.null [poKind obligation, poSubject obligation] || null (poRows obligation) || Map.null (poConditions obligation)]
            <> ["empty required measurement ID" | any T.null (poRows obligation)]
            <> duplicates "required measurement" (poRows obligation)
            <> ["empty required condition: " <> key | (key, value) <- Map.toList (poConditions obligation), T.null key || T.null value]
    receiptProblems receipt =
        ["unsupported performance receipt schema" | prSchemaVersion receipt /= 1]
            <> ["performance identity mismatch: " <> receiptKey receipt | prIdentity receipt /= expected]
            <> ["performance attempt mismatch: " <> receiptKey receipt | prAttempt receipt /= attempt]
            <> ["native performance did not pass: " <> receiptKey receipt | npOutcome (prNative receipt) /= NativePassed]
            <> ["performance artifacts missing: " <> receiptKey receipt | Map.null (prArtifacts receipt)]
            <> ["invalid performance artifact digest: " <> toText path | (path, value) <- Map.toList (prArtifacts receipt), null path || T.length value /= 64 || not (T.all isHexDigit value)]
            <> concatMap (observationsProblems receipt) (filter ((== receiptKey receipt) . obligationKey) obligations)
    observationsProblems receipt obligation =
        ["missing measurement: " <> row | row <- poRows obligation, row `notElem` map observationId observations]
            <> ["empty measurement ID" | any (T.null . observationId) observations]
            <> duplicates "measurement" (map observationId observations)
            <> ["measurement did not pass: " <> observationId observation | observation <- observations, observationOutcome observation /= NativePassed]
            <> ["measurement condition mismatch: " <> key | (key, value) <- Map.toList (poConditions obligation), Map.lookup key (npConditions native) /= Just value]
      where
        native = prNative receipt
        observations = npObservations native

duplicates :: Text -> [Text] -> [Text]
duplicates kind values = ["duplicate " <> kind <> ": " <> value | value <- Catalogue.duplicates values]
