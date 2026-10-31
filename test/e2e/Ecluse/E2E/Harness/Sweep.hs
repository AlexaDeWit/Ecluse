-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native providers share policy rendering and observations of product sweep logs.
module Ecluse.E2E.Harness.Sweep (
    sweepEnvUnder,
    identityRule,
    identityRules,
    admissionOnlyEntry,
    advisoryRules,
    permissiveRules,
    denyingRules,
    renderRules,
    observeRun,
    policyEnvironment,
) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text qualified as T
import Ecluse.E2E.Harness.Advisories (advisoryStoreEnv)
import Ecluse.E2E.Harness.Docker (RoleRun (..))
import Ecluse.Test.Contract.Sweep.Types
import Ecluse.Test.Log (lineMessage)

-- | Request a complete inventory walk under the supplied rules.
sweepEnvUnder :: Text -> [(Text, Text)]
sweepEnvUnder rules =
    [ ("ECLUSE_RULES", rules)
    , ("ECLUSE_DREDGER__FULL_WALK", "true")
    ]

-- | Render one explicit identity denial.
identityRule :: Text -> Text
identityRule name = identityRules [name]

-- | Render named identity denials with stable order.
identityRules :: [Text] -> Text
identityRules = renderRules . zipWith identityEntry [1 :: Int ..]

identityEntry :: Int -> Text -> Pair
identityEntry position revoked =
    fromString ("revoke-" <> show position) .= object ["type" .= ("DenyByIdentity" :: Text), "identity" .= revoked]

-- | Restrict the identity denial to admission.
admissionOnlyEntry :: Text -> Pair
admissionOnlyEntry revoked =
    "revoke-at-admission" .= object ["type" .= ("DenyByIdentity" :: Text), "identity" .= revoked, "appliesTo" .= (["admission"] :: [Text])]

-- | Permit old releases while denying scored advisories.
advisoryRules :: Text
advisoryRules =
    renderRules
        [ "min-age" .= object ["type" .= ("AllowIfOlderThan" :: Text), "ageSeconds" .= (0 :: Int)]
        , "deny-cve" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]
        ]

-- | The zero-age rule admits fixture releases.
minAgeRule :: Pair
minAgeRule = "min-age" .= object ["type" .= ("AllowIfOlderThan" :: Text), "ageSeconds" .= (0 :: Int)]

-- | Render the shared permissive fixture policy.
permissiveRules :: Text
permissiveRules = renderRules [minAgeRule]

-- | Add explicit denials after the permissive age rule.
denyingRules :: [Text] -> Text
denyingRules denied = renderRules (minAgeRule : zipWith identityEntry [1 :: Int ..] denied)

-- | Encode rules through the configuration wire format.
renderRules :: [Pair] -> Text
renderRules = decodeUtf8 . toStrict . encode . object

auditMessages :: RoleRun -> [Text]
auditMessages run = mapMaybe lineMessage (T.lines (roleOutput run))

sweepMessages :: RoleRun -> [Text]
sweepMessages run = filter isSweepMessage (map withoutTarget (auditMessages run))

isSweepMessage :: Text -> Bool
isSweepMessage message = any (`T.isPrefixOf` message) ["deleting ", "dry run, would delete ", "mirror sweep cycle "]

withoutTarget :: Text -> Text
withoutTarget message
    | any (`T.isPrefixOf` message) ["mirrorTarget ", "privateUpstream "] = T.drop 2 (snd (T.breakOn ": " message))
    | otherwise = message

-- | Render canonical sweep policies without choosing a native backend.
policyEnvironment :: SweepPolicy -> [(Text, Text)]
policyEnvironment = \case
    FullIdentity name -> sweepEnvUnder (identityRule name)
    AdmissionOnly name -> sweepEnvUnder (renderRules [admissionOnlyEntry name])
    IdentityPolicy names -> [("ECLUSE_RULES", identityRules names)]
    PermissivePolicy -> [("ECLUSE_RULES", permissiveRules)]
    AdvisoryPolicy -> advisoryStoreEnv <> [("ECLUSE_RULES", advisoryRules)]

-- | Retain the process outcome and its unmodified audit messages.
observeRun :: RoleRun -> SweepRun
observeRun run = SweepRun (roleExit run) (roleOutput run) (auditMessages run) (sweepMessages run)
