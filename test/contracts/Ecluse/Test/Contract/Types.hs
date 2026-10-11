-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

-- | Pure metadata for fixed suites and execution audits.
module Ecluse.Test.Contract.Types (
    Tier (..),
    Support (..),
    Profile (..),
    Obligation (..),
    Case (..),
    Group (..),
    Catalogue (..),
) where

import Data.Aeson (FromJSON, ToJSON)

-- | The collaborator and measurement boundary of a registration.
data Tier = Unit | Integration | E2E | Smoke | Residency | Allocation | Load | Benchmark
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Unsupported and planned coverage carries the reason or delivery reference.
data Support = Supported | Unsupported Text | Planned Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Capabilities describe the composed provider, independently of its installed prerequisites.
data Profile = Profile
    { profileId :: Text
    , profileEcosystems :: [Text]
    , profileBackends :: [Text]
    , profileCapabilities :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Reviewed requirements must be declared independently of concrete case registrations.
data Obligation = Obligation
    { obligationId :: Text
    , obligationProfile :: Text
    , obligationContract :: Text
    , obligationSupport :: Support
    , obligationCapabilities :: [Text]
    , obligationCases :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A case is one Hspec example, including all assertions that example owns.
data Case = Case
    { caseId :: Text
    , caseRequirements :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Scheduling preserves the complete resource lifecycle of each group.
data Group = Group
    { groupId :: Text
    , groupContractId :: Text
    , groupProfileId :: Text
    , groupTier :: Tier
    , groupPartition :: Text
    , groupCases :: [Case]
    , groupCapabilities :: [Text]
    , groupPrerequisites :: [Text]
    , groupResources :: [Text]
    , groupIsolation :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Production ecosystem names come from product registration, not test profiles.
data Catalogue = Catalogue
    { catalogueProductionEcosystems :: [Text]
    , catalogueProfiles :: [Profile]
    , catalogueObligations :: [Obligation]
    , catalogueGroups :: [Group]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
