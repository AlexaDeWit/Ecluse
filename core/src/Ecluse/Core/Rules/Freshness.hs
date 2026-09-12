-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | How old the advisory push behind a CVE-based deny may be. The clock is the published object's
own timestamp, which "Ecluse.Core.Cve.Slot" carries, so a recompile of unchanged bytes still moves
it and a restart does not reset it. What a source says about its own data is diagnostic and is
never read here. "Ecluse.Core.Rules" applies the reading to a prepared rule.
-}
module Ecluse.Core.Rules.Freshness (
    -- * The effective maximum
    MaxAdvisoryAge (..),
    AdvisoryAgeBasis (..),
    maxAdvisoryAgeFor,
    advisoryAgeFloor,
    advisoryAgeLead,

    -- * Reading one push
    AdvisoryPublication (..),
    AdvisoryAge (..),
    AdvisoryFreshness (..),
    assessAdvisoryAge,
    ageAlarmStep,
) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))

{- | The maximum push age one mount's CVE-based denies accept, and where the value came from.
The basis is carried so the boot log can report it beside the number.
-}
data MaxAdvisoryAge = MaxAdvisoryAge
    { maxAdvisoryAge :: NominalDiffTime
    -- ^ The effective limit. A push older than this expires.
    , maxAdvisoryAgeBasis :: AdvisoryAgeBasis
    -- ^ Which of the three ways below produced it.
    }
    deriving stock (Eq, Show)

-- | Where an effective maximum came from.
data AdvisoryAgeBasis
    = -- | The operator set @advisories.maxAgeSeconds@, which overrides every derivation.
      AgeConfigured
    | {- | Derived to land 'advisoryAgeLead' ahead of the earliest quarantine admission this
      mount's own rules allow (carried), so the failure shows before that cohort is admitted.
      -}
      AgeBeforeQuarantine NominalDiffTime
    | -- | The floor, which no derivation goes below.
      AgeFloor
    deriving stock (Eq, Show)

-- | The shortest maximum a derivation yields: three days.
advisoryAgeFloor :: NominalDiffTime
advisoryAgeFloor = 259200

-- | How far ahead of the earliest quarantine admission a derived maximum lands: 24 hours.
advisoryAgeLead :: NominalDiffTime
advisoryAgeLead = 86400

{- | One mount's effective maximum. An explicit value is final, above and below the derivation.
One mount's rules are read alone, so another ecosystem's quarantine cannot set this limit.
-}
maxAdvisoryAgeFor :: Maybe NominalDiffTime -> [Rule] -> MaxAdvisoryAge
maxAdvisoryAgeFor (Just explicit) _ = MaxAdvisoryAge explicit AgeConfigured
maxAdvisoryAgeFor Nothing rules = maybe floorAge derivedFrom (foldr earlier Nothing rules)
  where
    earlier (AllowIfOlderThan quarantine) soFar = Just (maybe quarantine (min quarantine) soFar)
    earlier _ soFar = soFar

    derivedFrom quarantine
        | quarantine - advisoryAgeLead > advisoryAgeFloor =
            MaxAdvisoryAge (quarantine - advisoryAgeLead) (AgeBeforeQuarantine quarantine)
        | otherwise = floorAge

    floorAge = MaxAdvisoryAge advisoryAgeFloor AgeFloor

{- | One reading of a push: when it landed, how old it is now, and the maximum it was read
against. An audit line and an alarm both render this, so neither can report a different number.
-}
data AdvisoryAge = AdvisoryAge
    { advisoryPushedAt :: UTCTime
    , advisoryAge :: NominalDiffTime
    , advisoryMaxAge :: NominalDiffTime
    }
    deriving stock (Eq, Show)

{- | What a slot says about the serving artifact's publication. The undated case is separate
because a generation whose age cannot be established is not the same as none serving at all.
-}
data AdvisoryPublication
    = -- | Nothing is serving yet, so there is no artifact to age.
      NoGeneration
    | -- | The published object's own timestamp.
      PublishedAt UTCTime
    | -- | A generation is serving and the store reported no publication time for it.
      UndatedGeneration
    deriving stock (Eq, Show)

{- | What a push permits. 'AdvisoryAging' is still eligible: it is the early warning, raised at
half the maximum so an update outage surfaces while there is still time to act on it.
-}
data AdvisoryFreshness
    = -- | Within half the maximum, or nothing serving to age.
      AdvisoryFresh
    | -- | Past half the maximum and still eligible.
      AdvisoryAging AdvisoryAge
    | -- | Past the maximum. CVE-based denial refuses, whatever its @onUnavailable@ says.
      AdvisoryStale AdvisoryAge
    | {- | A serving generation whose age cannot be established, which is unverified evidence
      and refuses on the same terms as an expired one.
      -}
      AdvisoryUndated
    deriving stock (Eq, Show)

{- | Read one publication against a maximum: equal to it is eligible, and greater expires. Nothing
serving is not aged here, leaving the ordinary absent-database path to decide.
-}
assessAdvisoryAge :: MaxAdvisoryAge -> UTCTime -> AdvisoryPublication -> AdvisoryFreshness
assessAdvisoryAge limit now = \case
    NoGeneration -> AdvisoryFresh
    UndatedGeneration -> AdvisoryUndated
    PublishedAt pushedAt -> reading pushedAt
  where
    reading pushedAt
        | age > maxAge = AdvisoryStale observed
        -- Doubling the age keeps the halfway test off a division that could round either way.
        | age + age > maxAge = AdvisoryAging observed
        | otherwise = AdvisoryFresh
      where
        age = diffUTCTime now pushedAt
        maxAge = maxAdvisoryAge limit
        observed = AdvisoryAge{advisoryPushedAt = pushedAt, advisoryAge = age, advisoryMaxAge = maxAge}

{- | The early warning's next latch state, and the reading to report where this one crosses. An
undated generation has no age to report and raises its own alarm where the artifact lands.
-}
ageAlarmStep :: Bool -> AdvisoryFreshness -> (Bool, Maybe AdvisoryAge)
ageAlarmStep latched = \case
    AdvisoryFresh -> (False, Nothing)
    AdvisoryUndated -> (False, Nothing)
    AdvisoryAging observed -> crossing observed
    AdvisoryStale observed -> crossing observed
  where
    crossing observed = (True, observed <$ guard (not latched))
