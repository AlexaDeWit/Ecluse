-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Fixtures the outage fold and the reporter over it share: the reminder gap, the two
readings an advisory rule can report, an admission identity, and the state's observable shape.
-}
module Ecluse.Rules.Outage.Support (
    period,
    down,
    up,
    ident,
    shape,
    isHealthy,
) where

import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Rules.Outage.Internal (
    AdmissionIdentity (AdmissionIdentity),
    LoggedAdmissions,
    OngoingOutage (ooLogged, ooReportedAt, ooRules, ooSince),
    OutageState (Healthy, Outage),
    SourceHealth (SourceAnswered, SourceUnavailable),
 )
import Ecluse.Core.Rules.Types (Reason)

-- | The reminder gap under test, in seconds.
period :: (Num a) => a
period = 900

-- | The named rule could not reach its source, for want of a database.
down :: Text -> SourceHealth
down rule = SourceUnavailable rule "no advisory database loaded"

-- | The named rule consulted its source.
up :: Text -> SourceHealth
up = SourceAnswered

-- | One admission's identity: the package, the version, and the rules it skipped.
ident :: Text -> Text -> [Text] -> AdmissionIdentity
ident package version rules = AdmissionIdentity package version (Set.fromList rules)

{- | The state's observable shape: nothing for a healthy source, else when the outage began, when
it last reported, the rules still unable, and the identities logged in order.
-}
shape :: OutageState -> Maybe (UTCTime, UTCTime, Map Text Reason, LoggedAdmissions)
shape = \case
    Healthy -> Nothing
    Outage ongoing -> Just (ooSince ongoing, ooReportedAt ongoing, ooRules ongoing, ooLogged ongoing)

isHealthy :: OutageState -> Bool
isHealthy = isNothing . shape
