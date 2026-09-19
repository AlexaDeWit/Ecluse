-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Fixtures the rules-engine specs share: the fixed instant every age and cooldown
calculation is taken against, the evaluation context at it, the package version under test,
and the advisory push-age maximum a seven-day quarantine derives.
-}
module Ecluse.Rules.Support (
    now,
    ctx,
    ctxAt,
    pkg,
    sixDayLimit,
) where

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageDetails (pkgLicenses, pkgPublishedAt), mkPackageName, mkScope)
import Ecluse.Core.Rules.Freshness (MaxAdvisoryAge, maxAdvisoryAgeFor)
import Ecluse.Core.Rules.Types (
    EvalContext (EvalContext),
    Rule (AllowIfOlderThan),
    RuleEvidence,
    completeEvidence,
 )
import Ecluse.Test.Package (sampleDetails, v1_0_0)

-- | A fixed "now", so age and cooldown arithmetic stay deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

{- | An 'EvalContext' at a given instant, the request snapshot the age rules read. A breaker
ignores it and takes its clock from its own injected reading instead.
-}
ctxAt :: UTCTime -> EvalContext
ctxAt t = EvalContext t Nothing

ctx :: EvalContext
ctx = ctxAt now

{- | A package version under an optional npm scope, published @ageDays@ days before 'now'.
The rules under test read only the scope, the publish age, and the install-code signal.
-}
pkg :: Maybe Text -> Integer -> RuleEvidence
pkg mScope ageDays = completeEvidence details
  where
    details =
        (sampleDetails (mkPackageName Npm (mkScope <$> mScope) "thing") v1_0_0)
            { pkgPublishedAt = Just (addUTCTime (negate (fromInteger ageDays * nominalDay)) now)
            , pkgLicenses = ["MIT"]
            }

{- | The maximum a mount deriving from a seven-day quarantine gets: six days. Push-age
readings are taken against it, so the boundary cases read as an operator's would.
-}
sixDayLimit :: MaxAdvisoryAge
sixDayLimit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (7 * nominalDay)]
