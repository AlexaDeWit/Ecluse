-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | An in-memory 'CveLookup' for pure-tier tests.

Rule-evaluation specs in the core suite use this fake instead of SQLite. The
app-tier conformance spec runs the same behavioural cases against this fake
and the real handle, so the two cannot drift apart.
-}
module Ecluse.Test.Cve (
    fakeCveLookup,
    fakeCveDb,
    unscoredEpssCases,
) where

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveLookup (..))
import Ecluse.Core.Osv.Epss (epssForIds, mkEpssScores, parseEpssLine)
import Ecluse.Core.Osv.Provenance (noProvenance)
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore))

{- | Build the fake from (package name, range) rows. The remediation probe is exact string equality
on the fixed bound, matching the artifact's verbatim version text.
-}
fakeCveLookup :: [(Text, AdvisoryRange)] -> CveLookup
fakeCveLookup rows =
    CveLookup
        { cveRemediationProbe = \name version ->
            pure (any (\(n, ar) -> n == name && arUpperBound ar == FixedBefore version) rows)
        , cveAdvisoriesFor = \name -> pure [ar | (n, ar) <- rows, n == name]
        , cveCoveredNames = pure (ordNub (map fst rows))
        }

{- | An owning handle over the fake lookup, closing to nothing. A spec that pins when the slot
retires a displaced generation builds its own recording handle instead.
-}
fakeCveDb :: [(Text, AdvisoryRange)] -> CveDb
fakeCveDb rows = CveDb{cveDbLookup = fakeCveLookup rows, cveDbClose = pass, cveDbMeta = [], cveDbProvenance = noProvenance}

-- | Individual gaps in a nonempty feed, joined through the production score parser.
unscoredEpssCases :: [(String, Maybe Double)]
unscoredEpssCases =
    [ ("malware without a CVE alias", score ["MAL-2026-1"] [])
    , ("a CVE absent from a valid feed", score ["CVE-2026-10001"] [])
    , ("a malformed score row in an accepted feed", score ["CVE-2026-10001"] ["CVE-2026-10001,not-a-number,0.5"])
    ]
  where
    score ids rows =
        epssForIds
            (mkEpssScores (mapMaybe parseEpssLine ("CVE-2026-10002,0.75,0.9" : rows)))
            ids
