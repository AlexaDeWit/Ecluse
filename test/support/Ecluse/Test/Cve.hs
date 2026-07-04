{- | An in-memory 'CveLookup' for pure-tier tests.

Rule-evaluation specs in the core suite use this fake instead of SQLite; the
app-tier conformance spec runs the same behavioural cases against this fake
and the real handle, so the two cannot drift apart.
-}
module Ecluse.Test.Cve (
    fakeCveLookup,
) where

import Ecluse.Core.Cve (AdvisoryRange (..), CveLookup (..))

{- | Build the fake from (package name, range) rows, mirroring the artifact's
vocabulary: OSV wire names, verbatim version text, and the probe as exact
string equality on the fixed bound.
-}
fakeCveLookup :: [(Text, AdvisoryRange)] -> CveLookup
fakeCveLookup rows =
    CveLookup
        { cveRemediationProbe = \name version ->
            pure (any (\(n, ar) -> n == name && arFixed ar == Just version) rows)
        , cveAdvisoriesFor = \name -> pure [ar | (n, ar) <- rows, n == name]
        }
