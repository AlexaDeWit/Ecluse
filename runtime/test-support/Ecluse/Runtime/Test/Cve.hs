-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | 'CveFetch' doubles for suites that drive the advisory sync without a network.

Each one throws 'TestContractEscape' from the arm the case asserts is never reached, so a
call the design forbids surfaces as a typed failure rather than a silent success.
-}
module Ecluse.Runtime.Test.Cve (
    headOnlyFetch,
    refusingFetch,
) where

import UnliftIO.Exception (throwIO)

import Ecluse.Core.Cve (DbEtag)
import Ecluse.Runtime.Cve.Sync (CveFetch (..), OsvDbFetchFault)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

{- | A fetch that answers the given HEAD result and refuses to download. Its cases decide on the
ETag alone, so reaching the download arm is a broken premise.
-}
headOnlyFetch :: Either OsvDbFetchFault (Maybe DbEtag) -> CveFetch
headOnlyFetch etagResult =
    CveFetch
        { fetchHeadEtag = pure etagResult
        , fetchDownload = \_ -> throwIO (TestContractEscape "must not download")
        }

-- | A fetch that refuses both arms, for a case that threads a handle but never syncs.
refusingFetch :: CveFetch
refusingFetch =
    CveFetch
        { fetchHeadEtag = throwIO (TestContractEscape "must not fetch")
        , fetchDownload = \_ -> throwIO (TestContractEscape "must not fetch")
        }
