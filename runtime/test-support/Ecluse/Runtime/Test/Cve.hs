-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory transport doubles. Unexpected calls throw 'TestContractEscape'.
module Ecluse.Runtime.Test.Cve (
    headOnlyFetch,
    refusingFetch,
) where

import UnliftIO.Exception (throwIO)

import Ecluse.Runtime.Cve.Sync (CveFetch (..), FetchedObject, OsvDbFetchFault)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | A HEAD response whose download arm throws if the test unexpectedly reaches it.
headOnlyFetch :: Either OsvDbFetchFault (Maybe FetchedObject) -> CveFetch
headOnlyFetch headResult =
    CveFetch
        { fetchHead = pure headResult
        , fetchDownload = \_ -> throwIO (TestContractEscape "must not download")
        }

-- | A fetch that refuses both arms, for a case that threads a handle but never syncs.
refusingFetch :: CveFetch
refusingFetch =
    CveFetch
        { fetchHead = throwIO (TestContractEscape "must not fetch")
        , fetchDownload = \_ -> throwIO (TestContractEscape "must not fetch")
        }
