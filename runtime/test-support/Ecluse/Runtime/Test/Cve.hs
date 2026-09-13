-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory transport doubles. Unexpected calls throw 'TestContractEscape'.
module Ecluse.Runtime.Test.Cve (
    headOnlyFetch,
    fetchServing,
    fetchServingAt,
    refusingFetch,
) where

import Data.Time (UTCTime)
import UnliftIO.Exception (throwIO)

import Ecluse.Runtime.Cve.Sync (CveFetch (..), DbEtag (DbEtag), FetchedObject (FetchedObject), OsvDbFetchFault)
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

-- | An object whose download writes the supplied artifact fixture.
fetchServing :: Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServing = fetchServingAt Nothing

-- | 'fetchServing' with the publication time the store reports for the object.
fetchServingAt :: Maybe UTCTime -> Maybe Text -> (FilePath -> IO ()) -> CveFetch
fetchServingAt pushedAt mEtag write =
    CveFetch
        { fetchHead = pure (Right ((\etag -> FetchedObject (DbEtag etag) pushedAt) <$> mEtag))
        , fetchDownload = \dest -> case mEtag of
            Nothing -> throwIO (TestContractEscape "download called with no object present")
            Just etag -> write dest $> Right (FetchedObject (DbEtag etag) pushedAt)
        }
