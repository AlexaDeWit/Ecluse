-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Reading back which fault a registry leg returned.

Every leg reports its fault as a value, so a case that decides on the arm and not the payload
asks one of these. They are polymorphic in the success side, so the fetch predicates serve a
bounded read, a projection, and a publish's own fetch alike.
-}
module Ecluse.Test.Registry (
    isUrlUnformableFetch,
    isTransportFetch,
    isBoundExceededFetch,
    isUrlUnformablePublish,
    isBoundExceededPublish,
) where

import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    PublishFault (PublishFetch),
 )

-- | Which arm a fetch fault took.
isUrlUnformableFetch, isTransportFetch, isBoundExceededFetch :: Either FetchFault a -> Bool
isUrlUnformableFetch = \case
    Left (FetchUrlUnformable _) -> True
    _ -> False
isTransportFetch = \case
    Left (FetchTransport _) -> True
    _ -> False
isBoundExceededFetch = \case
    Left (FetchBoundExceeded _) -> True
    _ -> False

-- | Which arm of its own fetch a publish fault carried.
isUrlUnformablePublish, isBoundExceededPublish :: Either PublishFault a -> Bool
isUrlUnformablePublish = \case
    Left (PublishFetch (FetchUrlUnformable _)) -> True
    _ -> False
isBoundExceededPublish = \case
    Left (PublishFetch (FetchBoundExceeded _)) -> True
    _ -> False
