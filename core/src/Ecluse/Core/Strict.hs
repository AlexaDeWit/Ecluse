-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Evaluation for containers a long-lived result retains, so the result holds no deferred work.
module Ecluse.Core.Strict (strictElements) where

-- | Evaluate every element to weak head normal form when the container is evaluated.
strictElements :: (Foldable t) => t a -> t a
strictElements elements = foldr seq elements elements
