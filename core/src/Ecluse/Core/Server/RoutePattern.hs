-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The route-pattern engine: a mount's serve surface as an ordered list of
declarative patterns, matched by a generic engine to classify a request.

A 'RoutePattern' is a method condition, a sequence of path segments (literals and
named captures), and a builder that turns the captured values into a shared
"Ecluse.Core.Server.Route".'Route'. 'classifyWith' matches a request's method and
already-mount-stripped, percent-decoded segments against each pattern in order and
builds the 'Route' -- the parser the front door routes on, expressed as data.

Because a route's grammar is a value (its 'rpSegs') rather than hand-written control
flow, the /same/ description can later be rendered as an OpenAPI path template for the
capability manifest, so the documented grammar cannot drift from the routed one. This
module supplies the forward (classify) direction; the render direction lands with the
manifest that consumes it.

The engine is generic over a registry's capture-value type @v@ (npm supplies its
own), carries no ecosystem knowledge itself, and is pure, so the whole routing table
is unit-testable with no server.

== What stays a named function

The engine owns the __structure__ (literal matching, capture arity, ordering, full
consumption). It does not infer a registry's __semantics__: a 'Capture' carries its
own segment parser and a pattern its own builder, so the security-critical leaf logic
(component-safety gates, an ecosystem's scoped-name decoding, a version parse) stays as
named, reviewed functions the pattern references, rather than being regenerated from a
generic template.
-}
module Ecluse.Core.Server.RoutePattern (
    -- * Patterns
    RoutePattern (..),
    PatternSeg (..),
    Capture (..),
    MethodMatch (..),

    -- * Classification
    classifyWith,
) where

import Network.HTTP.Types.Method (Method, methodPut)

import Ecluse.Core.Server.Route (Classifier, Route (Unsupported))

{- | One route as data: which method it answers, its mount-relative path template,
and how the captured values become a 'Route'. Generic over the registry's
capture-value type @v@.
-}
data RoutePattern v = RoutePattern
    { rpMethod :: MethodMatch
    -- ^ The method condition a request must satisfy to match this pattern.
    , rpSegs :: [PatternSeg v]
    -- ^ The mount-relative path template: literal segments and named captures, in order.
    , rpBuild :: [v] -> Maybe Route
    {- ^ Assemble the captured values (one per 'SegCap', in template order) into the
    route's 'Route'. This is where a __cross-capture__ check lives (e.g. an artifact
    file name that must be consistent with the package captured earlier); a 'Nothing'
    denies the request (deny by default). Total over the captures the pattern produces.
    -}
    }

{- | One segment of a path template: a fixed segment matched verbatim, or a named
capture that consumes one or more leading segments and yields a value.
-}
data PatternSeg v
    = SegLit Text
    | SegCap (Capture v)

{- | A named path capture: how it parses (the security-critical leaf) and how it
documents. 'capConsume' may consume __more than one__ segment (an ecosystem whose
identifier spans a decoded @\'\/\'@ needs this), and returns the unconsumed tail, so
captures thread left to right; 'Nothing' fails the match (the request falls through
to the next pattern, or to the deny-by-default catch-all).
-}
data Capture v = Capture
    { capName :: Text
    -- ^ The capture name, as it appears in the template (@{package}@).
    , capDescription :: Text
    -- ^ A one-line, human-facing description for the documentation.
    , capConsume :: [Text] -> Maybe (v, [Text])
    -- ^ Consume the leading segments this capture claims, yielding its value and the tail.
    }

{- | The method condition on a route. 'MethodRead' matches every method that is not a
write, mirroring the front door's existing rule (a @PUT@ is the one client write; a
@HEAD@, @GET@, and any other method read). Kept as a small closed vocabulary rather
than a bare predicate so a renderer can still name the documented method.
-}
data MethodMatch
    = -- | The write method (@PUT@): the publish request.
      MethodPut
    | -- | Any non-write method: the read grammar.
      MethodRead
    deriving stock (Eq, Show)

-- | Whether a request method satisfies a pattern's 'MethodMatch'.
methodMatches :: MethodMatch -> Method -> Bool
methodMatches MethodPut m = m == methodPut
methodMatches MethodRead m = m /= methodPut

{- | Classify a request against an ordered pattern list: the first pattern whose
method and segments match builds the 'Route'; if none matches, the request is
'Unsupported' (deny by default). This is a 'Classifier'.
-}
classifyWith :: [RoutePattern v] -> Classifier
classifyWith pats method segs =
    fromMaybe Unsupported (listToMaybe (mapMaybe (matchPattern method segs) pats))

{- Match one request against one pattern: the method must satisfy the pattern's
condition, the segments must be consumed __exactly__ (no trailing segments), and the
builder must accept the captures. 'Nothing' when the pattern does not claim this request. -}
matchPattern :: Method -> [Text] -> RoutePattern v -> Maybe Route
matchPattern method segs rp
    | methodMatches (rpMethod rp) method = consumeSegs (rpSegs rp) segs >>= rpBuild rp
    | otherwise = Nothing

{- Run a pattern's segments against a request's segments, collecting one value per
capture in template order. Requires __exact__ consumption: a leftover request segment,
or a pattern segment with nothing to match, fails. A 'SegCap' may consume more than one
segment (its 'capConsume' decides) and threads the remainder to the rest of the pattern. -}
consumeSegs :: [PatternSeg v] -> [Text] -> Maybe [v]
consumeSegs [] [] = Just []
consumeSegs (SegLit l : ps) (s : ss)
    | l == s = consumeSegs ps ss
consumeSegs (SegCap c : ps) ss = do
    (v, rest) <- capConsume c ss
    (v :) <$> consumeSegs ps rest
consumeSegs _ _ = Nothing
