-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot validation pass: one role-parameterised applicative that accumulates every refusal
and every advisory a configuration earns.

A rule names the condition it detects and its severity per role, and 'rule' is the only path from
a detected condition to an outcome, so a rule cannot be fatal on one boot path and missing on
another.
Advisories ride the success path, so a check that passes can still advise.
-}
module Ecluse.Composition.Vet (
    -- * The accumulating pass
    Vet,
    runVet,
    vetRole,
    decided,

    -- * Rules and their severity
    Severity (..),
    rule,
) where

import Validation (Validation (Failure, Success), validationToEither)

import Ecluse.Composition.BootError (BootError)
import Ecluse.Composition.Types (RegistryRole)

{- | A boot check awaiting its role: the advisories it logs, beside either every refusal it earned
or the value it vetted.
-}
newtype Vet a = Vet (RegistryRole -> ([Text], Validation [BootError] a))

instance Functor Vet where
    fmap f (Vet run) = Vet $ \role ->
        let (advisories, outcome) = run role
         in (advisories, fmap f outcome)

{- 'Vet' has no 'Monad' instance on purpose. A bind would let one check's failure hide the next
check's finding, which is the short-circuiting this pass exists to remove. '<*>' runs both sides
whatever either decides, so one boot reports everything an operator must fix. -}
instance Applicative Vet where
    pure a = Vet (const ([], Success a))
    Vet runF <*> Vet runA = Vet $ \role ->
        let (fAdvisories, f) = runF role
            (aAdvisories, a) = runA role
         in (fAdvisories <> aAdvisories, f <*> a)

-- | Run a pass for one role: every advisory it logs, and either every refusal or the vetted value.
runVet :: RegistryRole -> Vet a -> ([Text], Either [BootError] a)
runVet role (Vet run) = second validationToEither (run role)

{- | The role the pass runs for. A witness one role alone may hold is built from this, so no other
role's pass can issue one.
-}
vetRole :: Vet RegistryRole
vetRole = Vet $ \role -> ([], Success role)

{- | Carry an outcome a producer decided for itself into the pass, so its refusals join the rest.
'rule' cannot express one, because there is no condition left to detect.
-}
decided :: Either [BootError] a -> Vet a
decided outcome = Vet . const $ case outcome of
    Left errs -> ([], Failure errs)
    Right a -> ([], Success a)

-- | What one role does about a finding a rule detected.
data Severity finding
    = -- | Refuse to boot, reporting this refusal.
      Refuse (finding -> BootError)
    | -- | Boot, and log this advisory line.
      Advise (finding -> Text)

{- | One rule: its severity per role, the condition it detects in an input, and that input. Every
refusal and every advisory is built here, so the two cannot describe different rules.
-}
rule :: (RegistryRole -> Severity finding) -> (input -> Maybe finding) -> input -> Vet ()
rule severity detect input = Vet $ \role ->
    case (detect input, severity role) of
        (Nothing, _) -> ([], Success ())
        (Just finding, Refuse toRefusal) -> ([], Failure [toRefusal finding])
        (Just finding, Advise toAdvisory) -> ([toAdvisory finding], Success ())
