-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- This spec writes the applicative laws out in full (@pure f <*> v@) to /assert/ them.
-- hlint would otherwise "simplify" the exact expressions under test. The silence is
-- file-wide because proving the laws is the file's purpose, not an oversight.
{- HLINT ignore "Use <$>" -}

module Ecluse.Composition.VetSpec (spec) where

import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Composition.BootError (BootError (QueueUrlUnrecognised))
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (
    Severity (Advise, Refuse),
    Vet,
    rule,
    runVet,
 )

spec :: Spec
spec = do
    lawSpec
    accumulationSpec

{- The four applicative laws. A hand-rolled instance can break any of them, and the accumulation
every boot report depends on is only total while they hold. -}
lawSpec :: Spec
lawSpec = describe "the applicative laws" $ do
    it "identity: pure id <*> v is v" $
        hedgehog $ do
            v <- forAll genProbe
            observe (pure id <*> probeVet v) === observe (probeVet v)

    it "composition: pure (.) <*> u <*> v <*> w is u <*> (v <*> w)" $
        hedgehog $ do
            u <- forAll genProbe
            v <- forAll genProbe
            w <- forAll genProbe
            observe (pure (.) <*> probeVetFn u <*> probeVetFn v <*> probeVet w)
                === observe (probeVetFn u <*> (probeVetFn v <*> probeVet w))

    it "homomorphism: pure f <*> pure x is pure (f x)" $
        hedgehog $ do
            x <- forAll genValue
            observe (pure (+ 1) <*> pure x) === observe (pure (x + 1))

    it "interchange: u <*> pure y is pure ($ y) <*> u" $
        hedgehog $ do
            u <- forAll genProbe
            y <- forAll genValue
            observe (probeVetFn u <*> pure y) === observe (pure ($ y) <*> probeVetFn u)

accumulationSpec :: Spec
accumulationSpec = describe "accumulation" $ do
    it "reports the refusals of both sides of an application, in order" $
        observe (probeVetFn (Probe Refused 1) <*> probeVet (Probe Refused 2))
            `shouldBe` [ ([], Left [QueueUrlUnrecognised "refused", QueueUrlUnrecognised "refused"])
                       , ([], Left [QueueUrlUnrecognised "refused", QueueUrlUnrecognised "refused"])
                       ]

    it "keeps the advisories of a refused pass, so a refusal never hides an advisory" $
        observe (probeVetFn (Probe Advised 1) <*> probeVet (Probe Refused 2))
            `shouldBe` [ (["advised"], Left [QueueUrlUnrecognised "refused"])
                       , (["advised"], Left [QueueUrlUnrecognised "refused"])
                       ]

-- A vet is a function of its role, so the laws compare what it yields under every role.
observe :: Vet a -> [([Text], Either [BootError] a)]
observe v = [runVet role v | role <- [MirrorWriter, MirrorPruner]]

-- The finding a probe carries. 'RoleSplit' is what exercises the role reader.
data Finding = Undetected | Refused | Advised | RoleSplit
    deriving stock (Eq, Show, Bounded, Enum)

-- A printable stand-in for a vet: one finding, and the value the vet yields.
data Probe = Probe Finding Int
    deriving stock (Eq, Show)

genProbe :: Gen Probe
genProbe = Probe <$> Gen.enumBounded <*> genValue

genValue :: Gen Int
genValue = Gen.int (Range.linear 0 9)

probeVet :: Probe -> Vet Int
probeVet (Probe finding n) = n <$ findingVet finding

-- The same probe as a function-valued vet, which the composition and interchange laws need.
probeVetFn :: Probe -> Vet (Int -> Int)
probeVetFn (Probe finding n) = (n +) <$ findingVet finding

findingVet :: Finding -> Vet ()
findingVet = \case
    Undetected -> rule (const (Refuse QueueUrlUnrecognised)) (const Nothing) ()
    Refused -> rule (const (Refuse QueueUrlUnrecognised)) Just "refused"
    Advised -> rule (const (Advise id)) Just "advised"
    RoleSplit -> rule roleSplit Just "split"

roleSplit :: RegistryRole -> Severity Text
roleSplit = \case
    MirrorWriter -> Advise id
    MirrorPruner -> Refuse QueueUrlUnrecognised
