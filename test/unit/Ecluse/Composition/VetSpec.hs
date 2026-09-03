-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
    decided,
    rule,
    runVet,
    vetRole,
 )

spec :: Spec
spec = do
    lawSpec
    accumulationSpec
    inputSpec

{- The four applicative laws. A hand-rolled instance can break any of them, and the accumulation
every boot report depends on is only total while they hold. Each is written out in full, because
hlint would otherwise "simplify" the exact expression under test. -}
{- HLINT ignore lawSpec "Use <$>" -}
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
            `shouldBe` [ ([], Left [QueueUrlUnrecognised "Refused 1", QueueUrlUnrecognised "Refused 2"])
                       , ([], Left [QueueUrlUnrecognised "Refused 1", QueueUrlUnrecognised "Refused 2"])
                       ]

    it "logs the advisories of both sides of an application, in order" $
        -- Both sides advise, so reversing the mappend that joins them fails here.
        observe (probeVetFn (Probe Advised 1) <*> probeVet (Probe Advised 2))
            `shouldBe` [ (["Advised 1", "Advised 2"], Right 3)
                       , (["Advised 1", "Advised 2"], Right 3)
                       ]

    it "keeps the advisories of a refused pass, so a refusal never hides an advisory" $
        observe (probeVetFn (Probe Advised 1) <*> probeVet (Probe Refused 2))
            `shouldBe` [ (["Advised 1"], Left [QueueUrlUnrecognised "Refused 2"])
                       , (["Advised 1"], Left [QueueUrlUnrecognised "Refused 2"])
                       ]

-- The two ways into a pass that are not a rule: the role it runs for, and a settled outcome.
inputSpec :: Spec
inputSpec = describe "the pass's own inputs" $ do
    it "reads back the role it runs for, so a role-specific witness has a source" $
        observe vetRole `shouldBe` [([], Right MirrorWriter), ([], Right MirrorPruner)]

    it "accumulates an already-decided refusal after the rules preceding it" $
        observe (probeVetFn (Probe Refused 1) <*> decided (Left [QueueUrlUnrecognised "settled"]))
            `shouldBe` [ ([], Left [QueueUrlUnrecognised "Refused 1", QueueUrlUnrecognised "settled"])
                       , ([], Left [QueueUrlUnrecognised "Refused 1", QueueUrlUnrecognised "settled"])
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
probeVet probe@(Probe _ n) = n <$ findingVet probe

-- The same probe as a function-valued vet, which the composition and interchange laws need.
probeVetFn :: Probe -> Vet (Int -> Int)
probeVetFn probe@(Probe _ n) = (n +) <$ findingVet probe

{- Each finding carries its own probe's value, so the two sides of an application are
distinguishable and an assertion on their order fails when they are swapped. -}
findingVet :: Probe -> Vet ()
findingVet (Probe finding n) = case finding of
    Undetected -> rule (const (Refuse QueueUrlUnrecognised)) (const Nothing) label
    Refused -> rule (const (Refuse QueueUrlUnrecognised)) Just label
    Advised -> rule (const (Advise id)) Just label
    RoleSplit -> rule roleSplit Just label
  where
    label = show finding <> " " <> show n

roleSplit :: RegistryRole -> Severity Text
roleSplit = \case
    MirrorWriter -> Advise id
    MirrorPruner -> Refuse QueueUrlUnrecognised
