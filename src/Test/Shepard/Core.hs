{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | The core typeclasses and associated methods of shepards.
module Test.Shepard.Core
  ( module Test.Shepard.Core
  , (<$$>)
  , (<**>)
  ) where

import           Control.Monad (replicateM)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Typeable (Typeable)
import           Data.TypeRepMap (TypeRepMap)
import qualified Data.TypeRepMap as TM
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Lens.Micro.Mtl ((%=), (.=), use)
import           Lens.Micro.TH (makeLenses)
import           Moo.GeneticAlgorithm.Types (Genome, Population)

import           Test.Shepard.Util


-- | The state we carry as we perform shepards actions.
data ShepardData s = ShepardData
  { -- | Remaining genes, controlling how a Shepard operates.
    _genes       :: !(Genome s)
    -- | A Shepard's bag of tricks contains items of many differnt types. When
    -- tinkering, a Shepard (depending on its genome) might look in its bag of
    -- tricks to see whether it has anything of the appropriate type to replace
    -- what it's currently tinkering with (or, depending on the type, do
    -- something different - for example, utilise a monoid instance to add
    -- things together).
  , _bagOfTricks :: !(TypeRepMap [])
  }
makeLenses 'ShepardData

-- | Tinker monad.
type TinkerM s = State (ShepardData s)


-- | The interface to shepards. This class defines two actions
--   - `tinker`ing with an existing value
--   - `conjure`ing a new value
class (GeneOps s, Typeable a) => Shepard s a where
  -- | Tinker with an item of type 'a'.
  tinker :: Gen a -> TinkerM s (Gen a)

  -- | As well as tinkering, shepards can conjure fresh items into existence.
  conjure :: TinkerM s a


-- | Helper function to save a value in the bagOfTricks, and return it.
saveInBagOfTricks :: forall s a. Typeable a => a -> TinkerM s a
saveInBagOfTricks x = do
  bagOfTricks %= consOrInsert
  pure x
 where
  consOrInsert :: TypeRepMap [] -> TypeRepMap []
  consOrInsert trm = if TM.member @a trm
                        then TM.adjust (x:) trm
                        else TM.insert [x] trm

-- | Construct a tinker function given a set of possible things to do.
--
--   Each 'toy' is a function taking the original value and one grabbed from the
--   bag of tricks or conjured.
tinkerWithToys
  :: (AddShrinks a, Shepard s a)
  => [Gen a -> Gen a -> Gen a]
  -> (Gen a -> TinkerM s (Gen a))
tinkerWithToys toys a =
  let
    defaultToys = [const, flip const]
    allToys     = defaultToys ++ toys
  in tinkerRummagedOrConjureOrSave $ do
    toy <- (allToys !!) <$> geneListIndex allToys
    toy a <$> tinkerRummagedOrConjure

-- | Either tinker with a rummaged value, conjure a new value, or save the
-- argument in the bagOfTricks and return it.
tinkerRummagedOrConjureOrSave :: (Shepard s a, AddShrinks a)
                              => TinkerM s (Gen a) -> TinkerM s (Gen a)
tinkerRummagedOrConjureOrSave m =
  onGene tinkerRummagedOrConjure (saveInBagOfTricks =<< m)

--------------------------------------------------------------------------------
-- Gene operations
--------------------------------------------------------------------------------

-- | Read (and consume) a gene from the genome.
transcribeGene :: TinkerM s s
transcribeGene = do
  s <- use genes
  case s of
    [] -> error "Genome has run out! Try increasing the size of the genome."
    (x : xs) -> do

      genes .= xs
      return x

-- | A typeclass for actions over genomes.
class GeneOps s where
  -- | Choose between two actions based on the value of a gene.
  onGene
    :: TinkerM s a
    -- ^ When gene is on.
    -> TinkerM s a
    -- ^ When gene is off.
    -> TinkerM s a

  -- | Transcribe sufficient genes to get an integer in the range [0..n].
  transcribeGenesAsInt
    :: Int
    -> TinkerM s Int


--------------------------------------------------------------------------------
-- Bag of tricks
--------------------------------------------------------------------------------

-- | Fetch something from the bag of tricks if there's something there.
rummage :: forall a s . (GeneOps s, Typeable a) => TinkerM s (Maybe a)
rummage = do
  bag <- use bagOfTricks
  case TM.lookup bag of
    Nothing -> pure Nothing
    -- @mhueschen: \/ will not shrink, I believe
    Just xs ->
      (xs !!) <$> geneListIndex xs

-- | Fetch everything from the bag of tricks.
rummageAll :: forall a s . Typeable a => TinkerM s [a]
rummageAll = do
  bag <- use bagOfTricks
  case TM.lookup bag of
    Nothing -> pure []
    -- @mhueschen: \/ will not shrink, I believe
    Just xs -> pure xs

-- | Fetch something from the bag of tricks, or else conjure it up.
rummageOrConjure :: forall a s . Shepard s a => TinkerM s a
rummageOrConjure = maybe conjure pure =<< rummage

-- | Attempt to rummage. If a value is available, either tinker with it or
-- leave it intact. If no value is available, conjure a fresh one and add
-- shrinks to it.
tinkerRummagedOrConjure :: forall a s . (Shepard s a, AddShrinks a)
                        => TinkerM s (Gen a)
tinkerRummagedOrConjure = do
  mR <- rummage
  case mR of
    Nothing -> addShrinks <$> conjure
    Just  v -> onGene (tinker v) (pure v)


--------------------------------------------------------------------------------
-- AddShrinks class
--------------------------------------------------------------------------------

-- | Whereas `pure` creates a Hedgehog tree with no shrinks, `addShrinks`
--   creates a tree with shrinks.
class AddShrinks a where
  addShrinks :: a -> Gen a
  default addShrinks
    :: Enum a
    => a
    -> Gen a
  addShrinks = Gen.shrink shrinkEnum . pure

-- | Use an Enum instance to create a shrink tree which shrinks towards
-- `toEnum 0`.
shrinkEnum :: Enum a => a -> [a]
shrinkEnum x = toEnum <$>
  if e == 0
     then []
     else tail (enumFromThenTo e e1 0)
 where
  absDecr v = signum v * (abs v - 1)
  e  = fromEnum x
  e1 = absDecr e


--------------------------------------------------------------------------------
-- SeedShepard class
--------------------------------------------------------------------------------

-- | Recur down a datatype, adding the sub-datatypes to the `TinkerM` `TypeRepMap`
class SeedShepard a where
  -- | Recur down a type, adding elements to the TypeRepMap
  seeder :: a -> TinkerM s ()

  default seeder
    :: Typeable a
    => a
    -> TinkerM s ()
  seeder x = () <$ saveInBagOfTricks x


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Spawn a Shepard from a given genome and a bag of tricks.
mkShepard :: Genome s -> TypeRepMap [] -> ShepardData s
mkShepard = ShepardData

-- | Spawn a Shepard from a genome, with an empty TypeRepMap.
mkEmptyShepard :: Genome s -> ShepardData s
mkEmptyShepard genome = ShepardData genome TM.empty

-- | Use the genome to generate an index within the bounds
-- of the provided list.
geneListIndex :: GeneOps s => [a] -> TinkerM s Int
geneListIndex xs = transcribeGenesAsInt (length xs - 1)

-- | Convenience Hedgehog generator.
genPopulation :: Gen (Population Bool)
genPopulation = do
  genomeSize <- Gen.int (Range.linear 0 1000)
  Gen.list (Range.linear 0 300) $ do
    genome <- replicateM genomeSize Gen.bool
    (,) <$> pure genome <*> Gen.double (Range.constant 0 (10.0^^(3::Int)))
