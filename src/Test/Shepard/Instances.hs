{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Shepard.Instances where

import           Control.Applicative (liftA2)
import           Control.Monad (replicateM)
import qualified Data.Bimap as Bimap
import           Data.Char (chr)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Ratio (Ratio)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8, Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Lens.Micro.Mtl ((.=), use)
import           Moo.GeneticAlgorithm.Binary (bitsNeeded, decodeBinary)
import           Numeric.Natural (Natural)

import Test.Shepard.Core
import Test.Shepard.TH


instance GeneOps Bool where

  onGene yes no = do
    tg <- transcribeGene
    if tg then yes else no

  transcribeGenesAsInt n = do
    (gs, xs) <- splitAt (bitsNeeded (0,n)) <$> use genes
    genes .= xs
    let base = n+1
    if base == 0
       then error "transcribeGenesAsInt: divide by zero"
       else return $ decodeBinary (0, n) gs `mod` base

--------------------------------------------------------------------------------
-- Primitive shepards
--------------------------------------------------------------------------------

instance GeneOps a => Shepard a Bool where
  tinker _ = addShrinks <$> onGene rummageOrConjure conjure
  conjure = saveInBagOfTricks =<< onGene (pure True) (pure False)

instance GeneOps a => Shepard a Char where
  tinker _ = addShrinks <$> onGene rummageOrConjure conjure
  -- TODO : this uses up 21 bits of genome, and we may not be interested in thorough
  -- coverage of the Char space
  conjure = saveInBagOfTricks =<< chr <$> transcribeGenesAsInt 1114111

instance GeneOps a => Shepard a Integer where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< toEnum <$> conjure

instance GeneOps a => Shepard a Natural where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

instance GeneOps a => Shepard a Int where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< (\x -> x-1000) <$> transcribeGenesAsInt 2000

instance GeneOps a => Shepard a Word64 where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

-- | This instance generates Double values in range [0..1] (inclusive) at 0.01
-- increments. 0.01, 0.02 ... 0.99, 1.00
instance GeneOps a => Shepard a Double where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< do
    i <- transcribeGenesAsInt 100
    pure (fromIntegral i / 100)


--------------------------------------------------------------------------------
-- Composite shepards
--------------------------------------------------------------------------------

deriveShepard ''(,)
deriveShepard ''(,,)
deriveShepard ''(,,,)
deriveShepard ''(,,,,)
deriveShepard ''(,,,,,)
deriveShepard ''(,,,,,,)
deriveShepard ''(,,,,,,,)
deriveShepard ''(,,,,,,,,)
deriveShepard ''(,,,,,,,,,)
deriveShepard ''(,,,,,,,,,,)
deriveShepard ''Ratio

instance (Shepard s a, AddShrinks a)
      => Shepard s (Maybe a) where
  -- TODO mhueschen - reconsider this. it seems suspect.
  tinker obj = tinkerRummagedOrConjureOrSave $ do
    x <- tinker (Gen.just obj)
    pure (Gen.maybe x)

  conjure = saveInBagOfTricks =<<
    onGene (pure Nothing) (Just <$> conjure)

-- | Our list Shepard behaves slightly differently, since it pulls whole lists of
-- things from the bag of tricks, and is also specialised to do some more
-- messing about with lists.
instance (AddShrinks a, Eq a, Typeable a, GeneOps s, Shepard s a)
      => Shepard s [a] where
  tinker obj = tinkerRummagedOrConjureOrSave $ do
    rummaged <- (map (sequenceA . map addShrinks)) <$> rummageAll
    -- If there's nothing to rummage, we do unary operations
    -- Otherwise we select BinOps or UnOps based on a gene
    if (null rummaged)
       then toyUnOp
       else onGene toyUnOp (toyBinOp rummaged)

   where
    toyUnOp :: TinkerM s (Gen [a])
    toyUnOp = do
      toy <- (unOpToys !!) <$> geneListIndex unOpToys
      toy obj

    unOpToys :: [Gen [a] -> TinkerM s (Gen [a])]
    unOpToys =
      [ pure
      , \a -> pure (Gen.shuffle =<< a)
      , \a -> pure (Gen.subsequence =<< a)
      -- TODO mhueschen | consider tinkering with elements here
      ]

    toyBinOp :: [Gen [a]] -> TinkerM s (Gen [a])
    toyBinOp rummaged = do
      toy <- (binOpToys !!) <$> geneListIndex binOpToys
      val <- (rummaged !!) <$> geneListIndex rummaged
      toy obj val

    -- Toys for lists can use 'TinkerM', because they might be random
    binOpToys :: [Gen [a] -> Gen [a] -> TinkerM s (Gen [a])]
    binOpToys =
      [ \a _ -> pure a
      , \_ b -> pure b
      , \a _ -> pure (Gen.shuffle =<< a)
      , \a b -> pure ((++) <$> a <*> (Gen.subsequence =<< b))
      , \a b -> pure ((List.\\) <$> a <*> (Gen.subsequence =<< b))
      , \a b -> pure ((++) <$> (Gen.subsequence =<< a)
                           <*> (Gen.subsequence =<< b))
      ]

  conjure = saveInBagOfTricks =<< do
    listLen <- transcribeGenesAsInt 15
    replicateM listLen conjure

instance (Shepard s a, Ord a, AddShrinks a, Typeable a)
      => Shepard s (Set.Set a) where
  tinker obj = tinkerRummagedOrConjureOrSave $ do
    rummaged <- (map (sequenceA . map addShrinks)) <$> rummageAll
    -- If there's nothing to rummage, we do unary operations
    -- Otherwise we select BinOps or UnOps based on a gene
    if (null rummaged)
       then toyUnOp
       else onGene toyUnOp (toyBinOp rummaged)

   where
    toyUnOp :: TinkerM s (Gen (Set.Set a))
    toyUnOp = do
      toy <- (unOpToys !!) <$> geneListIndex unOpToys
      toy obj

    unOpToys :: [Gen (Set.Set a) -> TinkerM s (Gen (Set.Set a))]
    unOpToys =
      [ \a -> pure a
      , \a -> pure (Set.fromList <$> (Gen.shuffle =<< (Set.toList <$> a)))
      , \a -> pure (Set.fromList <$> (Gen.subsequence =<< (Set.toList <$> a)))
      -- TODO mhueschen | consider tinkering with elements here
      ]

    toyBinOp :: [Gen [a]] -> TinkerM s (Gen (Set.Set a))
    toyBinOp rummaged = do
      toy <- (binOpToys !!) <$> geneListIndex binOpToys
      val <- (rummaged !!) <$> geneListIndex rummaged
      toy obj val

    -- Toys for sets can use 'TinkerM', because they might be random
    binOpToys :: [Gen (Set.Set a) -> Gen [a] -> TinkerM s (Gen (Set.Set a))]
    binOpToys =
      [ \a _ -> pure a
      , \_ b -> pure (Set.fromList <$> b)
      , \a b -> pure (Set.difference
                       <$> a
                       <*> (Set.fromList <$> (Gen.subsequence =<< b)))
      , \a b -> pure (Set.union
                       <$> a
                       <*> (Set.fromList <$> (Gen.subsequence =<< b)))
      , \a b -> pure $ (Set.intersection <$> a <*> (Set.fromList <$> b))
      ]

  conjure = saveInBagOfTricks =<< do
    listLen <- transcribeGenesAsInt 15
    cs <- replicateM listLen conjure
    pure (Set.fromList cs)

instance (Shepard s k, Shepard s v, Ord k, Eq k, Eq v, AddShrinks (Map.Map k v),
          AddShrinks k, AddShrinks v, Typeable k, Typeable v)
         => Shepard s (Map.Map k v) where
    tinker obj = tinkerRummagedOrConjureOrSave $ do
      rummagedKeys <- (map (sequenceA . map addShrinks)) <$> rummageAll
      rummagedVals <- (map (sequenceA . map addShrinks)) <$> rummageAll
      -- If there's nothing to rummage, we do unary operations
      -- Otherwise we select BinOps or UnOps based on a gene
      if (null rummagedKeys) || (null rummagedVals)
         then toyUnOp
         else onGene toyUnOp (toyBinOp rummagedKeys rummagedVals)

     where
      toyUnOp :: TinkerM s (Gen (Map.Map k v))
      toyUnOp = do
        toy <- (unOpToys !!) <$> geneListIndex unOpToys
        toy obj

      unOpToys :: [Gen (Map.Map k v) -> TinkerM s (Gen (Map.Map k v))]
      unOpToys =
        [ \a -> pure a
        , \a -> pure (Map.fromList <$> (Gen.shuffle =<< (Map.toList <$> a)))
        , \a -> pure (Map.fromList <$> (Gen.subsequence =<< (Map.toList <$> a)))
        -- TODO mhueschen | consider tinkering with elements here
        ]

      toyBinOp :: [Gen [k]] -> [Gen [v]] -> TinkerM s (Gen (Map.Map k v))
      toyBinOp rummagedKeys rummagedVals = do
        toy <- (binOpToys !!) <$> geneListIndex binOpToys
        key <- (rummagedKeys !!) <$> geneListIndex rummagedKeys
        val <- (rummagedVals !!) <$> geneListIndex rummagedVals
        toy obj key val

      -- Toys for sets can use 'TinkerM', because they might be random
      binOpToys :: [Gen (Map.Map k v) -> Gen [k] -> Gen [v]
                -> TinkerM s (Gen (Map.Map k v))]
      binOpToys =
        [ \a _ _ -> pure a
        , \a k v ->
          pure (Map.union
                 <$> a
                 <*> (Map.fromList <$> (zip <$> (Gen.subsequence =<< k)
                                            <*> (Gen.subsequence =<< v))))
        , \a k _ -> pure (Map.withoutKeys <$> a
                           <*> (Set.fromList <$> (Gen.subsequence =<< k)))
        , \a k _ -> pure (Map.restrictKeys <$> a
                           <*> (Set.fromList <$> (Gen.subsequence =<< k)))
        ]

    conjure = saveInBagOfTricks =<< do
      listLen <- transcribeGenesAsInt 15
      cs <- replicateM listLen conjure
      pure (Map.fromList cs)

--------------------------------------------------------------------------------
-- AddShrinks
--------------------------------------------------------------------------------

instance AddShrinks () where
instance AddShrinks Bool where
instance AddShrinks Char where
instance AddShrinks Double where
instance AddShrinks Integer where
instance AddShrinks Natural where
instance AddShrinks Int where
instance AddShrinks Word8 where
instance AddShrinks Word64 where

deriveAddShrinks ''(,)
deriveAddShrinks ''(,,)
deriveAddShrinks ''(,,,)
deriveAddShrinks ''(,,,,)
deriveAddShrinks ''(,,,,,)
deriveAddShrinks ''(,,,,,,)
deriveAddShrinks ''(,,,,,,,)
deriveAddShrinks ''(,,,,,,,,)
deriveAddShrinks ''(,,,,,,,,,)
deriveAddShrinks ''(,,,,,,,,,,)
deriveAddShrinks ''Ratio

instance (AddShrinks k, Ord k, AddShrinks v) => AddShrinks (Map.Map k v) where
  addShrinks xs = Map.fromList <$> mapM addShrinks (Map.toList xs)

instance AddShrinks a => AddShrinks [a] where
  addShrinks ls = mapM addShrinks ls

instance (AddShrinks a, Ord a) => AddShrinks (Set.Set a) where
  addShrinks xs = Set.fromList <$> mapM addShrinks (Set.toList xs)

instance AddShrinks a => AddShrinks (Maybe a) where
  addShrinks Nothing  = pure Nothing
  addShrinks (Just x) = Just <$> addShrinks x

--------------------------------------------------------------------------------
-- SeedShepard
--------------------------------------------------------------------------------

instance SeedShepard () where
instance SeedShepard Bool where
instance SeedShepard Char where
instance SeedShepard Integer where
instance SeedShepard Natural where
instance SeedShepard Int where
instance SeedShepard Word8 where
instance SeedShepard Word64 where
instance SeedShepard Double where

deriveSeedShepard ''(,)
deriveSeedShepard ''(,,)
deriveSeedShepard ''(,,,)
deriveSeedShepard ''(,,,,)
deriveSeedShepard ''(,,,,,)
deriveSeedShepard ''(,,,,,,)
deriveSeedShepard ''(,,,,,,,)
deriveSeedShepard ''(,,,,,,,,)
deriveSeedShepard ''(,,,,,,,,,)
deriveSeedShepard ''(,,,,,,,,,,)

instance (SeedShepard a, Typeable a) => SeedShepard [a] where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> xs)
instance (SeedShepard a, Typeable a) => SeedShepard (Seq.Seq a) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> xs)
instance (SeedShepard a, Typeable a, SeedShepard b, Typeable b)
  => SeedShepard (Bimap.Bimap a b) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> Bimap.keys xs)
    () <$ sequenceA (seeder <$> Bimap.elems xs)
instance (SeedShepard a, Typeable a, SeedShepard b, Typeable b)
  => SeedShepard (Map.Map a b) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> Map.keys xs)
    () <$ sequenceA (seeder <$> Map.elems xs)
instance (SeedShepard a, Typeable a) => SeedShepard (Set.Set a) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> Set.toList xs)
instance (SeedShepard a, Typeable a) => SeedShepard (Maybe a) where
  seeder mb = do
    () <$ saveInBagOfTricks mb
    case mb of
      Nothing -> pure ()
      Just x -> seeder x
