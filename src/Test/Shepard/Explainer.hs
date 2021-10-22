{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Shepard.Explainer where

import Control.Monad.Trans.State.Strict (runState)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.TreeDiff
import Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Tree as ITree
import           Moo.GeneticAlgorithm.Types (Population)

import Test.Shepard.Core


explainShepard
  :: (Shepard Bool s, ToExpr s)
  => s
  -> ShepardData Bool
  -> Maybe (Edit EditExpr, ShepardData Bool)
explainShepard sig shepard =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        let (newSigGen, finalShepard) = runState (tinker (pure sig)) shepard
        newSig <- newSigGen
        pure $ (ediff sig newSig, finalShepard)
 where
  genSize = Range.Size 1
  genSeed = Seed 12345 12345

explainShepardGen
  :: (Shepard Bool s, ToExpr s)
  => Maybe Size
  -> Maybe Seed
  -> Gen s
  -> ShepardData Bool
  -> Maybe (s, s, Edit EditExpr, ShepardData Bool)
explainShepardGen mbSize mbSeed sigGen shepard =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        sig    <- sigGen
        let (newSigGen, finalShepard) = runState (tinker (pure sig)) shepard
        newSig <- newSigGen
        pure $ (sig, newSig, ediff sig newSig, finalShepard)
 where
  genSize = case mbSize of
              Nothing -> Range.Size 1
              Just gz -> gz
  genSeed = case mbSeed of
              Nothing -> Seed 12345 12345
              Just gd -> gd

explainShepardGenFromFile
  :: (Shepard Bool s, ToExpr s)
  => Maybe Size
  -> Maybe Seed
  -> Gen s
  -> FilePath
  -> IO (Maybe (s, s, Edit EditExpr, ShepardData Bool))
explainShepardGenFromFile mbSize mbSeed sigGen fp = do
  str <- readFile fp
  pop <- case reads str :: [(Population Bool,String)] of
           [(pop,"")] -> pure pop
           _          -> error ("couldn't parse file: " <> fp)
  let bestGenome = case pop of
                     [] -> error "empty population"
                     ((best,_score):_) -> best
  let shepard = mkEmptyShepard bestGenome
  pure (explainShepardGen mbSize mbSeed sigGen shepard)
