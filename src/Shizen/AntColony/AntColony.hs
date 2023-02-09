{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module    : Shizen.AntColony.AntColony
-- Description : Short description
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : experimental
-- Portability:
--
-- An implementation of Ant Colony optimization algorithm for continuous
-- functions.
module Shizen.AntColony.AntColony
  ( module Shizen.AntColony.AntColony,
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Sort.Quick
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.System.Random.SFC
import Shizen.AntColony.Utils
import Shizen.AntColony.Types
import Prelude as P

pheromones :: forall p b. Position p b => Exp Int -> Exp R -> Acc (Vector (Ant p)) -> Acc (Vector R)
pheromones n evr ants =
  let weights = A.map (\(T2 i _) -> gaussian n evr (unindex1 i)) (indexed ants) :: Acc (Vector Double)
      weightsS = the $ A.sum weights
   in A.map (A./ weightsS) weights :: Acc (Vector Double)
  where
    gaussian :: Exp Int -> Exp R -> Exp Int -> Exp R
    gaussian n' evr' i =
      let rank = A.fromIntegral i :: Exp Double
          k = A.fromIntegral n' :: Exp Double
       in A.exp (A.negate ((rank A.** 2) A./ (2 A.* (evr' A.** 2) A.* (k A.** 2)))) A./ (k A.* 0.05 A.* A.sqrt (2 A.* A.pi))

newAnt ::
  forall p b. Position p b =>
  Acc Gen ->
  (Exp p -> Exp Objective) ->
  Exp ProblemType ->
  Exp R ->
  Exp b ->
  Acc (VectorAnt p) ->
  Acc (Vector R) ->
  (Acc (VectorAnt p), Acc Gen)
newAnt gen f pt evr b ants distribution =
      -- Compute the reference ants
  let bb = constant (0, 1) :: Exp (Double, Double)
      (ps, gen1) = runRandom gen (randomRVector bb)
      probability = ps A.!! 0
      refAnt = pickAnt probability distribution ants
      --
      (positions, gen2) = updatePosition evr b refAnt ants gen1
      objective = A.map f positions :: Acc (Vector Objective)
      newAnts = A.take 1 $ sortBy (compareAnts pt) $ A.zip positions objective
   in (newAnts, gen2)


-- | Function which returns a random ant given the pheremones and a probability.
pickAnt :: forall p b. Position p b => Exp R -> Acc (Vector R) -> Acc (VectorAnt p) -> Exp (Ant p)
pickAnt prob distribution ants =
  -- We match each value of the distribution with its index and filter out
  -- values for which the probability is less than the value of the
  -- distribution.
  let ipairs = A.afst $ A.filter (\(T2 _ d) -> d A.< prob) (indexed distribution)
      -- We retrieve the first index that satisfies the above property
      i = unindex1 $ A.fst $ ipairs A.!! 0
      n = A.length ants - 1
      k = i A.> n ? (n, i A.< 0 ? (0, i))
   in ants A.!! k

-- | Performs a single step of the algorithm.
makeNewAnts ::
  Position p b =>
  Acc Gen ->
  -- | Search space
  Exp b ->
  -- | Objective Function
  (Exp p -> Exp Objective) ->
  -- | ProblemType
  Exp ProblemType ->
  -- | number of new ants generated
  Exp Int ->
  -- | archive size
  Exp Int ->
  -- | Evaporation rate (learning rate)
  Exp R ->
  -- | Randomly generated ants
  Acc (VectorAnt p) ->
  (Acc (VectorAnt p), Acc Gen)
makeNewAnts gen b f pt c n evr old =
  -- We compute the pheromones for the old ants.
  let ph = pheromones n evr old
      -- Accumulated sum of the probabilities
      distribution = A.scanl1 (+) ph :: Acc (Vector Double)

      -- Loop
      loop =
        A.awhile
          ( \(T2 na _) -> unit $ A.length na A.< c
          )
          ( \(T2 na g) ->
               let (nant, g1) = newAnt g f pt evr b old distribution
                   newVars = (na A.++ nant, g1)
               in lift newVars
          )
          (lift $ newAnt gen f pt evr b old distribution)
      newAnts = A.take n $ sortBy (compareAnts pt) (old A.++ A.afst loop)
   in -- We return just the n best solutions
      (newAnts, A.asnd loop)

-- | Function that performs the ant colony algorithm
antColony ::
  forall p b. Position p b =>
  -- | Archive size (number of ants)
  Int ->
  -- | Cardinality of the new solution
  Int ->
  -- | Boundaries
  b ->
  -- | Minimization | Maximization
  ProblemType ->
  -- | Objective function
  (Exp p -> Exp Objective) ->
  -- | Evaporation rate
  R ->
  -- | Learning rate (jump at the first iteration)
  R ->
  -- | Iterations
  Int ->
  IO (Acc (VectorAnt p))
antColony k c b pt f evr ijump maxit =
  do
    -- We start by creating the random generator.
    gen <- createGenerator k
    gen_pos <- createGenerator c
    -- Now, we embed some variables into the Exp type of scalar expressions.

    let -- Parameter representing number of ants
        n = constant k :: Exp Int
        -- Parameter representing search space.
        b' = constant b
        -- Parameter representing the problem type
        pt' = constant pt :: Exp ProblemType
        -- Parameter representing the iterations of the algorithm loop.
        maxite = constant maxit :: Exp Int
        -- Parameter representing the number of new ants created at each iteration.
        varc = constant c :: Exp Int
        -- Parameter representing the evaporation of the trail
        evaporation_rate = constant evr :: Exp Double
        -- Parameter representing the jump at the first iteration (learning rate)
        alpha = constant ijump :: Exp Double

        -- We generate the ants positions randomly and within the boundaries
        (positions, _) = runRandom gen (randomPosition b')

        -- We compute the function value of each position
        objective = A.map f positions :: Acc (Vector Objective)

        ants = sortBy (compareAnts pt') $ A.zip positions objective

        -- Now, we create a container for the variables that are going to be updated
        -- at each iteration
        container = newContainer 0 alpha gen_pos :: Container

        -- Algorithm loop
        loop =
          A.afst $
            A.awhile
              (\(T2 _ container') -> A.map (A.< maxite) (unit (getIt container')))
              ( \(T2 old container') ->
                  let (new, gen_pos1) = makeNewAnts gen_pos b' f pt' varc n evaporation_rate old
                   in T2 new (updateContainer container' 1 evaporation_rate gen_pos1)
              )
              (T2 ants container)
     in return loop

prueba :: IO ()
prueba = do
  ants <- antColony 10 4 (bounds 10) False funct 0.9 1 100
  print $ CPU.run $ unit $ ants A.!! 0
  where
    bounds :: R -> B4
    bounds a = B4_ (-a, a) (-a, a) (-a, a) (-a, a)
    -- print ants
    funct :: Exp P4 -> Exp Objective
    funct (P4 x y z t) = x + y + z + t
    funct _ = 0.0
