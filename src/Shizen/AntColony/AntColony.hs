{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

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
import Data.Array.Accelerate.System.Random.SFC
import Shizen.AntColony.Types
import Shizen.AntColony.Utils
import Prelude as P

-- | Pheromone update function. It depends on the index of the ant in the
-- archive, the number of ants in the archive, and the evaporation rate.
pheromones ::
  forall p b.
  AntPosition p b =>
  Exp Int ->
  Exp R ->
  Acc (Vector (Ant p)) ->
  Acc (Vector R)
pheromones n evr ants =
  let weights = A.map (\(T2 i _) -> gaussian n evr (unindex1 i)) (indexed ants) :: Acc (Vector Double)
      weightsS = the $ A.sum weights
   in A.map (A./ weightsS) weights :: Acc (Vector Double)
  where
    gaussian :: Exp Int -> Exp R -> Exp Int -> Exp R
    gaussian n' evr' i =
      let rank = A.fromIntegral i :: Exp Double
          k = A.fromIntegral n' :: Exp Double
       in A.exp (A.negate ((rank A.** 2) A./ (2 A.* (evr' A.** 2) A.* (k A.** 2)))) A./ (k A.* 0.005 A.* A.sqrt (2 A.* A.pi))

-- | Function that creates a new ant
newAnt ::
  forall p b.
  AntPosition p b =>
  Acc Gen ->
  (Exp p -> Exp Objective) ->
  -- Exp ProblemType ->
  Exp R ->
  Exp b ->
  Acc (VectorAnt p) ->
  Acc (Vector R) ->
  (Acc (VectorAnt p), Acc Gen)
newAnt gen f {- pt  -} evr b ants distribution =
  -- Compute the reference ants
  let bb = constant (0, 1) :: Exp (Double, Double)
      (probs, gen1) = runRandom gen (randomRVector bb)
      probability = probs A.!! 0
      refAntPos = A.fst $ pickAnt probability distribution ants
      ps = A.map getPosition ants
      --
      (positions, gen2) = updatePosition evr b refAntPos ps gen1
      objective = A.map f positions :: Acc (Vector Objective)
      -- Create the new ants. A new ant is created by taking the best
      -- ant from the previous iteration and updating its position.
      -- newAnts = A.take 1 $ sortBy (compareAnts pt) $ A.zip positions objective
      newAnts = A.take 1 $ A.zip positions objective
   in (newAnts, gen2)

-- | Function which returns a random ant given the pheremones and a probability.
pickAnt :: forall p b. Position p b => Exp R -> Acc (Vector R) -> Acc (VectorAnt p) -> Exp (Ant p)
pickAnt prob distribution ants =
  -- We match each value of the distribution with its index and filter out
  -- values for which the probability is less than the value of the
  -- distribution.
  let ipairs = A.afst $ A.filter (\(T2 _ d) -> prob A.< d) (indexed distribution)
      -- We retrieve the first index that satisfies the above property
      i = unindex1 $ A.fst $ ipairs A.!! 0
   in ants A.!! i

-- | Performs a single step of the algorithm.
makeNewAnts ::
  AntPosition p b =>
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
              let (nant, g1) = newAnt g f {- pt  -} evr b old distribution
                  newVars = (na A.++ nant, g1)
               in lift newVars
          )
          (lift $ newAnt gen f {- pt  -} evr b old distribution)
      newAnts = A.take n $ sortBy (compareAnts pt) (old A.++ A.afst loop)
   in -- We return just the n best solutions
      (newAnts, A.asnd loop)

-- | Function that performs the ant colony algorithm
aco ::
  forall p b.
  AntPosition p b =>
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
  -- | Evaporation rate (Learning rate)
  R ->
  -- | Iterations
  Int ->
  IO (Acc (VectorAnt p))
aco k c b pt f evr maxit =
  do
    -- We start by creating the random generator.
    gen <- createGenerator k
    genPos <- createGenerator 1

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
        evaporationRate = constant evr :: Exp Double

        -- We generate the ants positions randomly and within the boundaries
        (positions, _) = runRandom gen (randomPosition b')

        -- We compute the function value of each position
        objective = A.map f positions :: Acc (Vector Objective)

        ants = sortBy (compareAnts pt') $ A.zip positions objective

        -- Now, we create a container for the variables that are going to be updated
        -- at each iteration
        container = newContainer 0 genPos :: Container

        -- Algorithm loop
        loop =
          A.afst $
            A.awhile
              (\(T2 _ container') -> A.map (A.< maxite) (unit (getIt container')))
              ( \(T2 old container') ->
                  let gen_pos1 = getGen container'
                      (new, gen_pos2) = makeNewAnts gen_pos1 b' f pt' varc n evaporationRate old
                   in T2 new (updateContainer container' 1 gen_pos2)
              )
              (T2 ants container)
     in -- Return the best ant
        return $ A.take 1 loop
