{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- |
-- Module    : Shizen.AntColony.AntColony
-- Description : Continuous Ant Colony Optimization
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : experimental
-- Portability:
--
-- An implementation of Ant Colony optimization algorithm for continuous
-- functions.

module Shizen.AntColony.AntColony
  ( aco,
  )
where

import Data.Array.Accelerate as A
import Data.Function
import Data.Array.Accelerate.Data.Sort.Merge
import Data.Array.Accelerate.System.Random.SFC
import Shizen.AntColony.Types
import Shizen.Utils
import qualified Prelude as P

-- | Pheromone update function. Compute and normalize the fitness of each ant.
pheromones ::
  forall p b.
  AntPosition p b =>
  Acc (Vector (Ant p)) ->
  Acc (Vector R)
pheromones ants =
  let objectives = map getObjective ants :: Acc (Vector R)
      -- Calculate fitness from the objectives
      fitness = map (\x -> x >= 0 ? (1 / (1 + x), 1 + abs x)) objectives
      -- Normalize fitness
      fitnessSum = the $ sum fitness
   in map (/ fitnessSum) fitness :: Acc (Vector R)

-- | Function that creates a new ant
newAnt ::
  forall p b.
  AntPosition p b =>
  Exp SFC64 ->
  (Exp p -> Exp R) ->
  Exp R ->
  Exp b ->
  Exp p ->
  Exp p ->
  Exp (Ant p, SFC64)
newAnt gen f evr b refAntPos std =
  let -- We create the new ant, using the AntPosition
      -- function updatePosition
      T2 newPos gen2 = updatePosition evr b refAntPos std gen
      -- We compute the objective function for the new ant
      obj = f newPos
      newAnt' = T2 newPos obj
   in T2 newAnt' gen2

-- | Function which returns a random ant given the pheremones and a probability.
-- It performs the Roulette Wheel selection.
pickAnts ::
  forall p b.
  AntPosition p b =>
  Exp Int ->
  Acc Gen ->
  Acc (Vector R) ->
  Acc (VectorAnt p) ->
  Acc (VectorAnt p, Gen)
pickAnts c gen distribution ants =
  let -- We generate a random number for each ant.
      (randoms, gen') = runRandom gen randomVector :: (Acc (Vector R), Acc Gen)
      randomsMatrix = replicate (lift (Z :. c :. All)) randoms
      -- Replicate the distribution vector 'c' times (transposed)
      distributionMatrix = replicate (lift (Z :. All :. c)) distribution

      matrixRD = indexed $ zipWith (<) randomsMatrix distributionMatrix
      pickedIndex = fold1 (\(T2 i b1) (T2 j b2) -> b1 ? (T2 i b1, T2 j b2)) matrixRD
   in lift (map (\(T2 (I2 _ r) _) -> ants !! r) pickedIndex, gen')

computeStd ::
  forall p b.
  AntPosition p b =>
  Exp Int ->
  Exp Int ->
  Acc (VectorAnt p) ->
  Acc (VectorAnt p) ->
  Acc (Vector p)
computeStd c n picked old  =
      -- Both matrices have the same size: n x c
  let positionMartrix = replicate (lift (Z :. c :. All)) (map getPosition old) :: Acc (Matrix p)
      refPosMatrix = replicate (lift (Z :. All :. n)) (map getPosition picked) :: Acc (Matrix p)

      -- size: c x n.
      distMatrix = zipWith (\p r -> pmap abs $ difference p r) positionMartrix refPosMatrix :: Acc (Matrix p)

      -- Vector of size c. (n op add)
      distVector = fold1 add distMatrix :: Acc (Vector p)
    in map (pmap (/ (fromIntegral n - 1))) distVector


-- | Performs a single step of the algorithm.
makeNewAnts ::
  forall p b.
  AntPosition p b =>
  Acc Gen ->
  -- | Search space
  Exp b ->
  -- | Objective Function
  (Exp p -> Exp R) ->
  -- | ProblemType
  Exp Int ->
  -- | archive size
  Exp Int ->
  -- | Evaporation rate (learning rate)
  Exp R ->
  -- | Previous ants
  Acc (VectorAnt p) ->
  Acc (VectorAnt p, Gen)
makeNewAnts gen b f c n evr old =
  -- We compute the pheromones for the old ants.
  let ph = pheromones old :: Acc (Vector R)
      -- Accumulated sum of the probabilities
      distribution = scanl1 (+) ph :: Acc (Vector R)

      -- Choose 'c' ants as references
      T2 pickedAnts gen' = pickAnts c gen distribution old

      -- Compute the std for each position
      stdVector = computeStd c n pickedAnts old

      newAnts = zipWith3 (\a std g -> newAnt g f evr b (getPosition a) std) pickedAnts stdVector gen' :: Acc (Vector (Ant p, SFC64))

      -- Convert the new ants to a vector, and the Exp SFC64 to a Gen
      newAnts' = mfst newAnts :: Acc (VectorAnt p)
      gen'' = msnd newAnts :: Acc Gen

      -- We sort the new ants, and take the first 'n' ants.
      sorted = take n $ merge (compare `on` snd) old n newAnts' c :: Acc (VectorAnt p)
   in
      lift (sorted, gen'') :: Acc (VectorAnt p, Gen)


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
  -- | Objective function
  (Exp p -> Exp R) ->
  -- | Evaporation rate (Learning rate)
  R ->
  -- | Iterations
  Int ->
  P.IO (Acc (VectorAnt p))
aco k c b f evr maxit =
  do
    -- We start by creating the random generator.
    gen <- createGenerator k

    -- Now, we embed some variables into the Exp type of scalar expressions.
    let -- Parameter representing number of ants
        n = constant k :: Exp Int
        -- Parameter representing search space.
        b' = constant b
        -- Parameter representing the iterations of the algorithm loop.
        maxite = constant maxit :: Exp Int
        -- Parameter representing the number of new ants created at each iteration.
        varc = constant c :: Exp Int
        -- Parameter representing the evaporation of the trail
        evr' = constant evr :: Exp R

        -- We generate the ants positions randomly and within the boundaries
        (positions, gen') = runRandom gen (randomPositions b')

        -- We compute the function value of each position
        objective = map f positions :: Acc (Vector R)

        ants = sortBy (compare `on` snd) (zip positions objective)

        -- Now, we create a container for the variables that are going to be updated
        -- at each iteration
        genc = take varc gen' :: Acc (Vector SFC64)

        step :: Acc (VectorAnt p) -> Acc Gen  -> Acc (VectorAnt p, Gen)
        step o g = makeNewAnts g b' f varc n evr' o

        -- Algorithm loop
        T3 output _ _ =
          A.awhile
            (\(T3 _ it _) -> map (< maxite) it)
            ( \(T3 old it genc1) ->
                let T2 new genc2 = step old genc1
                    it' = map (+ 1) it
                 in T3 new it' genc2
            )
            (T3 ants (unit 0) genc)
     in -- Return the best ant
        P.return $ take 1 output
