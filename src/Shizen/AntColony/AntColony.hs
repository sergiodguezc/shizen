{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

-- |
-- Module    : Shizen.AntColony.AntColony
-- Description : Short description
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
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
import Data.Array.Accelerate.Data.Sort.MyMerge
import Data.Array.Accelerate.System.Random.SFC
import Shizen.AntColony.Types
import Shizen.AntColony.Utils

-- | Pheromone update function. It depends on the index of the ant in the
-- archive, the number of ants in the archive, and the evaporation rate.
pheromones ::
  forall p b.
  AntPosition p b =>
  -- Exp Int ->
  -- Exp R ->
  Acc (Vector (Ant p)) ->
  Acc (Vector R)
pheromones {- n  evr  -}ants =
  -- let weights = A.map (\(T2 (I1 i) _) -> gaussian n evr i) (indexed ants) :: Acc (Vector R)
  --     weightsS = the $ A.sum weights
  let weights = A.map getObjective ants :: Acc (Vector R)
      weightsS = the $ A.sum weights
   in A.map (A./ weightsS) weights :: Acc (Vector R)
  -- where
  --   gaussian :: Exp Int -> Exp R -> Exp Int -> Exp R
  --   gaussian n' evr' i =
  --     let rank = A.fromIntegral i :: Exp R
  --         k = A.fromIntegral n' :: Exp R
  --      in -- Harcoded: 0.005 is the standard deviation of the gaussian
  --         -- A.exp (A.negate ((rank A.** 2) A./ (2 A.* (evr' A.** 2) A.* (k A.** 2)))) A./ (k A.* A.sqrt (2 A.* A.pi))
  --         A.exp (A.negate ((rank A.** 2) A./ (2 A.* (evr' A.** 2) A.* (k A.** 2)))) A./ (k A.* 0.005 A.* A.sqrt (2 A.* A.pi))

-- | Function that creates a new ant
newAnt ::
  forall p b.
  AntPosition p b =>
  Exp SFC64 ->
  (Exp p -> Exp Objective) ->
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
  Acc (Vector R) ->
  Acc (Vector R) ->
  Acc (VectorAnt p) ->
  Acc (VectorAnt p)
pickAnts randoms distribution ants =
  let c = A.length randoms
      randomsMatrix = A.replicate (lift (Z :. c :. All)) randoms
      -- Replicate the distribution vector 'c' times (transposed)
      distributionMatrix = A.replicate (lift (Z :. All :. c)) distribution

      matrixRD = indexed $ A.zipWith (A.<) randomsMatrix distributionMatrix
      pickedIndex = A.fold1 (\(T2 i b1) (T2 j b2) -> b1 ? (T2 i b1, T2 j b2)) matrixRD
   in A.map (\(T2 (I2 _ r) _) -> ants A.!! r) pickedIndex

-- | Performs a single step of the algorithm.
makeNewAnts ::
  forall p b.
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
  -- | Previous ants
  Acc (VectorAnt p) ->
  (Acc (VectorAnt p), Acc Gen)
makeNewAnts gen b f pt c n evr old =
  -- We compute the pheromones for the old ants.
  let ph = pheromones {-n evr-} old :: Acc (Vector R)
      -- Accumulated sum of the probabilities
      distribution = A.scanl1 (+) ph :: Acc (Vector R)
      -- Now, we choose 'c' ants from the old ones,
      -- using the distribution as a probability.
      -- We generate a random number for each ant.
      (randoms, gen') = runRandom gen randomVector :: (Acc (Vector R), Acc Gen)

      pickedAnts = pickAnts randoms distribution old :: Acc (VectorAnt p)

      -- Both matrices have the same size: n x c
      refPosMatrix = A.replicate (lift (Z :. n :. All)) (A.map getPosition pickedAnts) :: Acc (Matrix p)
      positionMartrix = A.replicate (lift (Z :. All :. c)) (A.map getPosition old) :: Acc (Matrix p)

      -- distMatrix = A.transpose $ A.zipWith d2 positionMartrix refPosMatrix :: Acc (Matrix p)
      -- size: c x n
      distMatrix = A.transpose $ A.zipWith (\p r -> pmap (A.** 2) $ difference p r) positionMartrix refPosMatrix :: Acc (Matrix p)

      -- Vector of size c. (n op add)
      distVector = A.fold1 add distMatrix :: Acc (Vector p)
      avDist = A.map (pmap (\xi -> A.sqrt (xi A./ A.fromIntegral n))) distVector :: Acc (Vector p)
      -- Each projection of avDist is the average distance between the ref ant
      -- and the other ants
      -- Now, we have a vector with: ant + std + gen
      avDistRefAntGen = A.zip3 pickedAnts avDist gen' :: Acc (Vector (Ant p, p, SFC64))

      newAnts = A.map (\(T3 a std g) -> newAnt g f evr b (getPosition a) std) avDistRefAntGen :: Acc (Vector (Ant p, SFC64))

      -- Convert the new ants to a vector, and the Exp SFC64 to a Gen
      newAnts' = A.map (\(T2 a _) -> a) newAnts :: Acc (VectorAnt p)
      gen'' = A.map (\(T2 _ g) -> g) newAnts :: Acc Gen

      -- We sort the new ants, and take the first 'n' ants.
      sorted = A.take n $ merge (compareAnts pt) old n newAnts' c :: Acc (VectorAnt p)
      -- sorted = A.take n $ sortBy (compareAnts pt)(old A.++ newAnts') :: Acc (VectorAnt p)

   in (sorted, gen'') :: (Acc (VectorAnt p), Acc Gen)

-- sortAnts ::
--   forall p b.
--   AntPosition p b =>
--   Acc (VectorAnt p) ->
--   Acc (VectorAnt p)
-- sortAnts ants =
--     let indexObj = A.imap (\i a -> T2 (unindex1 i) (getObjective a)) ants
--         n = A.length ants
--         sortedObj = sortBy (on A.compare A.snd) indexObj :: Acc (Vector (Int, R))
--         newIndexes = A.map A.fst sortedObj :: Acc (Vector Int)
--     in A.scatter newIndexes (A.fill (index1 n) (ants A.!! 0)) ants

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
        evr' = constant evr :: Exp R

        -- We generate the ants positions randomly and within the boundaries
        (positions, gen1) = runRandom gen (randomPositions b')

        -- We compute the function value of each position
        objective = A.map f positions :: Acc (Vector Objective)

        ants = sortBy (compareAnts pt') (A.zip positions objective)

        -- Now, we create a container for the variables that are going to be updated
        -- at each iteration
        genc = A.take varc gen1
        container = newContainer 0 genc :: Container

        -- Algorithm loop
        loop =
          A.afst $
            A.awhile
              (\(T2 _ container') -> A.map (A.< maxite) (A.afst container'))
              ( \(T2 old container') ->
                  let genc1 = getGen container'
                      (new, genc2) = makeNewAnts genc1 b' f pt' varc n evr' old
                   in T2 new (updateContainer container' 1 genc2)
              )
              (T2 ants container)
     in -- Return the best ant
        return $ A.take 1 loop
        -- return loop
