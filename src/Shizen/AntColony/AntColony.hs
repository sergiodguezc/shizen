{-# LANGUAGE FlexibleContexts #-}

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
  ( module Shizen.AntColony.AntColony
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Sort.Quick
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.System.Random.MWC as R
import Shizen.AntColony.Random
import System.Random
import Shizen.AntColony.Types
import Control.Monad.ST


-- | Function which compute the pheromones. The probability is based on the rank
-- rather than in the fitness value.
pheromones :: Double -> Acc (Vector Ant) -> Acc (Vector Double)
pheromones evr ants =
  let n = A.length ants
      evre = constant evr :: Exp Double
      weights = A.map (\(T2 i _) -> gaussian evre n (unindex1 i)) (indexed ants) :: Acc (Vector Double)
      weightsS = the $ A.sum weights
   in A.map (A./ weightsS) weights :: Acc (Vector Double)
  where
    gaussian :: Exp Double -> Exp Int -> Exp Int -> Exp Double
    gaussian evr n i =
      let rank = A.fromIntegral i :: Exp Double
          k = A.fromIntegral n :: Exp Double
       in -- Hardcoded: t = 0.05
          A.exp (A.negate ((rank A.** 2) A./ (2 A.* (0.05 A.** 2) A.* (k A.** 2)))) A./ (k A.* 0.05 A.* A.sqrt (2 A.* A.pi))

newAnt :: StdGen -> (Exp Position -> Exp Objective) -> Double -> Boundaries -> Acc (Vector Ant) -> Acc (Vector Double) -> Acc (Vector Ant)
newAnt gen f evr b ants distribution =
  let probability = constant $ uniformSample gen (0, 1) :: Exp Double
      refAnt = pickAnt probability distribution ants :: Exp Ant
      evre = constant evr :: Exp Double

      -- Stddev
      sdx = evre * average_d2 0 refAnt ants :: Exp Double
      sdy = evre * average_d2 1 refAnt ants :: Exp Double
      sdz = evre * average_d2 2 refAnt ants :: Exp Double
      -- Means
      mx = getx $ getPosition refAnt :: Exp Double
      my = gety $ getPosition refAnt :: Exp Double
      mz = getz $ getPosition refAnt :: Exp Double

      newx = sampleG gen mx sdx :: Exp Double
      newy = sampleG gen my sdy :: Exp Double
      newz = sampleG gen mz sdz :: Exp Double
      -- newx = constant 0.0
      -- newy = constant 0.0
      -- newz = constant 0.0
      newPos = Position newx newy newz :: Exp Position
   in flatten . unit $ lift (newPos, f newPos)
   -- in flatten $ unit $ refAnt
  where
  -- TODO: Broken function
  sampleG :: StdGen -> Exp Double -> Exp Double -> Exp Double
  sampleG gen m sd = 
    let z = constant $ gaussianSample gen 0 1
        --
     in z A.* sd + m


-- | Function which returns a random ant given the distribution and a probability.
pickAnt :: Exp Double -> Acc (Vector Double) -> Acc (Vector Ant) -> Exp Ant
pickAnt prob distribution ants =
  -- We match each value of the distribution with its index and filter out
  -- values for which the probability is less than the value of the
  -- distribution.
  let ipairs = A.afst $ A.filter (\(T2 _ ph) -> ph A.< prob) (indexed distribution)
      -- We retrieve the first index that satisfies the above property
      i = unindex1 $ A.fst $ ipairs A.!! 0
      n = A.length ants
      k = i A.> n A.? (n, i)
   in ants A.!! k

average_d2 ::
  -- | index
  Exp Int ->
  -- | selected ant
  Exp Ant ->
  -- | ants
  Acc (Vector Ant) ->
  Exp Double
average_d2 i a ants =
  let value = getComponent i $ getPosition a :: Exp Double
   in the $ A.sum $ A.map (\a -> A.abs (getComponent i $ getPosition a) - value) ants

-- Function that calculates the next generation of ants
makeNewAnts :: StdGen -> Boundaries -> (Exp Position -> Exp Objective) -> ProblemType -> Int -> Double -> Acc (Vector Ant) -> Acc (Vector Ant)
makeNewAnts gen b f pt c evr old =
  -- We compute the pheromones for the old ants.
  let ph = pheromones evr old
      -- Accumulated sum of the probabilities
      distribution = A.scanl1 (+) ph :: Acc (Vector Double)

      -- Solution size.
      n = A.length old :: Exp Int
      -- Number of new ants
      varc = constant c :: Exp Int
      -- Loop
      loop =
        A.awhile
          (\(T2 old newAnts) -> unit (A.length newAnts A.< varc))
          ( \(T2 old newAnts) ->
              lift (old, newAnts A.++ newAnt gen f evr b old distribution)
          )
          (lift (old, newAnt gen f evr b old distribution))
   in -- We return just the n best solutions
      A.take n $ sortBy (compareAnts pt) (A.afst loop A.++ A.asnd loop)

-- | Function that performs the ant colony algorithm
antColony ::
  -- | Archive size (number of ants)
  Int ->
  -- | Cardinality of the new solution
  Int ->
  -- | Boundaries
  Boundaries ->
  -- | Minimization | Maximization
  ProblemType ->
  -- | Objective function
  (Exp Position -> Exp Objective) ->
  -- | Evaporation rate
  Double ->
  -- | Learning rate (jump at the first iteration)
  Double ->
  -- | Iterations
  Int ->
  IO (Acc (Vector Ant))
antColony k c b pt f evr ijump maxit =
  do
    gen <- getStdGen
    -- We start by generating the ants positions randomly and within the boundaries
    pos <- randomArray (randomPosition b) (Z :. k) :: IO (Vector Position)
    let positions = A.use pos :: Acc (Vector Position)
        -- We compute the function value of each position
        objective = A.map f positions :: Acc (Vector Objective)
        -- Now, we create and sort the ants
        ants = sortBy (compareAnts pt) $ A.zip positions objective :: Acc (Vector Ant)

        -- We embed some varibles into the type of scalar operations Exp
        -- Parameter representing the iterations of the algorithm loop.
        maxite = constant maxit :: Exp Int
        -- Parameter representing the evaporation of the trail
        evaporation_rate = constant evr
        --
        -- Parameter representing the jump at the first iteration (learning rate)
        alpha = uniformSample gen (- ijump, ijump) :: Double
        --
        -- Now, we create a container for the variables that are going to be updated
        -- at each iteration
        container = newContainer 0 alpha :: Container
        -- Algorithm loop
        loop =
          A.afst $
            A.awhile
              (\(T2 _ container) -> A.map (A.< maxite) (unit (getIt container)))
              ( \(T2 old container) ->
                  let new = makeNewAnts gen b f pt c evr old :: Acc (Vector Ant)
                   in T2 new (updateContainer container 1 evaporation_rate)
              )
              (T2 ants container)
     in return loop

prueba :: IO ()
prueba = do
  ants <- antColony 10 4 ((0, 10), (0, 10), (0, 10)) Minimization funct 0.9 1 100
  print $ CPU.run $ unit $ ants A.!! 0
  where
    funct :: Exp Position -> Exp Objective
    funct (Position x y z) = x ** 2 + y ** 2 + z ** 2
    funct _ = 0.0

testIO :: IO ()
testIO = do
  gen <- createSystemRandom
  print $ uniformSample gen (0,1)
  print $ uniformSample gen (0,1)
  print $ uniformSample gen (0,1)
  print $ uniformSample gen (0,1)
  print $ gaussianSample gen 0 1
  print $ gaussianSample gen 0 1
  print $ gaussianSample gen 0 1
  print $ gaussianSample gen 0 1
  print $ gaussianSample gen 0 1
