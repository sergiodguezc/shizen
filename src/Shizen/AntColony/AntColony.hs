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
  ( module Shizen.AntColony.AntColony,
  )
where

import Control.Monad.ST
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Sort.Quick
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.System.Random.MWC as R
import Shizen.AntColony.Random
import Shizen.AntColony.Types
import System.Random as SR
import System.Random.MWC as RS

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

normal :: (RandomGen a) => a -> Exp Double -> Exp Double -> Exp Double
normal gen m sd =
  let (g1, g2) = split gen
      u1 = constant $ Prelude.fst $ SR.randomR (0, 1) g1 :: Exp Double
      u2 = constant $ Prelude.fst $ SR.randomR (0, 1) g2 :: Exp Double
      -- Box-Muller transformation: z = N(0,1)
      z = A.sqrt (-2 * A.log u1) * A.cos (2 * A.pi * u2)
   in -- z*sd + m = N(m,sd)
      z * sd + m

newAnt :: (RandomGen a) => a -> (Exp Position -> Exp Objective) -> Double -> Boundaries -> Acc (Vector Ant) -> Acc (Vector Double) -> Acc (Vector Ant)
newAnt gen f evr b ants distribution =
  let (g1, g2) = split gen
      (g3, g4) = split g1
      --
      evre = constant evr :: Exp Double
      --
      probability = constant $ Prelude.fst $ SR.randomR (0, 1) g1 :: Exp Double
      refAnt = pickAnt probability distribution ants :: Exp Ant
      -- refPos = A.fst refAnt
      --
      -- dx = constant $ Prelude.fst $ SR.randomR (- evr, evr) g2 :: Exp Double
      -- dy = constant $ Prelude.fst $ SR.randomR (- evr, evr) g3 :: Exp Double
      -- dz = constant $ Prelude.fst $ SR.randomR (- evr, evr) g4 :: Exp Double
      --
      -- Stddev
      sdx = evre * averageD2 0 refAnt ants :: Exp Double
      sdy = evre * averageD2 1 refAnt ants :: Exp Double
      sdz = evre * averageD2 2 refAnt ants :: Exp Double
      -- Means
      mx = getx $ getPosition refAnt :: Exp Double
      my = gety $ getPosition refAnt :: Exp Double
      mz = getz $ getPosition refAnt :: Exp Double
      --
      (g5, g6) = split g3
      (g7, g8) = split g4
      -- --
      newx = normal g5 mx sdx :: Exp Double
      newy = normal g6 my sdy :: Exp Double
      newz = normal g7 mz sdz :: Exp Double 
      --
      newPos = fixBounds b $ Position newx newy newz
      -- newPos = updatePosition b dx dy dz refPos :: Exp Position
      newAnts = flatten . unit $ lift (newPos, f newPos) :: Acc (Vector Ant)
   in newAnts


-- TODO: Fix this function.
averageD2 ::
  -- | index
  Int ->
  -- | selected ant
  Exp Ant ->
  -- | ants
  Acc (Vector Ant) ->
  Exp Double
averageD2 i a ants =
  let value = getComponent i $ getPosition a :: Exp Double
      sums = A.sum $ compute $ A.map (\a -> A.abs (getComponent i $ getPosition a) - value) ants :: Acc (Scalar Double)
      elems = A.fromIntegral $ A.length ants - 1 :: Exp Double
   in the $ A.map (A./ elems) sums

-- | Function which returns a random ant given the pheremones and a probability.
pickAnt :: Exp Double -> Acc (Vector Double) -> Acc (Vector Ant) -> Exp Ant
pickAnt prob distribution ants =
  -- We match each value of the distribution with its index and filter out
  -- values for which the probability is less than the value of the
  -- distribution.
  let ipairs = A.afst $ A.filter (\(T2 _ d) -> d A.< prob) (indexed distribution)
      -- We retrieve the first index that satisfies the above property
      i = unindex1 $ A.fst $ ipairs A.!! 0
      n = A.length ants - 1
      k = i A.> n ? (n, i)
   in ants A.!! k

-- Function that calculates the next generation of ants
makeNewAnts :: (RandomGen a) => a -> Boundaries -> (Exp Position -> Exp Objective) -> ProblemType -> Int -> Double -> Acc (Vector Ant) -> Acc (Vector Ant)
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
          (\(T2 _ na) -> unit $ A.length na A.< varc)
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
    -- We start by generating the ants positions randomly and within the boundaries
    -- gen <- createSystemRandom
    gen <- initStdGen
    g2 <- createSystemRandom
    pos <- randomArrayWith g2 (randomPosition b) (Z :. k) :: IO (Vector Position)
    let positions = A.use pos :: Acc (Vector Position)
        -- We compute the function value of each position
        objective = A.map f positions :: Acc (Vector Objective)

        ants = sortBy (compareAnts pt) $ A.zip positions objective :: Acc (Vector Ant)

        -- We embed some varibles into the type of scalar operations Exp
        -- Parameter representing the iterations of the algorithm loop.
        maxite = constant maxit :: Exp Int
        -- Parameter representing the evaporation of the trail
        evaporation_rate = constant evr
        --
        -- Parameter representing the jump at the first iteration (learning rate)
        alpha = ijump :: Double
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
  ants <- antColony 10 4 ((0, 10), (0, 10), (0, 10)) Minimization funct 0.95 1 100
  print $ CPU.run $ unit $ ants A.!! 0
  where
    funct :: Exp Position -> Exp Objective
    funct (Position x y z) = x ** 2 + y ** 2 + z ** 2
    funct _ = 0.0
