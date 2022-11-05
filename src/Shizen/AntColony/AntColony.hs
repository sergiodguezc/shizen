{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------
-- |
-- Module    : Shizen.AntColony.AntColony
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
--
-- An implementation of Ant Colony optimization algorithm for continuous
-- functions.
--------------------------------------------------------------------
module Shizen.AntColony.AntColony where

import Data.Array.Accelerate as A
--
import Data.Array.Accelerate.Data.Sort.Quick
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.System.Random.MWC as R
import Shizen.AntColony.Position
import Shizen.AntColony.Types
import Prelude as P

-- | Function that performs the ant colony algorithm
antColony ::
  Int -> -- Archive size (number of ants)
  Boundaries -> -- Boundaries
  ProblemType -> -- Minimization | Maximization
  (Exp Position -> Exp Objective) -> -- Objective function
  Double -> -- Evaporation rate
  Int -> -- Iterations
  IO (Exp Objective)
antColony k b pt f e it =
  do
    -- We start by generating the ants positions randomly and within the boundaries
    pos <- randomArray (randomPosition b) (Z :. k) :: IO (Vector Position)
    let positions = A.use pos :: Acc (Vector Position)
        -- We compute the function value of each position
        objective = A.map f positions :: Acc (Vector Objective)

        -- Now, we create and sort the ants
        ants =
          sortBy (compareAnts pt) $
            A.zip positions objective ::
            Acc (Vector Ant)

        bestAnt = ants A.!! 0

        -- Algorithm loop
        loop =
          -- A.asnd $
          --   A.awhile
          --     (A.uncurry keepGoing (lift it))
          --     ( \as ->
          --         let (_, old) = unlift as :: (Acc (Vector Ant), Acc (Vector Ant))
          --             new = makeNewAnts old
          --          in lift (old, new)
          --     )
          --     -- Caso base
          --     (lift (ants, makeNewAnts ants)) ::
          --   (Acc (Vector Ant), Acc (Vector Ant))
    return 0.0
  where
    -- Function that calculates the next generation of ants
    makeNewAnts :: Acc (Vector Ant) -> Acc (Vector Ant)
    makeNewAnts old = old

    -- Loop condition
    keepGoing :: Exp Double -> Acc (Vector Ant) -> Acc (Vector Ant) -> Acc (Scalar Bool)
    keepGoing sc xs ys =
      A.or $
        A.zipWith
          ( \a1 a2 ->
              let T2 x1 y1 = unlift a1
                  T2 x2 y2 = unlift a2
               in abs (y1 - y2) A.> sc
          )
          xs
          ys

--  Prueba ants
-- prueba = do
--   ants <- antColony 3 [(-10.0, 10.0), (-20.0, 20.0), (-30.0, 50.0)] Minimization funct 0.0 0.0
--   return $ CPU.run ants
--   where
--     funct :: Exp Position -> Exp Objective
--     funct (Position x y z) = x + y + z
--     funct _ = 0.0
