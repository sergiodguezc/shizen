--------------------------------------------------------------------
-- |
-- Module    : Shizen.AntColony.Main
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Shizen.AntColony.Main (
    -- antColony
  -- , Objective
) where

import Data.Array.Accelerate as A
import Shizen.AntColony.Types
import Data.Array.Accelerate.System.Random.MWC

-- Function that performs the ant colony algorithm
-- antColony :: (ObjectiveFunction objectivefn) =>
-- antColony :: (ObjectiveFunction objectivefn) =>
--              Int ->                -- Archive size (number of ants)
--              Int ->                -- Dimension
--              Double ->             -- MaxBound
--              ProblemType ->        -- Minimization | Maximization
--              objectivefn ->  -- Objective function
--              Double ->             -- Evaporation rate
--              Double ->             -- Stopping criteria
--              IO Objective             -- Final value
-- antColony k n b f e s = do
--     ants <- initializeAnts k n b
--     obj <- A.map f.eval ants :: Vector Objective
--     return $ obj ! 1
--
initializeAnts :: Int ->               -- Archive size (number of ants)
                  Int ->               -- Dimension
                  Double ->            -- MaxBound
                  objectivefn ->       -- Objective function
                  IO (Vector Ant)
initializeAnts k n b f = do positions <- randomArray (uniformR (-b,b)) (Z :. n :. k) :: IO (Matrix Double)
                            join generate (index1 n) (\_ -> A.zip (use positions) (use (A.map f.eval positions)))

