--------------------------------------------------------------------
-- |
-- Module    : Shizen.AntColony.Types
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Shizen.AntColony.Types (
    Path
  , Position
  , Objective
  , Ant
) where

import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.System.Random.MWC

--------------------------------------------------------------------
--  Basic types to be used 
--------------------------------------------------------------------

-- | A representation of the exact location in the space (R^n).
--
type Position = Vector Double

-- | A representation of a solution. 
--
type Path = Vector Position

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
--
type Objective = Exp Double

-- | A representation of the ant on each iteration.
-- 
type Ant = ()

-- | A function that measures the performance of a solution.
--
class ObjectiveFunction f where
    evalObjective :: f -> [Path] -> Objective

