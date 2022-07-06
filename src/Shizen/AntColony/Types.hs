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

{-# LANGUAGE MultiParamTypeClasses #-}

module Shizen.AntColony.Types (
    Position
  , Objective
  , Ant
  , ProblemType
  , ObjectiveFunction
) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC as R

--------------------------------------------------------------------
--  Basic types to be used 
--------------------------------------------------------------------

-- | A type of optimization problem: whether the objective function
-- has to be miminized, or maximized.
data ProblemType = Minimization | Maximization deriving (Show, Prelude.Eq)

-- | A representation of the exact location in the space (R^n).
--
type Position = Vector Double

-- | A representation of pheromones. 
--
-- type Pheromones = 

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
--
type Objective = Scalar Double


-- | A representation of the ant on each iteration.
-- 
type Ant = (Position, Objective)

-- | A function to evaluate a genome should be an instance of
-- 'ObjectiveFunction' class. It may be called a cost function for minimization
-- problems, or a fitness function for maximization problems.
--
-- | Evaluate all fitness (cost) values at once.
--
class ObjectiveFunction where
    eval :: Position -> Objective
