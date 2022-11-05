{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

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
    Vec3(..)
  , module Shizen.AntColony.Types
) where

-- import Data.Array.Accelerate.Linear.V3
import Data.Primitive.Vec
import Data.Array.Accelerate                as A
import Prelude                              as P

--------------------------------------------------------------------
--  Basic types to be used 
--------------------------------------------------------------------

-- |  Optimization problem: whether the objective function
-- has to be minimized, or maximized.
data ProblemType = Minimization | Maximization deriving Show

-- | Data type that stores the search space bound
--
type Boundaries = [(Double, Double)]

-- | A representation of the exact location in the space (R^3).
--
data Position where
  Position_ :: Double -> Double -> Double -> Position
  deriving (Generic, Elt, Show)

pattern Position :: Exp Double -> Exp Double -> Exp Double -> Exp Position
pattern Position x y z = Pattern (x,y,z)
-- | A representation of pheromones. 
--
-- type Pheromones = 

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
--
type Objective =  Double

-- | A representation of the ant on each iteration.
-- 
type Ant = (Position, Objective)

-- instance P.Eq Ant where
--     (==) (Ant (_,ob1)) (Ant (_, ob2)) = ob1 P.== ob2
--
-- instance P.Ord Ant where
--     compare (Ant (_,ob1)) (Ant (_, ob2)) = P.compare ob1 ob2
--
-- | A function to evaluate a position should be an instance of
-- 'ObjectiveFunction' class. It may be called a cost function for minimization
-- problems, or a fitness function for maximization problems.
--
-- class ObjectiveFunction where
--     eval :: Exp Position -> Exp Objective

