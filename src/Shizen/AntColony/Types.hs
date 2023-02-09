{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module    : Shizen.AntColony.Types
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
-- |
-- |
module Shizen.AntColony.Types
  ( module Shizen.AntColony.Types,
  )
where

import Data.Array.Accelerate as A
import Control.Monad.State
import Data.Array.Accelerate.System.Random.SFC as SFC

--------------------------------------------------------------------
--  Basic types to be used
--------------------------------------------------------------------

-- | Not all compute devices support double precision. To simplify device
-- switching, change the type R to Float.
type R = Double

-- |  Optimization problem: whether the objective function
-- has to be minimized, or maximized.
type ProblemType = Bool

-- | Data type that stores the search space bound
type Bound = (R, R)

-- | Type used to represent the ant.
type Ant p = (p, R)

type VectorAnt p = Vector (Ant p)

-- | Loop Container
type Container = Acc (Scalar (Int, R), Gen)

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
type Objective = R

class Elt a => Boundaries a

class (Boundaries b, Elt p) => Position p b | p -> b, b -> p where
    projection :: Exp Int -> Exp p -> Exp R
    fixBounds :: Exp b -> Exp p -> Exp p
    randomPosition :: (Monad m) => Exp b -> RandomT m (Acc (Vector p))
    updatePosition :: Exp R -> Exp b -> Exp (Ant p) -> Acc (VectorAnt p) -> Acc Gen -> (Acc (Vector p), Acc Gen)

