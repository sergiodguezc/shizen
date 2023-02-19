{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module    : Shizen.Types.Types
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
-- |
-- |
module Shizen.Types
  ( module Shizen.Types,
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

-- | Loop Container
type Container = Acc (Scalar Int, Gen)

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
type Objective = R

class Elt a => Boundaries a where
  -- Function that creates a Boundaries type from a value
  fromValue :: R -> a
  -- Function that creates a Boundaries type from a unique Bound
  fromBound :: Bound -> a



class (Boundaries b, Elt p) => Position p b | p -> b, b -> p where
    projection :: Exp Int -> Exp p -> Exp R
    pmap :: (Exp R -> Exp R) -> Exp p -> Exp p
    psum :: Exp p -> Exp R
    pprod :: Exp p -> Exp R
    fixBounds :: Exp b -> Exp p -> Exp p
    randomPosition :: (Monad m) => Exp b -> RandomT m (Acc SFC.Gen) (Acc (Vector p))
    prod :: Exp p -> Exp p -> Exp p
    difference :: Exp p -> Exp p -> Exp p
    add :: Exp p -> Exp p -> Exp p
