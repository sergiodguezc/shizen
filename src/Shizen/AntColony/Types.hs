{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

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
  ( Vec3 (..),
    module Shizen.AntColony.Types,
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear.V3
import Data.Primitive.Vec

--------------------------------------------------------------------
--  Basic types to be used
--------------------------------------------------------------------

-- |  Optimization problem: whether the objective function
-- has to be minimized, or maximized.
data ProblemType = Minimization | Maximization deriving (Show)

-- | Data type that stores the search space bound
type Bound = (Double, Double)

type Boundaries = (Bound, Bound, Bound)

-- Getters
getFirstBound :: Boundaries -> Bound
getFirstBound (b, _, _) = b

getSecondBound :: Boundaries -> Bound
getSecondBound (_, b, _) = b

getThirdBound :: Boundaries -> Bound
getThirdBound (_, _, b) = b

-- | Loop Container
type Container = Acc (Scalar (Int, Double))

-- Constructors
newContainer :: Int -> Double -> Container
newContainer it evr = unit $ constant (it, evr)

newContainerExp :: Exp Int -> Exp Double -> Container
newContainerExp it evr = unit $ lift (it, evr)

-- Update Container
updateContainer :: Container -> Exp Int -> Exp Double -> Container
updateContainer c it evr =
  let it' = getIt c + it
      alpha' = getEvr c * evr
   in newContainerExp it' alpha'

-- Getters
getIt :: Container -> Exp Int
getIt c = A.fst $ the c

getEvr :: Container -> Exp Double
getEvr c = A.snd $ the c

-- | A representation of the exact location in the space (R^3).
data Position where
  Position_ :: Double -> Double -> Double -> Position
  deriving (Generic, Show)

instance Elt Position

pattern Position :: Exp Double -> Exp Double -> Exp Double -> Exp Position
pattern Position x y z = Pattern (x, y, z)

--
-- Getters

getPosition :: Exp Ant -> Exp Position
getPosition (T2 p _) = p

getObjective :: Exp Ant -> Exp Objective
getObjective (T2 _ o) = o

getx :: Exp Position -> Exp Double
getx (Position x _ _) = x
getx _ = error "error getx"

gety :: Exp Position -> Exp Double
gety (Position _ y _) = y
gety _ = error "error gety"

getz :: Exp Position -> Exp Double
getz (Position _ _ z) = z
getz _ = error "error getz"

getComponent :: Exp Int -> Exp Position -> Exp Double
getComponent 0 p = getx p
getComponent 1 p = gety p
getComponent 2 p = getz p
getComponent _ _ = error "error getComponent"

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
type Objective = Double

-- | A representation of the ant on each iteration.
type Ant = (Position, Objective)

-- | Sorting the ants
compareAnts :: ProblemType -> Exp Ant -> Exp Ant -> Exp Ordering
compareAnts Minimization a1 a2 =
  let ob1 = A.snd a1
      ob2 = A.snd a2
   in A.compare ob1 ob2
compareAnts Maximization a1 a2 =
  let ob1 = A.snd a1
      ob2 = A.snd a2
   in A.compare (- ob1) (- ob2)
