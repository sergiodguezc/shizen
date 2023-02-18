{-# LANGUAGE FunctionalDependencies #-}
module Shizen.RiverFormationDynamics.Types 
    ( module Shizen.RiverFormationDynamics.Types,
      module Shizen.Types
    )
where

import Shizen.Types
import Data.Array.Accelerate as A

-- | This type represents a solution of the optimization problem
type Drop p = (p, R, Int)

-- Drop getters
getPosition :: Elt p => Exp (Drop p) -> Exp p
getPosition (T3 p _ _) = p

getObjective :: Elt p => Exp (Drop p) -> Exp R
getObjective (T3 _ o _) = o

getNumSteps :: Elt p => Exp (Drop p) -> Exp Int
getNumSteps (T3 _ _ n) = n

-- | Type synonim for a vector of drops
type VectorDrop p = Vector (Drop p)

class Position p b => DropPosition p b | p -> b, b -> p where
    -- Function that narrows the boundaries of a Boundaries type
    -- given a percentage of the boundaries to be removed and 
    -- a position in which the new boundaries are centered.
    narrowBoundaries :: Exp R -> Exp p -> Exp b -> Exp b

