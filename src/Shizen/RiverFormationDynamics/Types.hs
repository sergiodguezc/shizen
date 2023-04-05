{-# LANGUAGE FunctionalDependencies #-}
module Shizen.RiverFormationDynamics.Types 
    ( module Shizen.RiverFormationDynamics.Types,
      module Shizen.Types
    )
where

import Shizen.Types
import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.SFC (Gen)

-- | This type represents a solution of the optimization problem
type Drop p = (p, R, Int)

-- Drop getters
getPosition :: Elt p => Exp (Drop p) -> Exp p
getPosition (T3 p _ _) = p

getObjective :: Elt p => Exp (Drop p) -> Exp R
getObjective (T3 _ o _) = o

getNumSteps :: Elt p => Exp (Drop p) -> Exp Int
getNumSteps (T3 _ _ n) = n

decreaseNumSteps :: Elt p => Exp (Drop p) -> Exp (Drop p)
decreaseNumSteps (T3 p o n) = T3 p o (n - 1)

-- | Loop containter for the RFD algorithm
-- We also need to store the new range factor
type RFDContainer = Acc (Scalar R, Scalar Int, Gen)

newRFDContainer :: Exp R -> Exp Int -> Acc Gen -> RFDContainer
newRFDContainer rf it gen = A.lift (unit rf, unit it, gen)
-- {-# INLINE newContainer #-}

-- Update Container
updateRFDContainer :: RFDContainer -> Exp R -> Exp Int -> Acc Gen -> RFDContainer
updateRFDContainer c rf it gen =
  let it' = getRFDIt c + it
   in newRFDContainer rf it' gen
-- {-# INLINE updateContainer #-}

-- Container Getters
getRFDIt :: RFDContainer -> Exp Int
getRFDIt (T3 _ it _) = the it
-- {-# INLINE getIt #-}

getRFDGen :: RFDContainer -> Acc Gen
getRFDGen (T3 _ _ g) = g
-- {-# INLINE getGen #-}

getRFDRangeFactor :: RFDContainer -> Exp R
getRFDRangeFactor (T3 rf _ _) = the rf


-- | Type synonym for a vector of drops
type VectorDrop p = Vector (Drop p)

class Position p b => DropPosition p b | p -> b, b -> p where
    -- Function that narrows the boundaries of a Boundaries type
    -- given a percentage of the boundaries to be removed and 
    -- a position in which the new boundaries are centered.
    narrowBoundaries :: Exp R -> Exp p -> Exp b -> Exp b


instance DropPosition P1 B1 where
    narrowBoundaries p (P1 x) (B1 b) =
      let T2 minb1 maxb1 = b
          --
          range = (maxb1 - minb1) * p
          --
          minb1' = x - (range A./ 2)
          maxb1' = x + (range A./ 2)
       in B1 (T2 minb1' maxb1')
    narrowBoundaries _ _ _ = error "Error: narrowBoundaries"
  
instance DropPosition P2 B2 where
    narrowBoundaries p (P2 x y) (B2 b1 b2) =
      let T2 minb1 maxb1 = b1
          T2 minb2 maxb2 = b2
          --
          range1 = (maxb1 - minb1) * p
          range2 = (maxb2 - minb2) * p
          --
          minb1' = x - (range1 A./ 2)
          maxb1' = x + (range1 A./ 2)
          minb2' = y - (range2 A./ 2)
          maxb2' = y + (range2 A./ 2)
       in B2 (T2 minb1' maxb1') (T2 minb2' maxb2')
    narrowBoundaries _ _ _ = error "Error: narrowBoundaries"

instance DropPosition P3 B3 where
  narrowBoundaries p (P3 x y z) (B3 b1 b2 b3) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        --
        range1 = (maxb1 - minb1) * p
        range2 = (maxb2 - minb2) * p
        range3 = (maxb3 - minb3) * p
        --
        minb1' = x - (range1 A./ 2)
        maxb1' = x + (range1 A./ 2)
        minb2' = y - (range2 A./ 2)
        maxb2' = y + (range2 A./ 2)
        minb3' = z - (range3 A./ 2)
        maxb3' = z + (range3 A./ 2)
     in B3 (T2 minb1' maxb1') (T2 minb2' maxb2') (T2 minb3' maxb3')
  narrowBoundaries _ _ _ = error "Error: narrowBoundaries"

instance DropPosition P4 B4 where
  narrowBoundaries p (P4 x y z w) (B4 b1 b2 b3 b4) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        --
        range1 = (maxb1 - minb1) * p
        range2 = (maxb2 - minb2) * p
        range3 = (maxb3 - minb3) * p
        range4 = (maxb4 - minb4) * p
        --
        minb1' = x - (range1 A./ 2)
        maxb1' = x + (range1 A./ 2)
        minb2' = y - (range2 A./ 2)
        maxb2' = y + (range2 A./ 2)
        minb3' = z - (range3 A./ 2)
        maxb3' = z + (range3 A./ 2)
        minb4' = w - (range4 A./ 2)
        maxb4' = w + (range4 A./ 2)
     in B4 (T2 minb1' maxb1') (T2 minb2' maxb2') (T2 minb3' maxb3') (T2 minb4' maxb4')
  narrowBoundaries _ _ _ = error "Error: narrowBoundaries"

instance DropPosition P5 B5 where
  narrowBoundaries p (P5 x y z w u) (B5 b1 b2 b3 b4 b5) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        --
        range1 = (maxb1 - minb1) * p
        range2 = (maxb2 - minb2) * p
        range3 = (maxb3 - minb3) * p
        range4 = (maxb4 - minb4) * p
        range5 = (maxb5 - minb5) * p
        --
        minb1' = x - (range1 A./ 2)
        maxb1' = x + (range1 A./ 2)
        minb2' = y - (range2 A./ 2)
        maxb2' = y + (range2 A./ 2)
        minb3' = z - (range3 A./ 2)
        maxb3' = z + (range3 A./ 2)
        minb4' = w - (range4 A./ 2)
        maxb4' = w + (range4 A./ 2)
        minb5' = u - (range5 A./ 2)
        maxb5' = u + (range5 A./ 2)
     in B5 (T2 minb1' maxb1') (T2 minb2' maxb2') (T2 minb3' maxb3') (T2 minb4' maxb4') (T2 minb5' maxb5')
  narrowBoundaries _ _ _ = error "Error: narrowBoundaries"
