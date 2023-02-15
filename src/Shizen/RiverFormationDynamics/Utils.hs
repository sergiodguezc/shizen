module Shizen.RiverFormationDynamics.Utils 
    ( module Shizen.RiverFormationDynamics.Utils,
      module Shizen.Utils
    )
where

import Shizen.Utils
import Data.Array.Accelerate as A
import Shizen.RiverFormationDynamics.Types

-- | Function which compares the ants. The boolean represents whether 
-- the ordering is descending or ascending.
compareDrops :: Position p b => Exp ProblemType -> Exp (Drop p) -> Exp (Drop p) -> Exp Ordering
compareDrops cond a1 a2 =
  let ob1 = getObjective a1
      ob2 = getObjective a2
   in cond ? (A.compare ob1 ob2, A.compare ob2 ob1)
{-# INLINE compareDrops #-}
