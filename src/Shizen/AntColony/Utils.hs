{-# LANGUAGE ScopedTypeVariables #-}

module Shizen.AntColony.Utils
  (
    module Shizen.AntColony.Utils,
    module Shizen.Utils
  )
where

import Data.Array.Accelerate as A
import Shizen.AntColony.Types
import Shizen.Utils
import Data.Array.Accelerate.Data.Sort.MyMerge
import Data.Array.Accelerate.Unsafe (undef)

-- | Function which compares the ants. The boolean represents whether 
-- the ordering is descending or ascending.
compareAnts :: AntPosition p b => Exp ProblemType -> Exp (Ant p) -> Exp (Ant p) -> Exp Ordering
compareAnts cond a1 a2 =
  let ob1 = getObjective a1
      ob2 = getObjective a2
   in cond ? (A.compare ob1 ob2, A.compare ob2 ob1)
{-# INLINE compareAnts #-}
