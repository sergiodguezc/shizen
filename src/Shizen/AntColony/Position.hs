{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

--
-- Generators for various distributions of particle positions
--
module Shizen.AntColony.Position ( 
    randomPosition
  , compareAnts
  )
where

import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.System.Random.MWC ((:~>))
import Shizen.AntColony.Types       as T
import Data.Array.Accelerate        as A
import Prelude                      as P
import System.Random.MWC (uniformR)

-- | Points distributed uniformly
randomPosition :: (Shape sh) => Boundaries -> sh :~> Position
randomPosition b _ix gen =
  do
    x <- uniformR (b P.!! 0) gen
    y <- uniformR (b P.!! 1) gen
    z <- uniformR (b P.!! 2) gen
    return $ Position_ x y z

-- | Sorting the ants
compareAnts :: ProblemType -> Exp Ant -> Exp Ant -> Exp Ordering
compareAnts Minimization a1 a2 = let ob1 = A.snd a1
                                     ob2 = A.snd a2
                                     in A.compare ob1 ob2
compareAnts Maximization a1 a2 = let ob1 = A.snd a1
                                     ob2 = A.snd a2
                                     in A.compare ob2 ob1
