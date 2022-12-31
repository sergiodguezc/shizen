{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

--
-- Generators for various distributions of particle positions
--
module Shizen.AntColony.Random
  (
    module Shizen.AntColony.Random
  )
where

import Control.Monad.ST
import System.IO.Unsafe
import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC ((:~>))
import Shizen.AntColony.Types
import System.Random.MWC (uniformR)

fixBounds :: Boundaries -> Exp Position -> Exp Position
fixBounds (b1, b2, b3) (Position x y z) =
  let T2 minb1 maxb1 = constant b1
      T2 minb2 maxb2 = constant b2
      T2 minb3 maxb3 = constant b3
      --
      x' = x A.< minb1 ? (minb1, x A.> maxb1 ? (maxb1, x))
      y' = y A.< minb2 ? (minb2, y A.> maxb2 ? (maxb2, y))
      z' = z A.< minb3 ? (minb3, z A.> maxb3 ? (maxb3, z))
   in Position x' y' z'
fixBounds _ _ = error "Error: Cannot update position"

updatePosition :: Boundaries -> Exp Double -> Exp Double -> Exp Double -> Exp Position -> Exp Position
updatePosition (b1, b2, b3) dx dy dz (Position x y z) =
  let T2 minb1 maxb1 = constant b1
      T2 minb2 maxb2 = constant b2
      T2 minb3 maxb3 = constant b3
      -- x' = ((x + dx) A.> minb1 A.&& (x + dx) A.< maxb1) ? (x + dx, x)
      -- y' = ((y + dy) A.> minb2 A.&& (y + dy) A.< maxb2) ? (y + dy, y)
      -- z' = ((z + dz) A.> minb3 A.&& (z + dz) A.< maxb3) ? (z + dz, z)
      x' = (x + dx) A.< minb1 ? (minb1, (x + dx) A.> maxb1 ? (maxb1, x + dx))
      y' = (y + dy) A.< minb2 ? (minb2, (y + dy) A.> maxb2 ? (maxb2, y + dy))
      z' = (z + dz) A.< minb3 ? (minb3, (z + dz) A.> maxb3 ? (maxb3, z + dz))
   in Position x' y' z'
updatePosition _ _ _ _ _ = error "Error: Cannot update position"

-- | Points distributed uniformly
randomPosition :: (Shape sh) => Boundaries -> sh :~> Position
randomPosition b _ix gen =
  do
    x <- uniformR (getFirstBound b) gen
    y <- uniformR (getSecondBound b) gen
    z <- uniformR (getThirdBound b) gen
    return $ Position_ x y z
