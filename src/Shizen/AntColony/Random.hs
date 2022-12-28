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
import System.Random
-- import System.Random.MWC
--     ( uniformR,
--       create,
--       asGenST,
--       withSystemRandomST,
--       Gen,
--       createSystemSeed,
--       restore,
--       createSystemRandom )
-- import System.Random.MWC.Distributions (normal)
-- import System.Random.MWC (GenIO)
-- import System.Random.MWC (GenST)
-- import System.Random.MWC (withSystemRandom)

updatePosition :: Boundaries -> Exp Double -> Exp Double -> Exp Double -> Exp Position -> Exp Position
updatePosition (b1, b2, b3) dx dy dz (Position x y z) =
  let T2 b11 b12 = constant b1
      T2 b21 b22 = constant b2
      T2 b31 b32 = constant b3
      x' = x + dx A.> b11 A.&& x + dx A.< b12 ? (x + dx, x)
      y' = y + dy A.> b21 A.&& y + dy A.< b22 ? (y + dy, y)
      z' = z + dz A.> b31 A.&& z + dz A.< b32 ? (z + dz, z)
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

-- | Pure function which returns a random 'Double' within the bounds.
-- TODO: use a monad
--
-- uniformSample :: GenST s -> Bound -> Double
-- uniformSample :: Bound -> Double
uniformSample :: GenIO -> Bound -> Double
uniformSample gen b = 0 -- runST $ do
  -- (asGenST $ \gen' -> uniformR b gen') gen

-- TODO: use a monad
gaussianSample ::
  StdGen ->
  -- | Mean
  Double ->
  -- | Standard deviation
  Double ->
  Double
gaussianSample gen m sd = unsafePerformIO $ do
  normal m sd gen
