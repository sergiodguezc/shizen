{-# LANGUAGE MultiParamTypeClasses #-}

module Shizen.AntColony.Positions
  ( module Shizen.AntColony.Positions,
    module Shizen.Positions,
  )
where

import Shizen.AntColony.Types
import Shizen.Positions
import Data.Array.Accelerate as A
import Shizen.AntColony.Utils
import Data.Array.Accelerate.System.Random.SFC

-- R1
instance AntPosition P1 B1 where
  updatePosition evr b refAntPos positions gen1 =
    -- Stddev
    let sdx = evr * averageD2 0 refAntPos positions :: Exp Double
        -- Means
        mx = projection 0 refAntPos :: Exp Double
        --
        (xs, gen2) = runRandom gen1 (randomNVector mx sdx)
        --
        pos = A.map (fixBounds b) $ A.map P1 xs
     in (pos, gen2)

-- R2

instance AntPosition P2 B2 where
  updatePosition evr b refAntPos positions gen1 =
    -- Stddev
    let sdx = evr * averageD2 0 refAntPos positions :: Exp Double
        sdy = evr * averageD2 1 refAntPos positions :: Exp Double
        -- Means
        mx = projection 0 refAntPos :: Exp Double
        my = projection 1 refAntPos :: Exp Double
        --
        (xs, gen2) = runRandom gen1 (randomNVector mx sdx)
        (ys, gen3) = runRandom gen2 (randomNVector my sdy)
        --
        pos = A.map (fixBounds b) $ A.zipWith P2 xs ys
     in (pos, gen3)


-- R3

instance AntPosition P3 B3 where
  updatePosition evr b refAntPos positions gen1 =
    -- Stddev
    let sdx = evr * averageD2 0 refAntPos positions :: Exp Double
        sdy = evr * averageD2 1 refAntPos positions :: Exp Double
        sdz = evr * averageD2 2 refAntPos positions :: Exp Double
        -- Means
        mx = projection 0 refAntPos :: Exp Double
        my = projection 1 refAntPos :: Exp Double
        mz = projection 2 refAntPos :: Exp Double
        --
        (xs, gen2) = runRandom gen1 (randomNVector mx sdx)
        (ys, gen3) = runRandom gen2 (randomNVector my sdy)
        (zs, gen4) = runRandom gen3 (randomNVector mz sdz)
        --
        pos = A.map (fixBounds b) $ A.zipWith3 P3 xs ys zs
     in (pos, gen4)

-- R4

instance AntPosition P4 B4 where
  updatePosition evr b refAntPos ants gen1 =
    -- Stddev
    let sdx1 = evr * averageD2 0 refAntPos ants :: Exp Double
        sdx2 = evr * averageD2 1 refAntPos ants :: Exp Double
        sdx3 = evr * averageD2 2 refAntPos ants :: Exp Double
        sdx4 = evr * averageD2 3 refAntPos ants :: Exp Double
        -- Means
        mx1 = projection 0 refAntPos :: Exp Double
        mx2 = projection 1 refAntPos :: Exp Double
        mx3 = projection 2 refAntPos :: Exp Double
        mx4 = projection 3 refAntPos :: Exp Double
        --
        (x1s, gen2) = runRandom gen1 (randomNVector mx1 sdx1)
        (x2s, gen3) = runRandom gen2 (randomNVector mx2 sdx2)
        (x3s, gen4) = runRandom gen3 (randomNVector mx3 sdx3)
        (x4s, gen5) = runRandom gen4 (randomNVector mx4 sdx4)
        --
        pos = A.map (fixBounds b) $ A.zipWith4 P4 x1s x2s x3s x4s
     in (pos, gen5)


-- R5

instance AntPosition P5 B5 where
  updatePosition evr b refAntPos ants gen1 =
    -- Stddev
    let sdx1 = evr * averageD2 0 refAntPos ants :: Exp Double
        sdx2 = evr * averageD2 1 refAntPos ants :: Exp Double
        sdx3 = evr * averageD2 2 refAntPos ants :: Exp Double
        sdx4 = evr * averageD2 3 refAntPos ants :: Exp Double
        sdx5 = evr * averageD2 4 refAntPos ants :: Exp Double
        -- Means
        mx1 = projection 0 refAntPos :: Exp Double
        mx2 = projection 1 refAntPos :: Exp Double
        mx3 = projection 2 refAntPos :: Exp Double
        mx4 = projection 3 refAntPos :: Exp Double
        mx5 = projection 4 refAntPos :: Exp Double
        --
        (x1s, gen2) = runRandom gen1 (randomNVector mx1 sdx1)
        (x2s, gen3) = runRandom gen2 (randomNVector mx2 sdx2)
        (x3s, gen4) = runRandom gen3 (randomNVector mx3 sdx3)
        (x4s, gen5) = runRandom gen4 (randomNVector mx4 sdx4)
        (x5s, gen6) = runRandom gen5 (randomNVector mx5 sdx5)
        --
        pos = A.map (fixBounds b) $ A.zipWith5 P5 x1s x2s x3s x4s x5s
     in (pos, gen6)
