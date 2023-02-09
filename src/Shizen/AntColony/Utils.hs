{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Shizen.AntColony.Utils
  (
    module Shizen.AntColony.Utils
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC as MWC
import Shizen.AntColony.Types
import Data.Array.Accelerate.System.Random.SFC as SFC
import Control.Monad.State


-- | Generator
createGenerator :: Int -> IO (Acc SFC.Gen)
createGenerator n = createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)

-- | Ant getters
getPosition :: Elt p => Exp (Ant p) -> Exp p
getPosition = A.fst

getObjective :: Elt p => Exp (Ant p) -> Exp R
getObjective = A.snd

-- | Container
newContainer :: Exp Int -> Exp R -> Acc SFC.Gen -> Container
newContainer it evr gen = A.lift (unit $ A.lift (it, evr), gen)
{-# INLINE newContainer #-}

-- Update Container
updateContainer :: Container -> Exp Int -> Exp R -> Acc SFC.Gen -> Container
updateContainer c it evr gen =
  let it' = getIt c + it
      alpha' = getEvr c * evr
   in newContainer it' alpha' gen
{-# INLINE updateContainer #-}

-- Container Getters
getIt :: Container -> Exp Int
getIt c = A.fst $ the $ A.afst c
{-# INLINE getIt #-}

getEvr :: Container -> Exp R
getEvr c = A.snd $ the $ A.afst c
{-# INLINE getEvr #-}

getGen :: Container -> Acc SFC.Gen
getGen = A.asnd

-- | Function that computes the average distance between all ants and the
-- selected one at one component
averageD2 ::
  forall p b. Position p b =>
  -- | index
  Exp Int ->
  -- | selected ant
  Exp (Ant p) ->
  -- | ants
  Acc (VectorAnt p) ->
  Exp R
averageD2 i a ants =
  let value = projection i $ getPosition a :: Exp Double
      sums = A.sum $ compute $ A.map (\ant -> A.abs (projection i $ getPosition ant) - value) ants :: Acc (Scalar Double)
      elems = A.fromIntegral $ A.length ants - 1 :: Exp Double
   in the $ A.map (A./ elems) sums


-- | Function which compares the ants. The boolean represents whether 
-- the ordering is descending or ascending.
compareAnts :: Position p b => Exp ProblemType -> Exp (Ant p) -> Exp (Ant p) -> Exp Ordering
compareAnts cond a1 a2 =
  let ob1 = getObjective a1
      ob2 = getObjective a2
   in cond ? (A.compare ob1 ob2, A.compare ob2 ob1)
{-# INLINE compareAnts #-}

{- Instances -}

-- R3

data P3 where
    P3_ :: R -> R -> R -> P3
    deriving (Generic, Show)

instance Elt P3

pattern P3 :: Exp R -> Exp R -> Exp R -> Exp P3
pattern P3 x y z = Pattern (x, y, z)

data B3 where
    B3_ :: Bound -> Bound -> Bound -> B3
    deriving (Generic, Show)

instance Elt B3

pattern B3 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp B3
pattern B3 x y z = Pattern (x, y, z)

instance Boundaries B3

instance Position P3 B3 where
    projection i (P3 x y z) = (i A.== 0) ? (x, (i A.== 1) ? (y, z))
    projection _ _ = error "Error: projection"

    fixBounds (B3 b1 b2 b3) (P3 x y z) =
      let T2 minb1 maxb1 = b1
          T2 minb2 maxb2 = b2
          T2 minb3 maxb3 = b3
          --
          x' = x A.< minb1 ? (minb1, x A.> maxb1 ? (maxb1, x))
          y' = y A.< minb2 ? (minb2, y A.> maxb2 ? (maxb2, y))
          z' = z A.< minb3 ? (minb3, z A.> maxb3 ? (maxb3, z))
       in P3 x' y' z'
    fixBounds _ _ = error "Error: Cannot update position"

    randomPosition (B3 b1 b2 b3) = RandomT . StateT $ \(Gen s) ->
      let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
          (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
          (zs, s3) = A.unzip $ A.map (uniformRange b3) s2
        in return (A.zipWith3 P3 xs ys zs, Gen s3)
    randomPosition _ = error "Error: randomPosition"

    updatePosition evr b refAnt ants gen1 =
      -- Stddev
      let sdx = evr * averageD2 0 refAnt ants :: Exp Double
          sdy = evr * averageD2 1 refAnt ants :: Exp Double
          sdz = evr * averageD2 2 refAnt ants :: Exp Double
          -- Means
          mx = projection 0 $ getPosition refAnt :: Exp Double
          my = projection 1 $ getPosition refAnt :: Exp Double
          mz = projection 2 $ getPosition refAnt :: Exp Double
          --
          (xs, gen2) = runRandom gen1 (randomNVector mx sdx)
          (ys, gen3) = runRandom gen2 (randomNVector my sdy)
          (zs, gen4) = runRandom gen3 (randomNVector mz sdz)
          --
          pos = A.map (fixBounds b) $ A.zipWith3 P3 xs ys zs
      in (pos, gen4)

-- R4

data P4 where
    P4_ :: R -> R -> R -> R -> P4
    deriving (Generic, Show)

instance Elt P4

pattern P4 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp P4
pattern P4 x1 x2 x3 x4 = Pattern (x1, x2, x3, x4)

data B4 where
    B4_ :: Bound -> Bound -> Bound -> Bound -> B4
    deriving (Generic, Show)

instance Elt B4

pattern B4 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B4
pattern B4 x1 x2 x3 x4 = Pattern (x1, x2, x3, x4)

instance Boundaries B4

instance Position P4 B4 where
    projection i (P4 x1 x2 x3 x4) = (i A.== 0) ? (x1, (i A.== 1) ? (x2, (i A.== 2) ? (x3, x4)))
    projection _ _ = error "Error: projection"

    fixBounds (B4 b1 b2 b3 b4) (P4 x1 x2 x3 x4) =
      let T2 minb1 maxb1 = b1
          T2 minb2 maxb2 = b2
          T2 minb3 maxb3 = b3
          T2 minb4 maxb4 = b4
          --
          x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
          x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
          x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
          x4' = x4 A.< minb3 ? (minb3, x4 A.> maxb3 ? (maxb3, x4))
       in P4 x1' x2' x3' x4'
    fixBounds _ _ = error "Error: Cannot update position"

    randomPosition (B4 b1 b2 b3 b4) = RandomT . StateT $ \(Gen s) ->
      let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
          (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
          (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
          (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        in return (A.zipWith4 P4 x1s x2s x3s x4s, Gen s4)
    randomPosition _ = error "Error: randomPosition"

    updatePosition evr b refAnt ants gen1 =
      -- Stddev
      let sdx1 = evr * averageD2 0 refAnt ants :: Exp Double
          sdx2 = evr * averageD2 1 refAnt ants :: Exp Double
          sdx3 = evr * averageD2 2 refAnt ants :: Exp Double
          sdx4 = evr * averageD2 3 refAnt ants :: Exp Double
          -- Means
          mx1 = projection 0 $ getPosition refAnt :: Exp Double
          mx2 = projection 1 $ getPosition refAnt :: Exp Double
          mx3 = projection 2 $ getPosition refAnt :: Exp Double
          mx4 = projection 3 $ getPosition refAnt :: Exp Double
          --
          (x1s, gen2) = runRandom gen1 (randomNVector mx1 sdx1)
          (x2s, gen3) = runRandom gen2 (randomNVector mx2 sdx2)
          (x3s, gen4) = runRandom gen3 (randomNVector mx3 sdx3)
          (x4s, gen5) = runRandom gen4 (randomNVector mx4 sdx4)
          --
          pos = A.map (fixBounds b) $ A.zipWith4 P4 x1s x2s x3s x4s
      in (pos, gen5)
