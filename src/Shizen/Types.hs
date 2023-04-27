{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module    : Shizen.Types.Types
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
-- |
-- |
module Shizen.Types
  ( module Shizen.Types,
  )
where

import Control.Monad.State
import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.SFC as SFC
import Prelude as P

--------------------------------------------------------------------
--  Basic types to be used
--------------------------------------------------------------------

-- | Not all compute devices support double precision. To simplify device
-- switching, change the type R to Float.
type R = Float
-- |  Optimization problem: whether the objective function
-- has to be minimized, or maximized.
type ProblemType = Bool

-- | Data type that stores the search space bound
type Bound = (R, R)

-- | A measure of the observed performance. It may be called cost for
-- minimization problems, or fitness for maximization problems.
type Objective = R


-- Function that creates a Boundaries type from a value
fromValue :: Boundaries a => R -> a
fromValue v = fromBound (-v, v)

-- Function that fix the bounds of one dimension
fixBound :: Exp R -> Exp Bound -> Exp R
fixBound x (T2 l u) = A.max l (A.min u x)

class Elt a => Boundaries a where
  -- Function that creates a Boundaries type from a unique Bound
  fromBound :: Bound -> a

class (Boundaries b, Elt p) => Position p b | p -> b, b -> p where
  projection :: Exp Int -> Exp p -> Exp R
  pmap :: (Exp R -> Exp R) -> Exp p -> Exp p
  psum :: Exp p -> Exp R
  pprod :: Exp p -> Exp R
  fixBounds :: Exp b -> Exp p -> Exp p
  randomPositions :: (Monad m) => Exp b -> RandomT m (Acc SFC.Gen) (Acc (Vector p))
  randomPosition :: Exp b -> Exp SFC64 -> Exp (p, SFC64)
  prod :: Exp p -> Exp p -> Exp p
  difference :: Exp p -> Exp p -> Exp p
  add :: Exp p -> Exp p -> Exp p
  showContent :: p -> String
  boundariesDiameters :: Exp b -> Exp p
  toBoundaries :: Exp p -> Exp b


{- Instances -}

-- R1
data P1 where
  P1_ :: R -> P1
  deriving (Generic)

instance Show P1 where
  show p = "P1 (" P.++ showContent p P.++ ")"

instance Elt P1

pattern P1 :: Exp R -> Exp P1
pattern P1 x = Pattern x

data B1 where
  B1_ :: Bound -> B1
  deriving (Generic, Show)

instance Elt B1

pattern B1 :: Exp Bound -> Exp B1
pattern B1 x = Pattern x

instance Boundaries B1 where
  fromBound b = B1_ b

instance Position P1 B1 where
  toBoundaries (P1 x) = B1 (A.lift (-x, x))
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B1 b) = P1 (A.snd b - A.fst b)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B1 b) gen =
    let T2 x gen' = uniformRange b gen
     in A.lift (P1 x, gen')
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P1_ x) = show x
  pmap f (P1 x) = P1 (f x)
  pmap _ _ = error "Error pmap"

  psum (P1 x) = x
  psum _ = error "Error psum"

  pprod (P1 x) = x
  pprod _ = error "Error pmap"

  projection _ (P1 x) = x
  projection _ _ = error "Error: projection"

  fixBounds (B1 b) (P1 x) =
    let x' = fixBound x b
     in P1 x'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPositions (B1 b) = RandomT . StateT $ \s ->
    let (xs, s1) = A.unzip $ A.map (uniformRange b) s
     in return (A.map P1 xs, s1)
  randomPositions _ = error "Error: randomPositions"

  prod (P1 x) (P1 y) = P1 (x * y)
  prod _ _ = error "Error: prod"

  difference (P1 x) (P1 y) = P1 (x - y)
  difference _ _ = error "Error: difference"

  add (P1 x) (P1 y) = P1 (x + y)
  add _ _ = error "Error: sum"

-- R2

data P2 where
  P2_ :: R -> R -> P2
  deriving (Generic)

instance Show P2 where
  show p = "P2 (" P.++ showContent p P.++ ")"

instance Elt P2

pattern P2 :: Exp R -> Exp R -> Exp P2
pattern P2 x y = Pattern (x, y)

data B2 where
  B2_ :: Bound -> Bound -> B2
  deriving (Generic, Show)

instance Elt B2

pattern B2 :: Exp Bound -> Exp Bound -> Exp B2
pattern B2 x y = Pattern (x, y)

instance Boundaries B2 where
  fromBound b = B2_ b b

instance Position P2 B2 where
  toBoundaries (P2 x y) = B2 (A.lift (-x, x)) (A.lift (-y, y))
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B2 b1 b2) = P2 (A.snd b1 - A.fst b1) (A.snd b2 - A.fst b2)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B2 b1 b2) gen =
    let T2 x1 gen' = uniformRange b1 gen
        T2 x2 gen'' = uniformRange b2 gen'
     in A.lift (P2 x1 x2, gen'')
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P2_ x y) = show x P.++ ", " P.++ show y
  pmap f (P2 x y) = P2 (f x) (f y)
  pmap _ _ = error "Error pmap"

  psum (P2 x y) = x + y
  psum _ = error "Error psum"

  pprod (P2 x y) = x * y
  pprod _ = error "Error pmap"

  projection i (P2 x y) = (i A.== 0) ? (x, y)
  projection _ _ = error "Error: projection"

  fixBounds (B2 b1 b2) (P2 x y) =
    let x' = fixBound x b1
        y' = fixBound y b2
     in P2 x' y'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPositions (B2 b1 b2) = RandomT . StateT $ \s ->
    let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
        (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
     in return (A.zipWith P2 xs ys, s2)
  randomPositions _ = error "Error: randomPositions"

  prod (P2 x y) (P2 x' y') = P2 (x * x') (y * y')
  prod _ _ = error "Error: prod"

  difference (P2 x y) (P2 x' y') = P2 (x - x') (y - y')
  difference _ _ = error "Error: difference"

  add (P2 x y) (P2 x' y') = P2 (x + x') (y + y')
  add _ _ = error "Error: sum"

-- R3

data P3 where
  P3_ :: R -> R -> R -> P3
  deriving (Generic)

instance Show P3 where
  show p = "P3 (" P.++ showContent p P.++ ")"

instance Elt P3

pattern P3 :: Exp R -> Exp R -> Exp R -> Exp P3
pattern P3 x y z = Pattern (x, y, z)

data B3 where
  B3_ :: Bound -> Bound -> Bound -> B3
  deriving (Generic, Show)

instance Elt B3

pattern B3 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp B3
pattern B3 x y z = Pattern (x, y, z)

instance Boundaries B3 where
  fromBound b = B3_ b b b

instance Position P3 B3 where
  toBoundaries (P3 x y z) = B3 (A.lift (-x, x)) (A.lift (-y, y)) (A.lift (-z, z))
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B3 b1 b2 b3) = P3 (A.snd b1 - A.fst b1) (A.snd b2 - A.fst b2) (A.snd b3 - A.fst b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B3 b1 b2 b3) gen =
    let T2 x1 gen' = uniformRange b1 gen
        T2 x2 gen'' = uniformRange b2 gen'
        T2 x3 gen''' = uniformRange b3 gen''
     in A.lift (P3 x1 x2 x3, gen''')
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P3_ x y z) = show x P.++ ", " P.++ show y P.++ ", " P.++ show z
  pmap f (P3 x y z) = P3 (f x) (f y) (f z)
  pmap _ _ = error "Error pmap"

  psum (P3 x y z) = x + y + z
  psum _ = error "Error psum"

  pprod (P3 x y z) = x * y * z
  pprod _ = error "Error pmap"

  projection i (P3 x y z) = (i A.== 0) ? (x, (i A.== 1) ? (y, z))
  projection _ _ = error "Error: projection"

  fixBounds (B3 b1 b2 b3) (P3 x y z) =
    let x' = fixBound x b1
        y' = fixBound y b2
        z' = fixBound z b3
     in P3 x' y' z'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPositions (B3 b1 b2 b3) = RandomT . StateT $ \s ->
    let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
        (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
        (zs, s3) = A.unzip $ A.map (uniformRange b3) s2
     in return (A.zipWith3 P3 xs ys zs, s3)
  randomPositions _ = error "Error: randomPositions"

  prod (P3 x y z) (P3 x' y' z') = P3 (x * x') (y * y') (z * z')
  prod _ _ = error "Error: prod"

  difference (P3 x y z) (P3 x' y' z') = P3 (x - x') (y - y') (z - z')
  difference _ _ = error "Error: difference"

  add (P3 x y z) (P3 x' y' z') = P3 (x + x') (y + y') (z + z')
  add _ _ = error "Error: sum"

-- R4

data P4 where
  P4_ :: R -> R -> R -> R -> P4
  deriving (Generic)

instance Show P4 where
  show p = "P4 (" P.++ showContent p P.++ ")"

instance Elt P4

pattern P4 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp P4
pattern P4 x1 x2 x3 x4 = Pattern (x1, x2, x3, x4)

data B4 where
  B4_ :: Bound -> Bound -> Bound -> Bound -> B4
  deriving (Generic, Show)

instance Elt B4

pattern B4 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B4
pattern B4 x1 x2 x3 x4 = Pattern (x1, x2, x3, x4)

instance Boundaries B4 where
  fromBound b = B4_ b b b b

instance Position P4 B4 where
  toBoundaries (P4 x1 x2 x3 x4) = B4 (A.lift (-x1, x1)) (A.lift (-x2, x2)) (A.lift (-x3, x3)) (A.lift (-x4, x4))
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B4 b1 b2 b3 b4) = P4 (A.snd b1 - A.fst b1) (A.snd b2 - A.fst b2) (A.snd b3 - A.fst b3) (A.snd b4 - A.fst b4)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B4 b1 b2 b3 b4) gen =
    let T2 x1 gen' = uniformRange b1 gen
        T2 x2 gen'' = uniformRange b2 gen'
        T2 x3 gen''' = uniformRange b3 gen''
        T2 x4 gen'''' = uniformRange b4 gen'''
     in A.lift (P4 x1 x2 x3 x4, gen'''')
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P4_ x1 x2 x3 x4) = show x1 P.++ ", " P.++ show x2 P.++ ", " P.++ show x3 P.++ ", " P.++ show x4
  pmap f (P4 x y z t) = P4 (f x) (f y) (f z) (f t)
  pmap _ _ = error "Error pmap"

  psum (P4 x y z t) = x + y + z + t
  psum _ = error "Error psum"

  pprod (P4 x y z t) = x * y * z * t
  pprod _ = error "Error pprod"

  projection i (P4 x1 x2 x3 x4) = (i A.< 2) ? (i A.< 1 ? (x1, x2), (i A.> 2) ? (x3, x4))
  projection _ _ = error "Error: projection"

  fixBounds (B4 b1 b2 b3 b4) (P4 x1 x2 x3 x4) =
    let x1' = fixBound x1 b1
        x2' = fixBound x2 b2
        x3' = fixBound x3 b3
        x4' = fixBound x4 b4
     in P4 x1' x2' x3' x4'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPositions (B4 b1 b2 b3 b4) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
     in return (A.zipWith4 P4 x1s x2s x3s x4s, s4)
  randomPositions _ = error "Error: randomPositions"

  prod (P4 x1 x2 x3 x4) (P4 x1' x2' x3' x4') = P4 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4')
  prod _ _ = error "Error: prod"

  difference (P4 x1 x2 x3 x4) (P4 x1' x2' x3' x4') = P4 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4')
  difference _ _ = error "Error: difference"

  add (P4 x1 x2 x3 x4) (P4 x1' x2' x3' x4') = P4 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4')
  add _ _ = error "Error: sum"

-- R5

data P5 where
  P5_ :: R -> R -> R -> R -> R -> P5
  deriving (Generic)

instance Show P5 where
  show p = "P5 (" P.++ showContent p P.++ ")"

instance Elt P5

pattern P5 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P5
pattern P5 x1 x2 x3 x4 x5 = Pattern (x1, x2, x3, x4, x5)

data B5 where
  B5_ :: Bound -> Bound -> Bound -> Bound -> Bound -> B5
  deriving (Generic, Show)

instance Elt B5

pattern B5 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B5
pattern B5 x1 x2 x3 x4 x5 = Pattern (x1, x2, x3, x4, x5)

instance Boundaries B5 where
  fromBound b = B5_ b b b b b

instance Position P5 B5 where
  toBoundaries (P5 x1 x2 x3 x4 x5) =
    let b1 = A.lift (-x1, x1)
        b2 = A.lift (-x2, x2)
        b3 = A.lift (-x3, x3)
        b4 = A.lift (-x4, x4)
        b5 = A.lift (-x5, x5)
    in B5 b1 b2 b3 b4 b5
  toBoundaries _ = error "Error: toBoundaries"


  boundariesDiameters (B5 b1 b2 b3 b4 b5) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
     in P5 (maxb1 - minb1) (maxb2 - minb2) (maxb3 - minb3) (maxb4 - minb4) (maxb5 - minb5)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B5 b1 b2 b3 b4 b5) gen =
    let T2 x1 gen1 = uniformRange b1 gen
        T2 x2 gen2 = uniformRange b2 gen1
        T2 x3 gen3 = uniformRange b3 gen2
        T2 x4 gen4 = uniformRange b4 gen3
        T2 x5 gen5 = uniformRange b5 gen4
     in A.lift (P5 x1 x2 x3 x4 x5, gen5)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P5_ x1 x2 x3 x4 x5) = show x1 P.++ ", " P.++ show x2 P.++ ", " P.++ show x3 P.++ ", " P.++ show x4 P.++ ", " P.++ show x5
  pmap f (P5 x1 x2 x3 x4 x5) = P5 (f x1) (f x2) (f x3) (f x4) (f x5)
  pmap _ _ = error "Error pmap"

  psum (P5 x1 x2 x3 x4 x5) = x1 + x2 + x3 + x4 + x5
  psum _ = error "Error psum"

  pprod (P5 x1 x2 x3 x4 x5) = x1 * x2 * x3 * x4 * x5
  pprod _ = error "Error pprod"

  projection i (P5 x1 x2 x3 x4 x5) = i A.< 2 ? (i A.< 1 ? (x1, x2), i A.< 4 ? (i A.< 3 ? (x3, x4), x5))
  projection _ _ = error "Error: projection"

  fixBounds (B5 b1 b2 b3 b4 b5) (P5 x1 x2 x3 x4 x5) =
    let x1' = fixBound x1 b1
        x2' = fixBound x2 b2
        x3' = fixBound x3 b3
        x4' = fixBound x4 b4
        x5' = fixBound x5 b5
     in P5 x1' x2' x3' x4' x5'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPositions (B5 b1 b2 b3 b4 b5) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
     in return (A.zipWith5 P5 x1s x2s x3s x4s x5s, s5)
  randomPositions _ = error "Error: randomPositions"

  prod (P5 x1 x2 x3 x4 x5) (P5 x1' x2' x3' x4' x5') = P5 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4') (x5 * x5')
  prod _ _ = error "Error: prod"

  difference (P5 x1 x2 x3 x4 x5) (P5 x1' x2' x3' x4' x5') = P5 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4') (x5 - x5')
  difference _ _ = error "Error: difference"

  add (P5 x1 x2 x3 x4 x5) (P5 x1' x2' x3' x4' x5') = P5 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4') (x5 + x5')
  add _ _ = error "Error: sum"

-- R6

data P6 where
  P6_ :: R -> R -> R -> R -> R -> R -> P6
  deriving (Generic)

instance Show P6 where
  show p = "P6 (" P.++ showContent p P.++ ")"

instance Elt P6

pattern P6 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P6
pattern P6 x1 x2 x3 x4 x5 x6 = Pattern (x1, x2, x3, x4, x5, x6)

data B6 where
  B6_ :: Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> B6
  deriving (Generic, Show)

instance Elt B6

pattern B6 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B6
pattern B6 x1 x2 x3 x4 x5 x6 = Pattern (x1, x2, x3, x4, x5, x6)

instance Boundaries B6 where
  fromBound b = B6_ b b b b b b

instance Position P6 B6 where
  toBoundaries (P6 x1 x2 x3 x4 x5 x6) =
    let b1 = A.lift (-x1, x1)
        b2 = A.lift (-x2, x2)
        b3 = A.lift (-x3, x3)
        b4 = A.lift (-x4, x4)
        b5 = A.lift (-x5, x5)
        b6 = A.lift (-x6, x6)
    in B6 b1 b2 b3 b4 b5 b6
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B6 b1 b2 b3 b4 b5 b6) = P6 (A.snd b1 - A.fst b1) (A.snd b2 - A.fst b2) (A.snd b3 - A.fst b3) (A.snd b4 - A.fst b4) (A.snd b5 - A.fst b5) (A.snd b6 - A.fst b6)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B6 b1 b2 b3 b4 b5 b6) gen =
    let T2 x1 gen1 = uniformRange b1 gen
        T2 x2 gen2 = uniformRange b2 gen1
        T2 x3 gen3 = uniformRange b3 gen2
        T2 x4 gen4 = uniformRange b4 gen3
        T2 x5 gen5 = uniformRange b5 gen4
        T2 x6 gen6 = uniformRange b6 gen5
     in A.lift (P6 x1 x2 x3 x4 x5 x6, gen6)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P6_ x1 x2 x3 x4 x5 x6) = show x1 P.++ ", " P.++ show x2 P.++ ", " P.++ show x3 P.++ ", " P.++ show x4 P.++ ", " P.++ show x5 P.++ ", " P.++ show x6
  pmap f (P6 x1 x2 x3 x4 x5 x6) = P6 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6)
  pmap _ _ = error "Error pmap"

  psum (P6 x1 x2 x3 x4 x5 x6) = x1 + x2 + x3 + x4 + x5 + x6
  psum _ = error "Error psum"

  pprod (P6 x1 x2 x3 x4 x5 x6) = x1 * x2 * x3 * x4 * x5 * x6
  pprod _ = error "Error pprod"

  projection i (P6 x0 x1 x2 x3 x4 x5) = i A.< 3 ? (i A.< 2 ? (i A.< 1 ? (x0, x1), x2), i A.< 5 ? (i A.< 4 ? (x3, x4), x5))
  projection _ _ = error "Error: projection"

  fixBounds (B6 b1 b2 b3 b4 b5 b6) (P6 x1 x2 x3 x4 x5 x6) =
    let x1' = fixBound x1 b1
        x2' = fixBound x2 b2
        x3' = fixBound x3 b3
        x4' = fixBound x4 b4
        x5' = fixBound x5 b5
        x6' = fixBound x6 b6
     in P6 x1' x2' x3' x4' x5' x6'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPositions (B6 b1 b2 b3 b4 b5 b6) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
     in return (A.zipWith6 P6 x1s x2s x3s x4s x5s x6s, s6)
  randomPositions _ = error "Error: randomPositions"

  prod (P6 x1 x2 x3 x4 x5 x6) (P6 x1' x2' x3' x4' x5' x6') = P6 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4') (x5 * x5') (x6 * x6')
  prod _ _ = error "Error: prod"

  difference (P6 x1 x2 x3 x4 x5 x6) (P6 x1' x2' x3' x4' x5' x6') = P6 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4') (x5 - x5') (x6 - x6')
  difference _ _ = error "Error: difference"

  add (P6 x1 x2 x3 x4 x5 x6) (P6 x1' x2' x3' x4' x5' x6') = P6 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4') (x5 + x5') (x6 + x6')
  add _ _ = error "Error: sum"

-- R7

data P7 where
  P7_ :: R -> R -> R -> R -> R -> R -> R -> P7
  deriving (Generic)

instance Show P7 where
  show p = "P7 (" P.++ showContent p P.++ ")"

instance Elt P7

pattern P7 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P7
pattern P7 x1 x2 x3 x4 x5 x6 x7 = Pattern (x1, x2, x3, x4, x5, x6, x7)

data B7 where
  B7_ :: Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> B7
  deriving (Generic, Show)

instance Elt B7

pattern B7 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B7
pattern B7 x1 x2 x3 x4 x5 x6 x7 = Pattern (x1, x2, x3, x4, x5, x6, x7)

instance Boundaries B7 where
  fromBound b = B7_ b b b b b b b

instance Position P7 B7 where
  toBoundaries (P7 x1 x2 x3 x4 x5 x6 x7) =
    let b1 = A.lift (-x1, x1)
        b2 = A.lift (-x2, x2)
        b3 = A.lift (-x3, x3)
        b4 = A.lift (-x4, x4)
        b5 = A.lift (-x5, x5)
        b6 = A.lift (-x6, x6)
        b7 = A.lift (-x7, x7)
    in B7 b1 b2 b3 b4 b5 b6 b7
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B7 b1 b2 b3 b4 b5 b6 b7) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
     in P7 (maxb1 - minb1) (maxb2 - minb2) (maxb3 - minb3) (maxb4 - minb4) (maxb5 - minb5) (maxb6 - minb6) (maxb7 - minb7)
  boundariesDiameters _ = error "Error: boundariesDiameters"
  randomPosition (B7 b1 b2 b3 b4 b5 b6 b7) gen =
    let T2 x1 gen1 = uniformRange b1 gen
        T2 x2 gen2 = uniformRange b2 gen1
        T2 x3 gen3 = uniformRange b3 gen2
        T2 x4 gen4 = uniformRange b4 gen3
        T2 x5 gen5 = uniformRange b5 gen4
        T2 x6 gen6 = uniformRange b6 gen5
        T2 x7 gen7 = uniformRange b7 gen6
     in A.lift (P7 x1 x2 x3 x4 x5 x6 x7, gen7)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P7_ x1 x2 x3 x4 x5 x6 x7) = show x1 P.++ ", " P.++ show x2 P.++ ", " P.++ show x3 P.++ ", " P.++ show x4 P.++ ", " P.++ show x5 P.++ ", " P.++ show x6 P.++ ", " P.++ show x7
  pmap f (P7 x1 x2 x3 x4 x5 x6 x7) = P7 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)
  pmap _ _ = error "Error pmap"

  psum (P7 x1 x2 x3 x4 x5 x6 x7) = x1 + x2 + x3 + x4 + x5 + x6 + x7
  psum _ = error "Error psum"

  pprod (P7 x1 x2 x3 x4 x5 x6 x7) = x1 * x2 * x3 * x4 * x5 * x6 * x7
  pprod _ = error "Error pprod"

  -- Binary search for projection
  projection i (P7 x0 x1 x2 x3 x4 x5 x6) = i A.< 4 ? (i A.< 2 ? (i A.< 1 ? (x0, x1), i A.< 3 ? (x2, x3)), i A.< 6 ? (i A.< 5 ? (x4, x5), x6))
  projection _ _ = error "Error: projection"

  fixBounds (B7 b1 b2 b3 b4 b5 b6 b7) (P7 x1 x2 x3 x4 x5 x6 x7) =
    let x1' = fixBound x1 b1
        x2' = fixBound x2 b2
        x3' = fixBound x3 b3
        x4' = fixBound x4 b4
        x5' = fixBound x5 b5
        x6' = fixBound x6 b6
        x7' = fixBound x7 b7
     in P7 x1' x2' x3' x4' x5' x6' x7'
  fixBounds _ _ = error "Error: fixBounds"

  randomPositions (B7 b1 b2 b3 b4 b5 b6 b7) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
     in return (A.zipWith7 P7 x1s x2s x3s x4s x5s x6s x7s, s7)
  randomPositions _ = error "Error: randomPositions"

  prod (P7 x1 x2 x3 x4 x5 x6 x7) (P7 x1' x2' x3' x4' x5' x6' x7') = P7 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4') (x5 * x5') (x6 * x6') (x7 * x7')
  prod _ _ = error "Error: prod"

  difference (P7 x1 x2 x3 x4 x5 x6 x7) (P7 x1' x2' x3' x4' x5' x6' x7') = P7 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4') (x5 - x5') (x6 - x6') (x7 - x7')
  difference _ _ = error "Error: difference"

  add (P7 x1 x2 x3 x4 x5 x6 x7) (P7 x1' x2' x3' x4' x5' x6' x7') = P7 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4') (x5 + x5') (x6 + x6') (x7 + x7')
  add _ _ = error "Error: sum"

-- R8

data P8 where
  P8_ :: R -> R -> R -> R -> R -> R -> R -> R -> P8
  deriving (Generic)

instance Show P8 where
  show p = "P8 (" P.++ showContent p P.++ ")"

instance Elt P8

pattern P8 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P8
pattern P8 x1 x2 x3 x4 x5 x6 x7 x8 = Pattern (x1, x2, x3, x4, x5, x6, x7, x8)

data B8 where
  B8_ :: Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> B8
  deriving (Generic, Show)

instance Elt B8

pattern B8 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B8
pattern B8 b1 b2 b3 b4 b5 b6 b7 b8 = Pattern (b1, b2, b3, b4, b5, b6, b7, b8)

instance Boundaries B8 where
  fromBound b = B8_ b b b b b b b b

instance Position P8 B8 where
  toBoundaries (P8 x1 x2 x3 x4 x5 x6 x7 x8) =
    let b1 = A.lift (-x1, x1)
        b2 = A.lift (-x2, x2)
        b3 = A.lift (-x3, x3)
        b4 = A.lift (-x4, x4)
        b5 = A.lift (-x5, x5)
        b6 = A.lift (-x6, x6)
        b7 = A.lift (-x7, x7)
        b8 = A.lift (-x8, x8)
    in B8 b1 b2 b3 b4 b5 b6 b7 b8
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B8 b1 b2 b3 b4 b5 b6 b7 b8) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
        T2 minb8 maxb8 = b8
     in P8 (maxb1 - minb1) (maxb2 - minb2) (maxb3 - minb3) (maxb4 - minb4) (maxb5 - minb5) (maxb6 - minb6) (maxb7 - minb7) (maxb8 - minb8)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B8 b1 b2 b3 b4 b5 b6 b7 b8) gen =
    let T2 x1 gen1 = uniformRange b1 gen
        T2 x2 gen2 = uniformRange b2 gen1
        T2 x3 gen3 = uniformRange b3 gen2
        T2 x4 gen4 = uniformRange b4 gen3
        T2 x5 gen5 = uniformRange b5 gen4
        T2 x6 gen6 = uniformRange b6 gen5
        T2 x7 gen7 = uniformRange b7 gen6
        T2 x8 gen8 = uniformRange b8 gen7
     in A.lift (P8 x1 x2 x3 x4 x5 x6 x7 x8, gen8)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P8_ x1 x2 x3 x4 x5 x6 x7 x8) = show x1 P.++ ", " P.++ show x2 P.++ ", " P.++ show x3 P.++ ", " P.++ show x4 P.++ ", " P.++ show x5 P.++ ", " P.++ show x6 P.++ ", " P.++ show x7 P.++ ", " P.++ show x8
  pmap f (P8 x1 x2 x3 x4 x5 x6 x7 x8) = P8 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8)
  pmap _ _ = error "Error: pmap"

  psum (P8 x1 x2 x3 x4 x5 x6 x7 x8) = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  psum _ = error "Error: psum"

  pprod (P8 x1 x2 x3 x4 x5 x6 x7 x8) = x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8
  pprod _ = error "Error: pprod"

  -- Binary search for projection
  projection i (P8 x0 x1 x2 x3 x4 x5 x6 x7) = i A.< 4 ? (i A.< 2 ? (i A.< 1 ? (x0, x1), i A.< 3 ? (x2, x3)), i A.< 6 ? (i A.< 5 ? (x4, x5), i A.< 7 ? (x6, x7)))
  projection _ _ = error "Error: projection"

  fixBounds (B8 b1 b2 b3 b4 b5 b6 b7 b8) (P8 x1 x2 x3 x4 x5 x6 x7 x8) =
    let x1' = fixBound x1 b1
        x2' = fixBound x2 b2
        x3' = fixBound x3 b3
        x4' = fixBound x4 b4
        x5' = fixBound x5 b5
        x6' = fixBound x6 b6
        x7' = fixBound x7 b7
        x8' = fixBound x8 b8
     in P8 x1' x2' x3' x4' x5' x6' x7' x8'
  fixBounds _ _ = error "Error: fixBounds"

  randomPositions (B8 b1 b2 b3 b4 b5 b6 b7 b8) = RandomT . StateT $ \s0 ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
     in return (A.zipWith8 P8 x1s x2s x3s x4s x5s x6s x7s x8s, s8)
  randomPositions _ = error "Error: randomPositions"

  prod (P8 x1 x2 x3 x4 x5 x6 x7 x8) (P8 y1 y2 y3 y4 y5 y6 y7 y8) = P8 (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8)
  prod _ _ = error "Error: prod"

  difference (P8 x1 x2 x3 x4 x5 x6 x7 x8) (P8 y1 y2 y3 y4 y5 y6 y7 y8) = P8 (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8)
  difference _ _ = error "Error: difference"

  add (P8 x1 x2 x3 x4 x5 x6 x7 x8) (P8 y1 y2 y3 y4 y5 y6 y7 y8) = P8 (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8)
  add _ _ = error "Error: add"

-- R9
data P9 where
  P9_ :: P3 -> P3 -> P3 -> P9
  deriving (Generic)

instance Show P9 where
  show p = "P9 (" P.++ showContent p P.++ ")"

instance Elt P9

pattern P9 :: Exp P3 -> Exp P3 -> Exp P3 -> Exp P9
pattern P9 x1 x2 x3 = Pattern (x1, x2, x3)

data B9 where
  B9_ :: B3 -> B3 -> B3 -> B9
  deriving (Generic, Show)

instance Elt B9

pattern B9 :: Exp B3 -> Exp B3 -> Exp B3 -> Exp B9
pattern B9 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B9 where
  fromBound b = B9_ (fromBound b) (fromBound b) (fromBound b)

instance Position P9 B9 where
  toBoundaries (P9 x1 x2 x3) = B9 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B9 b1 b2 b3) = P9 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B9 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P3, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P3, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P3, SFC64)
     in A.lift (P9 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P9_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P9 x1 x2 x3) = P9 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P9 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P9 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  projection i (P9 x1 x2 x3) = i A.< 3 ? (projection i x1, i A.< 6 ? (projection (i - 3) x2, projection (i - 6) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B9 b1 b2 b3) (P9 x1 x2 x3) = P9 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  randomPositions (B9 b b' b'') = RandomT . StateT $ \s ->
    let B3 b1 b2 b3 = b
        B3 b4 b5 b6 = b'
        B3 b7 b8 b9 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        p3s = A.zipWith3 P3 x1s x2s x3s
        p3s' = A.zipWith3 P3 x4s x5s x6s
        p3s'' = A.zipWith3 P3 x7s x8s x9s
     in return (A.zipWith3 P9 p3s p3s' p3s'', s9)
  randomPositions _ = error "Error: randomPositions"

  difference (P9 x1 x2 x3) (P9 y1 y2 y3) = P9 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  prod (P9 x1 x2 x3) (P9 y1 y2 y3) = P9 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  add (P9 x1 x2 x3) (P9 y1 y2 y3) = P9 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: sum"

-- Representation of the 10-dimensional position and boundary types
data P10 where
  P10_ :: P3 -> P3 -> P4 -> P10
  deriving (Generic)

instance Show P10 where
  show p = "P10 (" P.++ showContent p P.++ ")"

instance Elt P10

pattern P10 :: Exp P3 -> Exp P3 -> Exp P4 -> Exp P10
pattern P10 x1 x2 x3 = Pattern (x1, x2, x3)

data B10 where
  B10_ :: B3 -> B3 -> B4 -> B10
  deriving (Generic, Show)

instance Elt B10

pattern B10 :: Exp B3 -> Exp B3 -> Exp B4 -> Exp B10
pattern B10 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B10 where
  fromBound b = B10_ (fromBound b) (fromBound b) (fromBound b)

instance Position P10 B10 where
  toBoundaries (P10 x1 x2 x3) = B10 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B10 b1 b2 b3) = P10 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B10 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P3, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P3, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P4, SFC64)
     in A.lift (P10 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P10_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P10 x1 x2 x3) = P10 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P10 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P10 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  projection i (P10 x1 x2 x3) = i A.< 3 ? (projection i x1, i A.< 6 ? (projection (i - 3) x2, projection (i - 6) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B10 b1 b2 b3) (P10 x1 x2 x3) = P10 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  randomPositions (B10 b b' b'') = RandomT . StateT $ \s ->
    let B3 b1 b2 b3 = b
        B3 b4 b5 b6 = b'
        B4 b7 b8 b9 b10 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        p3s = A.zipWith3 P3 x1s x2s x3s
        p3s' = A.zipWith3 P3 x4s x5s x6s
        p4s = A.zipWith4 P4 x7s x8s x9s x10s
     in return (A.zipWith3 P10 p3s p3s' p4s, s10)
  randomPositions _ = error "Error: randomPositions"

  difference (P10 x1 x2 x3) (P10 y1 y2 y3) = P10 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  prod (P10 x1 x2 x3) (P10 y1 y2 y3) = P10 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  add (P10 x1 x2 x3) (P10 y1 y2 y3) = P10 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: sum"

-- R11
data P11 where
  P11_ :: P3 -> P4 -> P4 -> P11
  deriving (Generic)

instance Show P11 where
  show p = "P11 (" P.++ showContent p P.++ ")"

instance Elt P11

pattern P11 :: Exp P3 -> Exp P4 -> Exp P4 -> Exp P11
pattern P11 x1 x2 x3 = Pattern (x1, x2, x3)

data B11 where
  B11_ :: B3 -> B4 -> B4 -> B11
  deriving (Generic, Show)

instance Elt B11

pattern B11 :: Exp B3 -> Exp B4 -> Exp B4 -> Exp B11
pattern B11 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B11 where
  fromBound b = B11_ (fromBound b) (fromBound b) (fromBound b)

instance Position P11 B11 where
  toBoundaries (P11 x1 x2 x3) = B11 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B11 b1 b2 b3) = P11 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B11 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P3, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P4, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P4, SFC64)
     in A.lift (P11 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P11_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P11 x1 x2 x3) = P11 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P11 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P11 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  fixBounds (B11 b1 b2 b3) (P11 x1 x2 x3) = P11 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  add (P11 x1 x2 x3) (P11 y1 y2 y3) = P11 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  difference (P11 x1 x2 x3) (P11 y1 y2 y3) = P11 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  projection i (P11 x1 x2 x3) = i A.< 3 ? (projection i x1, i A.< 7 ? (projection (i - 3) x2, projection (i - 7) x3))
  projection _ _ = error "Error: projection"

  randomPositions (B11 b b' b'') = RandomT . StateT $ \s ->
    let B3 b1 b2 b3 = b
        B4 b4 b5 b6 b7 = b'
        B4 b8 b9 b10 b11 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        p3s = A.zipWith3 P3 x1s x2s x3s
        p4s = A.zipWith4 P4 x4s x5s x6s x7s
        p4s' = A.zipWith4 P4 x8s x9s x10s x11s
     in return (A.zipWith3 P11 p3s p4s p4s', s11)
  randomPositions _ = error "Error: randomPositions"

  prod (P11 x1 x2 x3) (P11 y1 y2 y3) = P11 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R12

data P12 where
  P12_ :: P4 -> P4 -> P4 -> P12
  deriving (Generic)

instance Show P12 where
  show p = "P12 (" P.++ showContent p P.++ ")"

instance Elt P12

pattern P12 :: Exp P4 -> Exp P4 -> Exp P4 -> Exp P12
pattern P12 x1 x2 x3 = Pattern (x1, x2, x3)

data B12 where
  B12_ :: B4 -> B4 -> B4 -> B12
  deriving (Generic, Show)

instance Elt B12

pattern B12 :: Exp B4 -> Exp B4 -> Exp B4 -> Exp B12
pattern B12 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B12 where
  fromBound b = B12_ (fromBound b) (fromBound b) (fromBound b)

instance Position P12 B12 where
  toBoundaries (P12 x1 x2 x3) = B12 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B12 b1 b2 b3) = P12 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B12 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P4, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P4, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P4, SFC64)
     in A.lift (P12 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P12_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P12 x1 x2 x3) = P12 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P12 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P12 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  fixBounds (B12 b1 b2 b3) (P12 x1 x2 x3) = P12 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  add (P12 x1 x2 x3) (P12 y1 y2 y3) = P12 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  difference (P12 x1 x2 x3) (P12 y1 y2 y3) = P12 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  projection i (P12 x1 x2 x3) = i A.< 8 ? (i A.< 4 ? (projection i x1, projection (i - 4) x2), projection (i - 8) x3)
  projection _ _ = error "Error: projection"

  randomPositions (B12 b b' b'') = RandomT . StateT $ \s ->
    let B4 b1 b2 b3 b4 = b
        B4 b5 b6 b7 b8 = b'
        B4 b9 b10 b11 b12 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        p4s = A.zipWith4 P4 x1s x2s x3s x4s
        p4s' = A.zipWith4 P4 x5s x6s x7s x8s
        p4s'' = A.zipWith4 P4 x9s x10s x11s x12s
     in return (A.zipWith3 P12 p4s p4s' p4s'', s12)
  randomPositions _ = error "Error: randomPositions"

  prod (P12 x1 x2 x3) (P12 y1 y2 y3) = P12 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R13

data P13 where
  P13_ :: P4 -> P4 -> P5 -> P13
  deriving (Generic)

instance Show P13 where
  show p = "P13 (" P.++ showContent p P.++ ")"

instance Elt P13

pattern P13 :: Exp P4 -> Exp P4 -> Exp P5 -> Exp P13
pattern P13 x1 x2 x3 = Pattern (x1, x2, x3)

data B13 where
  B13_ :: B4 -> B4 -> B5 -> B13
  deriving (Generic, Show)

instance Elt B13

pattern B13 :: Exp B4 -> Exp B4 -> Exp B5 -> Exp B13
pattern B13 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B13 where
  fromBound b = B13_ (fromBound b) (fromBound b) (fromBound b)

instance Position P13 B13 where
  toBoundaries (P13 x1 x2 x3) = B13 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B13 b1 b2 b3) = P13 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B13 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P4, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P4, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P5, SFC64)
     in A.lift (P13 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P13_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P13 x1 x2 x3) = P13 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P13 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P13 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  fixBounds (B13 b b' b'') (P13 x1 x2 x3) = P13 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  add (P13 x1 x2 x3) (P13 y1 y2 y3) = P13 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  difference (P13 x1 x2 x3) (P13 y1 y2 y3) = P13 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  projection i (P13 x1 x2 x3) = i A.< 9 ? (i A.< 4 ? (projection i x1, projection (i - 5) x2), projection (i - 9) x3)
  projection _ _ = error "Error: projection"

  randomPositions (B13 b b' b'') = RandomT . StateT $ \s ->
    let B4 b1 b2 b3 b4 = b
        B4 b5 b6 b7 b8 = b'
        B5 b9 b10 b11 b12 b13 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        p4s = A.zipWith4 P4 x1s x2s x3s x4s
        p4s' = A.zipWith4 P4 x5s x6s x7s x8s
        p5s = A.zipWith5 P5 x9s x10s x11s x12s x13s
     in return (A.zipWith3 P13 p4s p4s' p5s, s13)
  randomPositions _ = error "Error: randomPositions"

  prod (P13 x1 x2 x3) (P13 y1 y2 y3) = P13 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R14

data P14 where
  P14_ :: P4 -> P5 -> P5 -> P14
  deriving (Generic)

instance Show P14 where
  show p = "P14 (" P.++ showContent p P.++ ")"

instance Elt P14

pattern P14 :: Exp P4 -> Exp P5 -> Exp P5 -> Exp P14
pattern P14 x1 x2 x3 = Pattern (x1, x2, x3)

data B14 where
  B14_ :: B4 -> B5 -> B5 -> B14
  deriving (Generic, Show)

instance Elt B14

pattern B14 :: Exp B4 -> Exp B5 -> Exp B5 -> Exp B14
pattern B14 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B14 where
  fromBound b = B14_ (fromBound b) (fromBound b) (fromBound b)

instance Position P14 B14 where
  toBoundaries (P14 x1 x2 x3) = B14 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B14 b1 b2 b3) = P14 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B14 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P4, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P5, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P5, SFC64)
     in A.lift (P14 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P14_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P14 x1 x2 x3) = P14 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P14 x1 x2 x3) (P14 y1 y2 y3) = P14 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  psum (P14 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  difference (P14 x1 x2 x3) (P14 y1 y2 y3) =
    P14 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  prod (P14 x1 x2 x3) (P14 y1 y2 y3) =
    P14 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  pprod (P14 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  projection i (P14 x1 x2 x3) = i A.< 9 ? (i A.< 4 ? (projection i x1, projection (i - 4) x2), projection (i - 9) x3)
  projection _ _ = error "Error: projection"

  fixBounds (B14 b1 b2 b3) (P14 x1 x2 x3) =
    P14 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  randomPositions (B14 b b' b'') = RandomT . StateT $ \s ->
    let B4 b1 b2 b3 b4 = b
        B5 b5 b6 b7 b8 b9 = b'
        B5 b10 b11 b12 b13 b14 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        p4s = A.zipWith4 P4 x1s x2s x3s x4s
        p5s = A.zipWith5 P5 x5s x6s x7s x8s x9s
        p5s' = A.zipWith5 P5 x10s x11s x12s x13s x14s
     in return (A.zipWith3 P14 p4s p5s p5s', s14)
  randomPositions _ = error "Error: randomPositions"

-- R15

data P15 where
  P15_ :: P5 -> P5 -> P5 -> P15
  deriving (Generic)

instance Show P15 where
  show p = "P15 (" P.++ showContent p P.++ ")"

instance Elt P15

pattern P15 :: Exp P5 -> Exp P5 -> Exp P5 -> Exp P15
pattern P15 x1 x2 x3 = Pattern (x1, x2, x3)

data B15 where
  B15_ :: B5 -> B5 -> B5 -> B15
  deriving (Generic, Show)

instance Elt B15

pattern B15 :: Exp B5 -> Exp B5 -> Exp B5 -> Exp B15
pattern B15 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B15 where
  fromBound b = B15_ (fromBound b) (fromBound b) (fromBound b)

instance Position P15 B15 where
  toBoundaries (P15 x1 x2 x3) = B15 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B15 b1 b2 b3) = P15 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B15 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P5, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P5, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P5, SFC64)
     in A.lift (P15 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P15_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P15 x1 x2 x3) = P15 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P15 x1 x2 x3) (P15 y1 y2 y3) = P15 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: padd"

  psum (P15 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P15 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  projection i (P15 x1 x2 x3) = i A.< 10 ? (i A.< 5 ? (projection i x1, projection (i - 5) x2), projection (i - 10) x3)
  projection _ _ = error "Error: projection"

  fixBounds (B15 b1 b2 b3) (P15 x1 x2 x3) = P15 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBound"

  randomPositions (B15 b b' b'') = RandomT . StateT $ \s ->
    let B5 b1 b2 b3 b4 b5 = b
        B5 b6 b7 b8 b9 b10 = b'
        B5 b11 b12 b13 b14 b15 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        p5s = A.zipWith5 P5 x1s x2s x3s x4s x5s
        p5s' = A.zipWith5 P5 x6s x7s x8s x9s x10s
        p5s'' = A.zipWith5 P5 x11s x12s x13s x14s x15s
     in return (A.zipWith3 P15 p5s p5s' p5s'', s15)
  randomPositions _ = error "Error: randomPositions"

  difference (P15 x1 x2 x3) (P15 y1 y2 y3) = P15 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  prod (P15 x1 x2 x3) (P15 y1 y2 y3) = P15 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R16

data P16 where
  P16_ :: P5 -> P5 -> P6 -> P16
  deriving (Generic)

instance Show P16 where
  show p = "P16 (" P.++ showContent p P.++ ")"

instance Elt P16

pattern P16 :: Exp P5 -> Exp P5 -> Exp P6 -> Exp P16
pattern P16 x1 x2 x3 = Pattern (x1, x2, x3)

data B16 where
  B16_ :: B5 -> B5 -> B6 -> B16
  deriving (Generic, Show)

instance Elt B16

pattern B16 :: Exp B5 -> Exp B5 -> Exp B6 -> Exp B16
pattern B16 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B16 where
  fromBound b = B16_ (fromBound b) (fromBound b) (fromBound b)

instance Position P16 B16 where
  toBoundaries (P16 x1 x2 x3) = B16 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B16 b1 b2 b3) = P16 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B16 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P5, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P5, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P6, SFC64)
     in A.lift (P16 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P16_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P16 x1 x2 x3) = P16 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P16 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P16 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P16 x1 x2 x3) (P16 y1 y2 y3) =
    P16 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  fixBounds (B16 b1 b2 b3) (P16 x1 x2 x3) =
    P16 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  projection i (P16 x1 x2 x3) = i A.< 10 ? (i A.< 5 ? (projection i x1, projection (i - 5) x2), projection (i - 10) x3)
  projection _ _ = error "Error: projection"

  add (P16 x1 x2 x3) (P16 y1 y2 y3) =
    P16 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  randomPositions (B16 b b' b'') = RandomT . StateT $ \s0 ->
    let B5 b1 b2 b3 b4 b5 = b
        B5 b6 b7 b8 b9 b10 = b'
        B6 b11 b12 b13 b14 b15 b16 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        p5s = A.zipWith5 P5 x1s x2s x3s x4s x5s
        p5s' = A.zipWith5 P5 x6s x7s x8s x9s x10s
        p6s = A.zipWith6 P6 x11s x12s x13s x14s x15s x16s
     in return (A.zipWith3 P16 p5s p5s' p6s, s16)
  randomPositions _ = error "Error: randomPositions"

  difference (P16 x1 x2 x3) (P16 y1 y2 y3) =
    P16 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

data P17 where
  P17_ :: P5 -> P6 -> P6 -> P17
  deriving (Generic)

instance Show P17 where
  show p = "P17 (" P.++ showContent p P.++ ")"

instance Elt P17

pattern P17 :: Exp P5 -> Exp P6 -> Exp P6 -> Exp P17
pattern P17 x1 x2 x3 = Pattern (x1, x2, x3)

data B17 where
  B17_ :: B5 -> B6 -> B6 -> B17
  deriving (Generic, Show)

instance Elt B17

pattern B17 :: Exp B5 -> Exp B6 -> Exp B6 -> Exp B17
pattern B17 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B17 where
  fromBound b = B17_ (fromBound b) (fromBound b) (fromBound b)

instance Position P17 B17 where
  toBoundaries (P17 x1 x2 x3) = B17 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B17 b1 b2 b3) = P17 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B17 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P5, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P6, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P6, SFC64)
     in A.lift (P17 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P17_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P17 x1 x2 x3) = P17 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P17 x1 x2 x3) (P17 y1 y2 y3) = P17 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P17 x1 x2 x3) = i A.< 5 ? (projection i x1, i A.< 11 ? (projection (i - 5) x2, projection (i - 11) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B17 b1 b2 b3) (P17 x1 x2 x3) = P17 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P17 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P17 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P17 x1 x2 x3) (P17 y1 y2 y3) = P17 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B17 b b' b'') = RandomT . StateT $ \s0 ->
    let B5 b1 b2 b3 b4 b5 = b
        B6 b6 b7 b8 b9 b10 b11 = b'
        B6 b12 b13 b14 b15 b16 b17 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        p5s = A.zipWith5 P5 x1s x2s x3s x4s x5s
        p6s = A.zipWith6 P6 x6s x7s x8s x9s x10s x11s
        p6s' = A.zipWith6 P6 x12s x13s x14s x15s x16s x17s
     in return (A.zipWith3 P17 p5s p6s p6s', s17)
  randomPositions _ = error "Error: randomPositions"

  difference (P17 x1 x2 x3) (P17 y1 y2 y3) = P17 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R18
data P18 where
  P18_ :: P6 -> P6 -> P6 -> P18
  deriving (Generic)

instance Show P18 where
  show p = "P18 (" P.++ showContent p P.++ ")"

instance Elt P18

pattern P18 :: Exp P6 -> Exp P6 -> Exp P6 -> Exp P18
pattern P18 x1 x2 x3 = Pattern (x1, x2, x3)

data B18 where
  B18_ :: B6 -> B6 -> B6 -> B18
  deriving (Generic, Show)

instance Elt B18

pattern B18 :: Exp B6 -> Exp B6 -> Exp B6 -> Exp B18
pattern B18 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B18 where
  fromBound b = B18_ (fromBound b) (fromBound b) (fromBound b)

instance Position P18 B18 where
  toBoundaries (P18 x1 x2 x3) = B18 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B18 b1 b2 b3) = P18 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B18 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P6, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P6, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P6, SFC64)
     in A.lift (P18 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P18_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P18 x1 x2 x3) = P18 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P18 x1 x2 x3) (P18 y1 y2 y3) = P18 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P18 x1 x2 x3) = i A.< 6 ? (projection i x1, i A.< 12 ? (projection (i - 6) x2, projection (i - 12) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B18 b1 b2 b3) (P18 x1 x2 x3) = P18 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P18 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P18 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P18 x1 x2 x3) (P18 y1 y2 y3) = P18 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B18 b b' b'') = RandomT . StateT $ \s0 -> do
    let B6 b1 b2 b3 b4 b5 b6 = b
        B6 b7 b8 b9 b10 b11 b12 = b'
        B6 b13 b14 b15 b16 b17 b18 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        p6s = A.zipWith6 P6 x1s x2s x3s x4s x5s x6s
        p6s' = A.zipWith6 P6 x7s x8s x9s x10s x11s x12s
        p6s'' = A.zipWith6 P6 x13s x14s x15s x16s x17s x18s
     in return (A.zipWith3 P18 p6s p6s' p6s'', s18)
  randomPositions _ = error "Error: randomPositions"

  difference (P18 x1 x2 x3) (P18 y1 y2 y3) = P18 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R19
data P19 where
  P19_ :: P6 -> P6 -> P7 -> P19
  deriving (Generic)

instance Show P19 where
  show p = "P19 (" P.++ showContent p P.++ ")"

instance Elt P19

pattern P19 :: Exp P6 -> Exp P6 -> Exp P7 -> Exp P19
pattern P19 x1 x2 x3 = Pattern (x1, x2, x3)

data B19 where
  B19_ :: B6 -> B6 -> B7 -> B19
  deriving (Generic, Show)

instance Elt B19

pattern B19 :: Exp B6 -> Exp B6 -> Exp B7 -> Exp B19
pattern B19 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B19 where
  fromBound b = B19_ (fromBound b) (fromBound b) (fromBound b)

instance Position P19 B19 where
  toBoundaries (P19 x1 x2 x3) = B19 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B19 b1 b2 b3) = P19 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B19 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen :: Exp (P6, SFC64)
        T2 x2 gen2 = randomPosition b2 gen1 :: Exp (P6, SFC64)
        T2 x3 gen3 = randomPosition b3 gen2 :: Exp (P7, SFC64)
     in A.lift (P19 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P19_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P19 x1 x2 x3) = P19 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P19 x1 x2 x3) (P19 y1 y2 y3) = P19 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P19 x1 x2 x3) = i A.< 6 ? (projection i x1, i A.< 12 ? (projection (i - 6) x2, projection (i - 12) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B19 b b' b'') (P19 x1 x2 x3) = P19 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  psum (P19 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P19 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P19 x1 x2 x3) (P19 y1 y2 y3) = P19 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B19 b b' b'') = RandomT . StateT $ \s0 ->
    let B6 b1 b2 b3 b4 b5 b6 = b
        B6 b7 b8 b9 b10 b11 b12 = b'
        B7 b13 b14 b15 b16 b17 b18 b19 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        p6s = A.zipWith6 P6 x1s x2s x3s x4s x5s x6s
        p6s' = A.zipWith6 P6 x7s x8s x9s x10s x11s x12s
        p7s = A.zipWith7 P7 x13s x14s x15s x16s x17s x18s x19s
     in return (A.zipWith3 P19 p6s p6s' p7s, s19)
  randomPositions _ = error "Error: randomPositions"

  difference (P19 x1 x2 x3) (P19 y1 y2 y3) = P19 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R20
data P20 where
  P20_ :: P6 -> P7 -> P7 -> P20
  deriving (Generic)

instance Show P20 where
  show p = "P20 (" P.++ showContent p P.++ ")"

instance Elt P20

pattern P20 :: Exp P6 -> Exp P7 -> Exp P7 -> Exp P20
pattern P20 x1 x2 x3 = Pattern (x1, x2, x3)

data B20 where
  B20_ :: B6 -> B7 -> B7 -> B20
  deriving (Generic, Show)

instance Elt B20

pattern B20 :: Exp B6 -> Exp B7 -> Exp B7 -> Exp B20
pattern B20 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B20 where
  fromBound b = B20_ (fromBound b) (fromBound b) (fromBound b)

instance Position P20 B20 where
  toBoundaries (P20 x1 x2 x3) = B20 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B20 b1 b2 b3) = P20 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B20 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P20 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P20_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P20 x1 x2 x3) = P20 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P20 x1 x2 x3) (P20 y1 y2 y3) = P20 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P20 x1 x2 x3) = i A.< 6 ? (projection i x1, i A.< 13 ? (projection (i - 6) x2, projection (i - 13) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B20 b1 b2 b3) (P20 x1 x2 x3) = P20 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P20 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P20 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P20 x1 x2 x3) (P20 y1 y2 y3) = P20 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B20 b b' b'') = RandomT . StateT $ \s0 ->
    let B6 b1 b2 b3 b4 b5 b6 = b
        B7 b7 b8 b9 b10 b11 b12 b13 = b'
        B7 b14 b15 b16 b17 b18 b19 b20 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        pos6 = A.zipWith6 P6 x1s x2s x3s x4s x5s x6s
        pos7 = A.zipWith7 P7 x7s x8s x9s x10s x11s x12s x13s
        pos7' = A.zipWith7 P7 x14s x15s x16s x17s x18s x19s x20s
     in return (A.zipWith3 P20 pos6 pos7 pos7', s20)
  randomPositions _ = error "Error: randomPositions"

  difference (P20 x1 x2 x3) (P20 y1 y2 y3) = P20 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R21

data P21 where
  P21_ :: P7 -> P7 -> P7 -> P21
  deriving (Generic)

instance Show P21 where
  show p = "P21 (" P.++ showContent p P.++ ")"

instance Elt P21

pattern P21 :: Exp P7 -> Exp P7 -> Exp P7 -> Exp P21
pattern P21 x1 x2 x3 = Pattern (x1, x2, x3)

data B21 where
  B21_ :: B7 -> B7 -> B7 -> B21
  deriving (Generic, Show)

instance Elt B21

pattern B21 :: Exp B7 -> Exp B7 -> Exp B7 -> Exp B21
pattern B21 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B21 where
  fromBound b = B21_ (fromBound b) (fromBound b) (fromBound b)

instance Position P21 B21 where
  toBoundaries (P21 x1 x2 x3) = B21 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B21 b1 b2 b3) = P21 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B21 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P21 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P21_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P21 x1 x2 x3) = P21 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P21 x1 x2 x3) (P21 y1 y2 y3) = P21 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P21 x1 x2 x3) = i A.< 7 ? (projection i x1, i A.< 14 ? (projection (i - 7) x2, projection (i - 14) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B21 b1 b2 b3) (P21 x1 x2 x3) = P21 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P21 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P21 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P21 x1 x2 x3) (P21 y1 y2 y3) = P21 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B21 b b' b'') = RandomT . StateT $ \s0 -> do
    let B7 b1 b2 b3 b4 b5 b6 b7 = b
        B7 b8 b9 b10 b11 b12 b13 b14 = b'
        B7 b15 b16 b17 b18 b19 b20 b21 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        p7s = zipWith7 P7 x1s x2s x3s x4s x5s x6s x7s
        p7s' = zipWith7 P7 x8s x9s x10s x11s x12s x13s x14s
        p7s'' = zipWith7 P7 x15s x16s x17s x18s x19s x20s x21s
     in return (A.zipWith3 P21 p7s p7s' p7s'', s21)
  randomPositions _ = error "Error: randomPositions"

  difference (P21 x1 x2 x3) (P21 y1 y2 y3) = P21 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R22

data P22 where
  P22_ :: P7 -> P7 -> P8 -> P22
  deriving (Generic)

instance Show P22 where
  show p = "P22 (" P.++ showContent p P.++ ")"

instance Elt P22

pattern P22 :: Exp P7 -> Exp P7 -> Exp P8 -> Exp P22
pattern P22 x1 x2 x3 = Pattern (x1, x2, x3)

data B22 where
  B22_ :: B7 -> B7 -> B8 -> B22
  deriving (Generic, Show)

instance Elt B22

pattern B22 :: Exp B7 -> Exp B7 -> Exp B8 -> Exp B22
pattern B22 b1 b2 b3 = Pattern (b1, b2, b3)

instance Boundaries B22 where
  fromBound b = B22_ (fromBound b) (fromBound b) (fromBound b)

instance Position P22 B22 where
  toBoundaries (P22 x1 x2 x3) = B22 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B22 b1 b2 b3) = P22 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B22 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P22 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P22_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P22 x1 x2 x3) = P22 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P22 x1 x2 x3) (P22 y1 y2 y3) = P22 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P22 x1 x2 x3) = i A.< 7 ? (projection i x1, i A.< 14 ? (projection (i - 7) x2, projection (i - 14) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B22 b1 b2 b3) (P22 x1 x2 x3) = P22 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P22 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P22 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P22 x1 x2 x3) (P22 y1 y2 y3) = P22 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B22 b b' b'') = RandomT . StateT $ \s0 ->
    let B7 b1 b2 b3 b4 b5 b6 b7 = b
        B7 b8 b9 b10 b11 b12 b13 b14 = b'
        B8 b15 b16 b17 b18 b19 b20 b21 b22 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        p7s = A.zipWith7 P7 x1s x2s x3s x4s x5s x6s x7s
        p7s' = A.zipWith7 P7 x8s x9s x10s x11s x12s x13s x14s
        p8s = A.zipWith8 P8 x15s x16s x17s x18s x19s x20s x21s x22s
     in return (A.zipWith3 P22 p7s p7s' p8s, s22)
  randomPositions _ = error "Error: randomPositions"

  difference (P22 x1 x2 x3) (P22 y1 y2 y3) = P22 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R23

data P23 where
  P23_ :: P7 -> P8 -> P8 -> P23
  deriving (Generic)

instance Show P23 where
  show p = "P23 (" P.++ showContent p P.++ ")"

instance Elt P23

pattern P23 :: Exp P7 -> Exp P8 -> Exp P8 -> Exp P23
pattern P23 x1 x2 x3 = Pattern (x1, x2, x3)

data B23 where
  B23_ :: B7 -> B8 -> B8 -> B23
  deriving (Generic, Show)

instance Elt B23

pattern B23 :: Exp B7 -> Exp B8 -> Exp B8 -> Exp B23
pattern B23 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B23 where
  fromBound b = B23_ (fromBound b) (fromBound b) (fromBound b)

instance Position P23 B23 where
  toBoundaries (P23 x1 x2 x3) = B23 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B23 b1 b2 b3) = P23 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B23 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P23 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P23_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P23 x1 x2 x3) = P23 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P23 x1 x2 x3) (P23 y1 y2 y3) = P23 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  projection i (P23 x1 x2 x3) = i A.< 7 ? (projection i x1, i A.< 15 ? (projection (i - 7) x2, projection (i - 15) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B23 b b' b'') (P23 x1 x2 x3) = P23 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P23 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P23 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P23 x1 x2 x3) (P23 y1 y2 y3) = P23 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  randomPositions (B23 b b' b'') = RandomT . StateT $ \s0 ->
    let B7 b1 b2 b3 b4 b5 b6 b7 = b
        B8 b8 b9 b10 b11 b12 b13 b14 b15 = b'
        B8 b16 b17 b18 b19 b20 b21 b22 b23 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        p7s = A.zipWith7 P7 x1s x2s x3s x4s x5s x6s x7s
        p8s = A.zipWith8 P8 x8s x9s x10s x11s x12s x13s x14s x15s
        p8's = A.zipWith8 P8 x16s x17s x18s x19s x20s x21s x22s x23s
     in return (A.zipWith3 P23 p7s p8s p8's, s23)
  randomPositions _ = error "Error: randomPositions"

  difference (P23 x1 x2 x3) (P23 y1 y2 y3) = P23 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

-- R24

data P24 where
  P24_ :: P8 -> P8 -> P8 -> P24
  deriving (Generic)

instance Show P24 where
  show p = "P24 (" P.++ showContent p P.++ ")"

instance Elt P24

pattern P24 :: Exp P8 -> Exp P8 -> Exp P8 -> Exp P24
pattern P24 x1 x2 x3 = Pattern (x1, x2, x3)

data B24 where
  B24_ :: B8 -> B8 -> B8 -> B24
  deriving (Generic, Show)

instance Elt B24

pattern B24 :: Exp B8 -> Exp B8 -> Exp B8 -> Exp B24
pattern B24 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B24 where
  fromBound b = B24_ (fromBound b) (fromBound b) (fromBound b)

instance Position P24 B24 where
  toBoundaries (P24 x1 x2 x3) = B24 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B24 b1 b2 b3) = P24 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B24 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P24 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P24_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P24 x1 x2 x3) = P24 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  projection i (P24 x1 x2 x3) = i A.< 8 ? (projection i x1, i A.< 16 ? (projection (i - 8) x2, projection (i - 16) x3))
  projection _ _ = error "Error: projection"

  fixBounds (B24 b b' b'') (P24 x1 x2 x3) = P24 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBound"

  psum (P24 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P24 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P24 x1 x2 x3) (P24 y1 y2 y3) = P24 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  add (P24 x1 x2 x3) (P24 y1 y2 y3) = P24 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  difference (P24 x1 x2 x3) (P24 y1 y2 y3) = P24 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  randomPositions (B24 b b' b'') = RandomT . StateT $ \s0 ->
    let B8 b1 b2 b3 b4 b5 b6 b7 b8 = b
        B8 b9 b10 b11 b12 b13 b14 b15 b16 = b'
        B8 b17 b18 b19 b20 b21 b22 b23 b24 = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        p8s = A.zipWith8 P8 x1s x2s x3s x4s x5s x6s x7s x8s
        p8s' = A.zipWith8 P8 x9s x10s x11s x12s x13s x14s x15s x16s
        p8s'' = A.zipWith8 P8 x17s x18s x19s x20s x21s x22s x23s x24s
     in return (A.zipWith3 P24 p8s p8s' p8s'', s23)
  randomPositions _ = error "Error: randomPositions"

-- R25

data P25 where
  P25_ :: P8 -> P8 -> P9 -> P25
  deriving (Generic)

instance Show P25 where
  show p = "P25 (" P.++ showContent p P.++ ")"

instance Elt P25

pattern P25 :: Exp P8 -> Exp P8 -> Exp P9 -> Exp P25
pattern P25 x1 x2 x3 = Pattern (x1, x2, x3)

data B25 where
  B25_ :: B8 -> B8 -> B9 -> B25
  deriving (Generic, Show)

instance Elt B25

pattern B25 :: Exp B8 -> Exp B8 -> Exp B9 -> Exp B25
pattern B25 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B25 where
  fromBound b = B25_ (fromBound b) (fromBound b) (fromBound b)

instance Position P25 B25 where
  toBoundaries (P25 x1 x2 x3) = B25 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B25 b1 b2 b3) = P25 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B25 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P25 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P25_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P25 x1 x2 x3) = P25 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  psum (P25 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  pprod (P25 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  prod (P25 x1 x2 x3) (P25 y1 y2 y3) = P25 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

  add (P25 x1 x2 x3) (P25 y1 y2 y3) = P25 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  fixBounds (B25 b1 b2 b3) (P25 x1 x2 x3) = P25 (fixBounds b1 x1) (fixBounds b2 x2) (fixBounds b3 x3)
  fixBounds _ _ = error "Error: fixBounds"

  difference (P25 x1 x2 x3) (P25 y1 y2 y3) = P25 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  projection i (P25 x1 x2 x3) = i A.< 8 ? (projection i x1, i A.< 16 ? (projection (i - 8) x2, projection (i - 16) x3))
  projection _ _ = error "Error: projection"

  randomPositions (B25 b b' b'') = RandomT . StateT $ \s0 ->
    let B8 b1 b2 b3 b4 b5 b6 b7 b8 = b
        B8 b9 b10 b11 b12 b13 b14 b15 b16 = b'
        B9 (B3 b17 b18 b19) (B3 b20 b21 b22) (B3 b23 b24 b25) = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        (x25s, s25) = A.unzip $ A.map (uniformRange b25) s24
        p8s = A.zipWith8 P8 x1s x2s x3s x4s x5s x6s x7s x8s
        p8s' = A.zipWith8 P8 x9s x10s x11s x12s x13s x14s x15s x16s
        p9s = A.zipWith3 P9 (A.zipWith3 P3 x17s x18s x19s) (A.zipWith3 P3 x20s x21s x22s) (A.zipWith3 P3 x23s x24s x25s)
     in return (A.zipWith3 P25 p8s p8s' p9s, s25)
  randomPositions _ = error "Error: randomPositions"

-- R26
data P26 where
  P26_ :: P8 -> P9 -> P9 -> P26
  deriving (Generic)

instance Show P26 where
  show p = "P26 (" P.++ showContent p P.++ ")"

instance Elt P26

pattern P26 :: Exp P8 -> Exp P9 -> Exp P9 -> Exp P26
pattern P26 x1 x2 x3 = Pattern (x1, x2, x3)

data B26 where
  B26_ :: B8 -> B9 -> B9 -> B26
  deriving (Generic, Show)

instance Elt B26

pattern B26 :: Exp B8 -> Exp B9 -> Exp B9 -> Exp B26
pattern B26 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B26 where
  fromBound b = B26_ (fromBound b) (fromBound b) (fromBound b)

instance Position P26 B26 where
  toBoundaries (P26 x1 x2 x3) = B26 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B26 b1 b2 b3) = P26 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B26 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P26 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P26_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P26 x1 x2 x3) = P26 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P26 x1 x2 x3) (P26 y1 y2 y3) = P26 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  fixBounds (B26 b b' b'') (P26 x1 x2 x3) = P26 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  difference (P26 x1 x2 x3) (P26 y1 y2 y3) = P26 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  psum (P26 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  projection i (P26 x1 x2 x3) = i A.< 8 ? (projection i x1, i A.< 17 ? (projection (i - 8) x2, projection (i - 17) x3))
  projection _ _ = error "Error: projection"

  pprod (P26 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  randomPositions (B26 b b' b'') = RandomT . StateT $ \s0 ->
    let B8 b1 b2 b3 b4 b5 b6 b7 b8 = b
        B9 (B3 b9 b10 b11) (B3 b12 b13 b14) (B3 b15 b16 b17) = b'
        B9 (B3 b18 b19 b20) (B3 b21 b22 b23) (B3 b24 b25 b26) = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        (x25s, s25) = A.unzip $ A.map (uniformRange b25) s24
        (x26s, s26) = A.unzip $ A.map (uniformRange b26) s25
        p8s = A.zipWith8 P8 x1s x2s x3s x4s x5s x6s x7s x8s
        p9s = A.zipWith3 P9 (A.zipWith3 P3 x9s x10s x11s) (A.zipWith3 P3 x12s x13s x14s) (A.zipWith3 P3 x15s x16s x17s)
        p9s' = A.zipWith3 P9 (A.zipWith3 P3 x18s x19s x20s) (A.zipWith3 P3 x21s x22s x23s) (A.zipWith3 P3 x24s x25s x26s)
     in return (A.zipWith3 P26 p8s p9s p9s', s26)
  randomPositions _ = error "Error: randomPositions"

  prod (P26 x1 x2 x3) (P26 y1 y2 y3) = P26 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R27
data P27 where
  P27_ :: P9 -> P9 -> P9 -> P27
  deriving (Generic)

instance Show P27 where
  show p = "P27 (" P.++ showContent p P.++ ")"

instance Elt P27

pattern P27 :: Exp P9 -> Exp P9 -> Exp P9 -> Exp P27
pattern P27 x1 x2 x3 = Pattern (x1, x2, x3)

data B27 where
  B27_ :: B9 -> B9 -> B9 -> B27
  deriving (Generic, Show)

instance Elt B27

pattern B27 :: Exp B9 -> Exp B9 -> Exp B9 -> Exp B27
pattern B27 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B27 where
  fromBound b = B27_ (fromBound b) (fromBound b) (fromBound b)

instance Position P27 B27 where
  toBoundaries (P27 x1 x2 x3) = B27 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B27 b1 b2 b3) = P27 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B27 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P27 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P27_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P27 x1 x2 x3) = P27 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P27 x1 x2 x3) (P27 y1 y2 y3) = P27 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  fixBounds (B27 b b' b'') (P27 x1 x2 x3) = P27 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  difference (P27 x1 x2 x3) (P27 y1 y2 y3) = P27 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  psum (P27 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  projection i (P27 x1 x2 x3) = i A.< 9 ? (projection i x1, i A.< 18 ? (projection (i - 9) x2, projection (i - 18) x3))
  projection _ _ = error "Error: projection"

  pprod (P27 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  randomPositions (B27 b b' b'') = RandomT . StateT $ \s0 ->
    let B9 (B3 b1 b2 b3) (B3 b4 b5 b6) (B3 b7 b8 b9) = b
        B9 (B3 b10 b11 b12) (B3 b13 b14 b15) (B3 b16 b17 b18) = b'
        B9 (B3 b19 b20 b21) (B3 b22 b23 b24) (B3 b25 b26 b27) = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        (x25s, s25) = A.unzip $ A.map (uniformRange b25) s24
        (x26s, s26) = A.unzip $ A.map (uniformRange b26) s25
        (x27s, s27) = A.unzip $ A.map (uniformRange b27) s26
        p9s = A.zipWith3 P9 (A.zipWith3 P3 x1s x2s x3s) (A.zipWith3 P3 x4s x5s x6s) (A.zipWith3 P3 x7s x8s x9s)
        p9s' = A.zipWith3 P9 (A.zipWith3 P3 x10s x11s x12s) (A.zipWith3 P3 x13s x14s x15s) (A.zipWith3 P3 x16s x17s x18s)
        p9s'' = A.zipWith3 P9 (A.zipWith3 P3 x19s x20s x21s) (A.zipWith3 P3 x22s x23s x24s) (A.zipWith3 P3 x25s x26s x27s)
     in return (A.zipWith3 P27 p9s p9s' p9s'', s27)
  randomPositions _ = error "Error: randomPositions"

  prod (P27 x1 x2 x3) (P27 y1 y2 y3) = P27 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R28
data P28 where
  P28_ :: P9 -> P9 -> P10 -> P28
  deriving (Generic)

instance Show P28 where
  show p = "P28 (" P.++ showContent p P.++ ")"

instance Elt P28

pattern P28 :: Exp P9 -> Exp P9 -> Exp P10 -> Exp P28
pattern P28 x1 x2 x3 = Pattern (x1, x2, x3)

data B28 where
  B28_ :: B9 -> B9 -> B10 -> B28
  deriving (Generic, Show)

instance Elt B28

pattern B28 :: Exp B9 -> Exp B9 -> Exp B10 -> Exp B28
pattern B28 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B28 where
  fromBound b = B28_ (fromBound b) (fromBound b) (fromBound b)

instance Position P28 B28 where
  toBoundaries (P28 x1 x2 x3) = B28 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B28 b1 b2 b3) = P28 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B28 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P28 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P28_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P28 x1 x2 x3) = P28 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P28 x1 x2 x3) (P28 y1 y2 y3) = P28 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  fixBounds (B28 b b' b'') (P28 x1 x2 x3) = P28 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  difference (P28 x1 x2 x3) (P28 y1 y2 y3) = P28 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  psum (P28 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  projection i (P28 x1 x2 x3) = i A.< 9 ? (projection i x1, i A.< 18 ? (projection (i - 9) x2, projection (i - 18) x3))
  projection _ _ = error "Error: projection"

  pprod (P28 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  randomPositions (B28 b b' b'') = RandomT . StateT $ \s0 ->
    let B9 (B3 b1 b2 b3) (B3 b4 b5 b6) (B3 b7 b8 b9) = b
        B9 (B3 b10 b11 b12) (B3 b13 b14 b15) (B3 b16 b17 b18) = b'
        B10 (B3 b19 b20 b21) (B3 b22 b23 b24) (B4 b25 b26 b27 b28) = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        (x25s, s25) = A.unzip $ A.map (uniformRange b25) s24
        (x26s, s26) = A.unzip $ A.map (uniformRange b26) s25
        (x27s, s27) = A.unzip $ A.map (uniformRange b27) s26
        (x28s, s28) = A.unzip $ A.map (uniformRange b28) s27
        p9s = A.zipWith3 P9 (A.zipWith3 P3 x1s x2s x3s) (A.zipWith3 P3 x4s x5s x6s) (A.zipWith3 P3 x7s x8s x9s)
        p9s' = A.zipWith3 P9 (A.zipWith3 P3 x10s x11s x12s) (A.zipWith3 P3 x13s x14s x15s) (A.zipWith3 P3 x16s x17s x18s)
        p9s'' = A.zipWith3 P10 (A.zipWith3 P3 x19s x20s x21s) (A.zipWith3 P3 x22s x23s x24s) (A.zipWith4 P4 x25s x26s x27s x28s)
     in return (A.zipWith3 P28 p9s p9s' p9s'', s28)
  randomPositions _ = error "Error: randomPositions"

  prod (P28 x1 x2 x3) (P28 y1 y2 y3) = P28 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"

-- R29
data P29 where
  P29_ :: P9 -> P10 -> P10 -> P29
  deriving (Generic)

instance Show P29 where
  show p = "P29 (" P.++ showContent p P.++ ")"

instance Elt P29

pattern P29 :: Exp P9 -> Exp P10 -> Exp P10 -> Exp P29
pattern P29 x1 x2 x3 = Pattern (x1, x2, x3)

data B29 where
  B29_ :: B9 -> B10 -> B10 -> B29
  deriving (Generic, Show)

instance Elt B29

pattern B29 :: Exp B9 -> Exp B10 -> Exp B10 -> Exp B29
pattern B29 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B29 where
  fromBound b = B29_ (fromBound b) (fromBound b) (fromBound b)

instance Position P29 B29 where
  toBoundaries (P29 x1 x2 x3) = B29 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B29 b1 b2 b3) = P29 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"


  randomPosition (B29 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P29 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P29_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P29 x1 x2 x3) = P29 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P29 x1 x2 x3) (P29 y1 y2 y3) = P29 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  fixBounds (B29 b b' b'') (P29 x1 x2 x3) = P29 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  difference (P29 x1 x2 x3) (P29 y1 y2 y3) = P29 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  psum (P29 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  projection i (P29 x1 x2 x3) = i A.< 9 ? (projection i x1, i A.< 19 ? (projection (i - 9) x2, projection (i - 19) x3))
  projection _ _ = error "Error: projection"

  pprod (P29 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  randomPositions (B29 b b' b'') = RandomT . StateT $ \s0 ->
    let B9 (B3 b1 b2 b3) (B3 b4 b5 b6) (B3 b7 b8 b9) = b
        B10 (B3 b10 b11 b12) (B3 b13 b14 b15) (B4 b16 b17 b18 b19) = b'
        B10 (B3 b20 b21 b22) (B3 b23 b24 b25) (B4 b26 b27 b28 b29) = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        (x25s, s25) = A.unzip $ A.map (uniformRange b25) s24
        (x26s, s26) = A.unzip $ A.map (uniformRange b26) s25
        (x27s, s27) = A.unzip $ A.map (uniformRange b27) s26
        (x28s, s28) = A.unzip $ A.map (uniformRange b28) s27
        (x29s, s29) = A.unzip $ A.map (uniformRange b29) s28
        p9s = A.zipWith3 P9 (A.zipWith3 P3 x1s x2s x3s) (A.zipWith3 P3 x4s x5s x6s) (A.zipWith3 P3 x7s x8s x9s)
        p9s' = A.zipWith3 P10 (A.zipWith3 P3 x10s x11s x12s) (A.zipWith3 P3 x13s x14s x15s) (A.zipWith4 P4 x16s x17s x18s x19s)
        p9s'' = A.zipWith3 P10 (A.zipWith3 P3 x20s x21s x22s) (A.zipWith3 P3 x23s x24s x25s) (A.zipWith4 P4 x26s x27s x28s x29s)
     in return (A.zipWith3 P29 p9s p9s' p9s'', s29)
  randomPositions _ = error "Error: randomPositions"

  prod (P29 x1 x2 x3) (P29 y1 y2 y3) = P29 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"


-- R30
data P30 where
  P30_ :: P10 -> P10 -> P10 -> P30
  deriving (Generic)

instance Show P30 where
  show p = "P30 (" P.++ showContent p P.++ ")"

instance Elt P30

pattern P30 :: Exp P10 -> Exp P10 -> Exp P10 -> Exp P30
pattern P30 x1 x2 x3 = Pattern (x1, x2, x3)

data B30 where
  B30_ :: B10 -> B10 -> B10 -> B30
  deriving (Generic, Show)

instance Elt B30

pattern B30 :: Exp B10 -> Exp B10 -> Exp B10 -> Exp B30
pattern B30 x1 x2 x3 = Pattern (x1, x2, x3)

instance Boundaries B30 where
  fromBound b = B30_ (fromBound b) (fromBound b) (fromBound b)

instance Position P30 B30 where
  toBoundaries (P30 x1 x2 x3) = B30 (toBoundaries x1) (toBoundaries x2) (toBoundaries x3)
  toBoundaries _ = error "Error: toBoundaries"

  boundariesDiameters (B30 b1 b2 b3) = P30 (boundariesDiameters b1) (boundariesDiameters b2) (boundariesDiameters b3)
  boundariesDiameters _ = error "Error: boundariesDiameters"

  randomPosition (B30 b1 b2 b3) gen =
    let T2 x1 gen1 = randomPosition b1 gen
        T2 x2 gen2 = randomPosition b2 gen1
        T2 x3 gen3 = randomPosition b3 gen2
     in A.lift (P30 x1 x2 x3, gen3)
  randomPosition _ _ = error "Error: randomPosition"

  showContent (P30_ x1 x2 x3) = showContent x1 P.++ ", " P.++ showContent x2 P.++ ", " P.++ showContent x3
  pmap f (P30 x1 x2 x3) = P30 (pmap f x1) (pmap f x2) (pmap f x3)
  pmap _ _ = error "Error: pmap"

  add (P30 x1 x2 x3) (P30 y1 y2 y3) = P30 (add x1 y1) (add x2 y2) (add x3 y3)
  add _ _ = error "Error: add"

  fixBounds (B30 b b' b'') (P30 x1 x2 x3) = P30 (fixBounds b x1) (fixBounds b' x2) (fixBounds b'' x3)
  fixBounds _ _ = error "Error: fixBounds"

  difference (P30 x1 x2 x3) (P30 y1 y2 y3) = P30 (difference x1 y1) (difference x2 y2) (difference x3 y3)
  difference _ _ = error "Error: difference"

  psum (P30 x1 x2 x3) = psum x1 + psum x2 + psum x3
  psum _ = error "Error: psum"

  projection i (P30 x1 x2 x3) = i A.< 10 ? (projection i x1, i A.< 20 ? (projection (i - 10) x2, projection (i - 20) x3))
  projection _ _ = error "Error: projection"

  pprod (P30 x1 x2 x3) = pprod x1 * pprod x2 * pprod x3
  pprod _ = error "Error: pprod"

  randomPositions (B30 b b' b'') = RandomT . StateT $ \s0 ->
    let B10 (B3 b1 b2 b3) (B3 b4 b5 b6) (B4 b7 b8 b9 b10) = b
        B10 (B3 b11 b12 b13) (B3 b14 b15 b16) (B4 b17 b18 b19 b20) = b'
        B10 (B3 b21 b22 b23) (B3 b24 b25 b26) (B4 b27 b28 b29 b30) = b''
        (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
        (x11s, s11) = A.unzip $ A.map (uniformRange b11) s10
        (x12s, s12) = A.unzip $ A.map (uniformRange b12) s11
        (x13s, s13) = A.unzip $ A.map (uniformRange b13) s12
        (x14s, s14) = A.unzip $ A.map (uniformRange b14) s13
        (x15s, s15) = A.unzip $ A.map (uniformRange b15) s14
        (x16s, s16) = A.unzip $ A.map (uniformRange b16) s15
        (x17s, s17) = A.unzip $ A.map (uniformRange b17) s16
        (x18s, s18) = A.unzip $ A.map (uniformRange b18) s17
        (x19s, s19) = A.unzip $ A.map (uniformRange b19) s18
        (x20s, s20) = A.unzip $ A.map (uniformRange b20) s19
        (x21s, s21) = A.unzip $ A.map (uniformRange b21) s20
        (x22s, s22) = A.unzip $ A.map (uniformRange b22) s21
        (x23s, s23) = A.unzip $ A.map (uniformRange b23) s22
        (x24s, s24) = A.unzip $ A.map (uniformRange b24) s23
        (x25s, s25) = A.unzip $ A.map (uniformRange b25) s24
        (x26s, s26) = A.unzip $ A.map (uniformRange b26) s25
        (x27s, s27) = A.unzip $ A.map (uniformRange b27) s26
        (x28s, s28) = A.unzip $ A.map (uniformRange b28) s27
        (x29s, s29) = A.unzip $ A.map (uniformRange b29) s27
        (x30s, s30) = A.unzip $ A.map (uniformRange b30) s28
        p9s = A.zipWith3 P10 (A.zipWith3 P3 x1s x2s x3s) (A.zipWith3 P3 x4s x5s x6s) (A.zipWith4 P4 x7s x8s x9s x10s)
        p9s' = A.zipWith3 P10 (A.zipWith3 P3 x11s x12s x13s) (A.zipWith3 P3 x14s x15s x16s) (A.zipWith4 P4 x17s x18s x19s x20s)
        p9s'' = A.zipWith3 P10 (A.zipWith3 P3 x21s x22s x23s) (A.zipWith3 P3 x24s x25s x26s) (A.zipWith4 P4 x27s x28s x29s x30s)
     in return (A.zipWith3 P30 p9s p9s' p9s'', s30)
  randomPositions _ = error "Error: randomPositions"

  prod (P30 x1 x2 x3) (P30 y1 y2 y3) = P30 (prod x1 y1) (prod x2 y2) (prod x3 y3)
  prod _ _ = error "Error: prod"
