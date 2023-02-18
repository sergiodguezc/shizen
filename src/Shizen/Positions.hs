{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shizen.Positions
  ( module Shizen.Positions,
  )
where

import Control.Monad.State
import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.SFC as SFC
import Shizen.Types
import Shizen.Utils

{- Instances -}

-- R1
data P1 where
  P1_ :: R -> P1
  deriving (Generic, Show)

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
  fromValue x = B1_ (- x, x)
  fromBound b = B1_ b

instance Position P1 B1 where
  pmap f (P1 x) = P1 (f x)
  pmap _ _ = error "Error pmap"

  psum (P1 x) = x
  psum _ = error "Error psum"

  pprod (P1 x) = x
  pprod _ = error "Error pmap"

  projection _ (P1 x) = x
  projection _ _ = error "Error: projection"

  fixBounds (B1 b) (P1 x) =
    let T2 minb1 maxb1 = b
        --
        x' = x A.< minb1 ? (minb1, x A.> maxb1 ? (maxb1, x))
     in P1 x'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPosition (B1 b) = RandomT . StateT $ \s ->
    let (xs, s1) = A.unzip $ A.map (uniformRange b) s
     in return (A.map P1 xs, s1)
  randomPosition _ = error "Error: randomPosition"

  prod (P1 x) (P1 y) = P1 (x * y)
  prod _ _ = error "Error: prod"

  difference (P1 x) (P1 y) = P1 (x - y)
  difference _ _ = error "Error: difference"

  add (P1 x) (P1 y) = P1 (x + y)
  add _ _ = error "Error: sum"

-- R2

data P2 where
  P2_ :: R -> R -> P2
  deriving (Generic, Show)

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
  fromValue x = B2_ (- x, x) (- x, x)
  fromBound b = B2_ b b

instance Position P2 B2 where
  pmap f (P2 x y) = P2 (f x) (f y)
  pmap _ _ = error "Error pmap"

  psum (P2 x y) = x + y
  psum _ = error "Error psum"

  pprod (P2 x y) = x * y
  pprod _ = error "Error pmap"

  projection i (P2 x y) = (i A.== 0) ? (x, y)
  projection _ _ = error "Error: projection"

  fixBounds (B2 b1 b2) (P2 x y) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        --
        x' = x A.< minb1 ? (minb1, x A.> maxb1 ? (maxb1, x))
        y' = y A.< minb2 ? (minb2, y A.> maxb2 ? (maxb2, y))
     in P2 x' y'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPosition (B2 b1 b2) = RandomT . StateT $ \s ->
    let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
        (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
     in return (A.zipWith P2 xs ys, s2)
  randomPosition _ = error "Error: randomPosition"

  prod (P2 x y) (P2 x' y') = P2 (x * x') (y * y')
  prod _ _ = error "Error: prod"

  difference (P2 x y) (P2 x' y') = P2 (x - x') (y - y')
  difference _ _ = error "Error: difference"

  add (P2 x y) (P2 x' y') = P2 (x + x') (y + y')
  add _ _ = error "Error: sum"

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

instance Boundaries B3 where
  fromValue x = B3_ (- x, x) (- x, x) (- x, x)
  fromBound b = B3_ b b b

instance Position P3 B3 where
  pmap f (P3 x y z) = P3 (f x) (f y) (f z)
  pmap _ _ = error "Error pmap"

  psum (P3 x y z) = x + y + z
  psum _ = error "Error psum"

  pprod (P3 x y z) = x * y * z
  pprod _ = error "Error pmap"

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

  randomPosition (B3 b1 b2 b3) = RandomT . StateT $ \s ->
    let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
        (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
        (zs, s3) = A.unzip $ A.map (uniformRange b3) s2
     in return (A.zipWith3 P3 xs ys zs, s3)
  randomPosition _ = error "Error: randomPosition"

  prod (P3 x y z) (P3 x' y' z') = P3 (x * x') (y * y') (z * z')
  prod _ _ = error "Error: prod"

  difference (P3 x y z) (P3 x' y' z') = P3 (x - x') (y - y') (z - z')
  difference _ _ = error "Error: difference"

  add (P3 x y z) (P3 x' y' z') = P3 (x + x') (y + y') (z + z')
  add _ _ = error "Error: sum"

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

instance Boundaries B4 where
  fromValue x = B4_ (- x, x) (- x, x) (- x, x) (- x, x)
  fromBound b = B4_ b b b b

instance Position P4 B4 where
  pmap f (P4 x y z t) = P4 (f x) (f y) (f z) (f t)
  pmap _ _ = error "Error pmap"

  psum (P4 x y z t) = x + y + z + t
  psum _ = error "Error psum"

  pprod (P4 x y z t) = x * y * z * t
  pprod _ = error "Error pprod"

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

  randomPosition (B4 b1 b2 b3 b4) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
     in return (A.zipWith4 P4 x1s x2s x3s x4s, s4)
  randomPosition _ = error "Error: randomPosition"

  prod (P4 x1 x2 x3 x4) (P4 x1' x2' x3' x4') = P4 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4')
  prod _ _ = error "Error: prod"

  difference (P4 x1 x2 x3 x4) (P4 x1' x2' x3' x4') = P4 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4')
  difference _ _ = error "Error: difference"

  add (P4 x1 x2 x3 x4) (P4 x1' x2' x3' x4') = P4 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4')
  add _ _ = error "Error: sum"

-- R5

data P5 where
  P5_ :: R -> R -> R -> R -> R -> P5
  deriving (Generic, Show)

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
  fromValue x = B5_ (- x, x) (- x, x) (- x, x) (- x, x) (- x, x)
  fromBound b = B5_ b b b b b

instance Position P5 B5 where
  pmap f (P5 x1 x2 x3 x4 x5) = P5 (f x1) (f x2) (f x3) (f x4) (f x5)
  pmap _ _ = error "Error pmap"

  psum (P5 x1 x2 x3 x4 x5) = x1 + x2 + x3 + x4 + x5
  psum _ = error "Error psum"

  pprod (P5 x1 x2 x3 x4 x5) = x1 * x2 * x3 * x4 * x5
  pprod _ = error "Error pprod"

  projection i (P5 x1 x2 x3 x4 x5) = (i A.== 0) ? (x1, (i A.== 1) ? (x2, (i A.== 2) ? (x3, (i A.== 3) ? (x4, x5))))
  projection _ _ = error "Error: projection"

  fixBounds (B5 b1 b2 b3 b4 b5) (P5 x1 x2 x3 x4 x5) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        --
        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
     in P5 x1' x2' x3' x4' x5'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPosition (B5 b1 b2 b3 b4 b5) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
     in return (A.zipWith5 P5 x1s x2s x3s x4s x5s, s5)
  randomPosition _ = error "Error: randomPosition"

  prod (P5 x1 x2 x3 x4 x5) (P5 x1' x2' x3' x4' x5') = P5 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4') (x5 * x5')
  prod _ _ = error "Error: prod"

  difference (P5 x1 x2 x3 x4 x5) (P5 x1' x2' x3' x4' x5') = P5 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4') (x5 - x5')
  difference _ _ = error "Error: difference"

  add (P5 x1 x2 x3 x4 x5) (P5 x1' x2' x3' x4' x5') = P5 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4') (x5 + x5')
  add _ _ = error "Error: sum"

-- R6

data P6 where
  P6_ :: R -> R -> R -> R -> R -> R -> P6
  deriving (Generic, Show)

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
  fromValue x = B6_ (- x, x) (- x, x) (- x, x) (- x, x) (- x, x) (- x, x)
  fromBound b = B6_ b b b b b b

instance Position P6 B6 where
  pmap f (P6 x1 x2 x3 x4 x5 x6) = P6 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6)
  pmap _ _ = error "Error pmap"

  psum (P6 x1 x2 x3 x4 x5 x6) = x1 + x2 + x3 + x4 + x5 + x6
  psum _ = error "Error psum"

  pprod (P6 x1 x2 x3 x4 x5 x6) = x1 * x2 * x3 * x4 * x5 * x6
  pprod _ = error "Error pprod"

  projection i (P6 x1 x2 x3 x4 x5 x6) = (i A.== 0) ? (x1, (i A.== 1) ? (x2, (i A.== 2) ? (x3, (i A.== 3) ? (x4, (i A.== 4) ? (x5, x6)))))
  projection _ _ = error "Error: projection"

  fixBounds (B6 b1 b2 b3 b4 b5 b6) (P6 x1 x2 x3 x4 x5 x6) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        --
        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
        x6' = x6 A.< minb6 ? (minb6, x6 A.> maxb6 ? (maxb6, x6))
     in P6 x1' x2' x3' x4' x5' x6'
  fixBounds _ _ = error "Error: Cannot update position"

  randomPosition (B6 b1 b2 b3 b4 b5 b6) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
     in return (A.zipWith6 P6 x1s x2s x3s x4s x5s x6s, s6)
  randomPosition _ = error "Error: randomPosition"

  prod (P6 x1 x2 x3 x4 x5 x6) (P6 x1' x2' x3' x4' x5' x6') = P6 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4') (x5 * x5') (x6 * x6')
  prod _ _ = error "Error: prod"

  difference (P6 x1 x2 x3 x4 x5 x6) (P6 x1' x2' x3' x4' x5' x6') = P6 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4') (x5 - x5') (x6 - x6')
  difference _ _ = error "Error: difference"

  add (P6 x1 x2 x3 x4 x5 x6) (P6 x1' x2' x3' x4' x5' x6') = P6 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4') (x5 + x5') (x6 + x6')
  add _ _ = error "Error: sum"

-- R7

data P7 where
  P7_ :: R -> R -> R -> R -> R -> R -> R -> P7
  deriving (Generic, Show)

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
  fromValue x = B7_ (- x, x) (- x, x) (- x, x) (- x, x) (- x, x) (- x, x) (- x, x)
  fromBound b = B7_ b b b b b b b

instance Position P7 B7 where
  pmap f (P7 x1 x2 x3 x4 x5 x6 x7) = P7 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)
  pmap _ _ = error "Error pmap"

  psum (P7 x1 x2 x3 x4 x5 x6 x7) = x1 + x2 + x3 + x4 + x5 + x6 + x7
  psum _ = error "Error psum"

  pprod (P7 x1 x2 x3 x4 x5 x6 x7) = x1 * x2 * x3 * x4 * x5 * x6 * x7
  pprod _ = error "Error pprod"

  projection i (P7 x1 x2 x3 x4 x5 x6 x7) = (i A.== 0) ? (x1, (i A.== 1) ? (x2, (i A.== 2) ? (x3, (i A.== 3) ? (x4, (i A.== 4) ? (x5, (i A.== 5) ? (x6, x7))))))
  projection _ _ = error "Error: projection"

  fixBounds (B7 b1 b2 b3 b4 b5 b6 b7) (P7 x1 x2 x3 x4 x5 x6 x7) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
        --
        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
        x6' = x6 A.< minb6 ? (minb6, x6 A.> maxb6 ? (maxb6, x6))
        x7' = x7 A.< minb7 ? (minb7, x7 A.> maxb7 ? (maxb7, x7))
     in P7 x1' x2' x3' x4' x5' x6' x7'
  fixBounds _ _ = error "Error: fixBounds"

  randomPosition (B7 b1 b2 b3 b4 b5 b6 b7) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b6) s6
     in return (A.zipWith7 P7 x1s x2s x3s x4s x5s x6s x7s, s7)
  randomPosition _ = error "Error: randomPosition"

  prod (P7 x1 x2 x3 x4 x5 x6 x7) (P7 x1' x2' x3' x4' x5' x6' x7') = P7 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4') (x5 * x5') (x6 * x6') (x7 * x7')
  prod _ _ = error "Error: prod"

  difference (P7 x1 x2 x3 x4 x5 x6 x7) (P7 x1' x2' x3' x4' x5' x6' x7') = P7 (x1 - x1') (x2 - x2') (x3 - x3') (x4 - x4') (x5 - x5') (x6 - x6') (x7 - x7')
  difference _ _ = error "Error: difference"

  add (P7 x1 x2 x3 x4 x5 x6 x7) (P7 x1' x2' x3' x4' x5' x6' x7') = P7 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4') (x5 + x5') (x6 + x6') (x7 + x7')
  add _ _ = error "Error: sum"

-- R8

data P8 where
  P8_ :: R -> R -> R -> R -> R -> R -> R -> R -> P8
  deriving (Generic, Show)

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
  fromValue x = B8_ (- x, x) (- x, x) (- x, x) (- x, x) (- x, x) (- x, x) (- x, x) (- x, x)
  fromBound b = B8_ b b b b b b b b

instance Position P8 B8 where
  pmap f (P8 x1 x2 x3 x4 x5 x6 x7 x8) = P8 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8)
  pmap _ _ = error "Error: pmap"

  psum (P8 x1 x2 x3 x4 x5 x6 x7 x8) = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  psum _ = error "Error: psum"

  pprod (P8 x1 x2 x3 x4 x5 x6 x7 x8) = x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8
  pprod _ = error "Error: pprod"


  projection i (P8 x1 x2 x3 x4 x5 x6 x7 x8) = i A.== 0 ? (x1, i A.== 1 ? (x2, i A.== 2 ? (x3, i A.== 3 ? (x4, i A.== 4 ? (x5, i A.== 5 ? (x6, i A.== 6 ? (x7, x8)))))))
  projection _ _ = error "Error: projection"

  fixBounds (B8 b1 b2 b3 b4 b5 b6 b7 b8) (P8 x1 x2 x3 x4 x5 x6 x7 x8) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
        T2 minb8 maxb8 = b8
        --
        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
        x6' = x6 A.< minb6 ? (minb6, x6 A.> maxb6 ? (maxb6, x6))
        x7' = x7 A.< minb7 ? (minb7, x7 A.> maxb7 ? (maxb7, x7))
        x8' = x8 A.< minb8 ? (minb8, x8 A.> maxb8 ? (maxb8, x8))
     in P8 x1' x2' x3' x4' x5' x6' x7' x8'
  fixBounds _ _ = error "Error: fixBounds"

  randomPosition (B8 b1 b2 b3 b4 b5 b6 b7 b8) = RandomT . StateT $ \s0 ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
     in return (A.zipWith8 P8 x1s x2s x3s x4s x5s x6s x7s x8s, s8)
  randomPosition _ = error "Error: randomPosition"

  prod (P8 x1 x2 x3 x4 x5 x6 x7 x8) (P8 y1 y2 y3 y4 y5 y6 y7 y8) = P8 (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8)
  prod _ _ = error "Error: prod"

  difference (P8 x1 x2 x3 x4 x5 x6 x7 x8) (P8 y1 y2 y3 y4 y5 y6 y7 y8) = P8 (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8)
  difference _ _ = error "Error: difference"

  add (P8 x1 x2 x3 x4 x5 x6 x7 x8) (P8 y1 y2 y3 y4 y5 y6 y7 y8) = P8 (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8)
  add _ _ = error "Error: add"

-- R9

data P9 where
  P9_ :: R -> R -> R -> R -> R -> R -> R -> R -> R -> P9
  deriving (Generic, Show)

instance Elt P9

pattern P9 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P9
pattern P9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = Pattern (x1, x2, x3, x4, x5, x6, x7, x8, x9)


data B9 where
  B9_ :: Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> B9
  deriving (Generic, Show)

instance Elt B9

pattern B9 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B9
pattern B9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = Pattern (x1, x2, x3, x4, x5, x6, x7, x8, x9)

instance Boundaries B9 where
  fromValue x = B9_ (-x, x) (-x,x) (-x,x) (-x,x) (-x,x) (-x,x) (-x,x) (-x,x) (-x,x)
  fromBound b = B9_ b b b b b b b b b

instance Position P9 B9 where
  pmap f (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = P9 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9)
  pmap _ _ = error "Error: pmap"

  psum (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  psum _ = error "Error: psum"

  pprod (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9
  pprod _ = error "Error: pprod"

  projection i (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = i A.== 0 ? (x1, i A.== 1 ? (x2, i A.== 2 ? (x3, i A.== 3 ? (x4, i A.== 4 ? (x5, i A.== 5 ? (x6, i A.== 6 ? (x7, i A.== 7 ? (x8, x9))))))))
  projection _ _ = error "Error: projection"

  fixBounds (B9 b1 b2 b3 b4 b5 b6 b7 b8 b9) (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
        T2 minb8 maxb8 = b8
        T2 minb9 maxb9 = b9

        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
        x6' = x6 A.< minb6 ? (minb6, x6 A.> maxb6 ? (maxb6, x6))
        x7' = x7 A.< minb7 ? (minb7, x7 A.> maxb7 ? (maxb7, x7))
        x8' = x8 A.< minb8 ? (minb8, x8 A.> maxb8 ? (maxb8, x8))
        x9' = x9 A.< minb9 ? (minb9, x9 A.> maxb9 ? (maxb9, x9))

    in P9 x1' x2' x3' x4' x5' x6' x7' x8' x9'
  fixBounds _ _ = error "Error: fixBounds"

  randomPosition (B9 b1 b2 b3 b4 b5 b6 b7 b8 b9) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
    in return (A.zipWith9 P9 x1s x2s x3s x4s x5s x6s x7s x8s x9s, s9)
  randomPosition _ = error "Error: randomPosition"

  prod (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (P9 y1 y2 y3 y4 y5 y6 y7 y8 y9) = P9 (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9)
  prod _ _ = error "Error: prod"

  difference (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (P9 y1 y2 y3 y4 y5 y6 y7 y8 y9) = P9 (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9)
  difference _ _ = error "Error: difference"

  add (P9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (P9 y1 y2 y3 y4 y5 y6 y7 y8 y9) = P9 (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9)
  add _ _ = error "Error: add"


-- We repeat the same for the 10-dimensional case

data P10 where
  P10_ :: R -> R -> R -> R -> R -> R -> R -> R -> R -> R -> P10
  deriving (Generic, Show)

instance Elt P10

pattern P10 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P10
pattern P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = Pattern (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

data B10 where
  B10_ :: Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> B10
  deriving (Generic, Show)

instance Elt B10

pattern B10 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B10
pattern B10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 = Pattern (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)

instance Boundaries B10 where
  fromValue x = B10_ (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x)
  fromBound b = B10_ b b b b b b b b b b

instance Position P10 B10 where
  pmap f (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = P10 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10)
  pmap _ _ = error "Error: pmap"

  psum (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  psum _ = error "Error: psum"

  pprod (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10
  pprod _ = error "Error: pprod"

  projection i (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = i A.== 0 ? (x1, i A.== 1 ? (x2, i A.== 2 ? (x3, i A.== 3 ? (x4, i A.== 4 ? (x5, i A.== 5 ? (x6, i A.== 6 ? (x7, i A.== 7 ? (x8, i A.== 8 ? (x9, x10)))))))))
  projection _ _ = error "Error: projection"

  fixBounds (B10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) =
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
        T2 minb8 maxb8 = b8
        T2 minb9 maxb9 = b9
        T2 minb10 maxb10 = b10

        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
        x6' = x6 A.< minb6 ? (minb6, x6 A.> maxb6 ? (maxb6, x6))
        x7' = x7 A.< minb7 ? (minb7, x7 A.> maxb7 ? (maxb7, x7))
        x8' = x8 A.< minb8 ? (minb8, x8 A.> maxb8 ? (maxb8, x8))
        x9' = x9 A.< minb9 ? (minb9, x9 A.> maxb9 ? (maxb9, x9))
        x10' = x10 A.< minb10 ? (minb10, x10 A.> maxb10 ? (maxb10, x10))
    in P10 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10'
  fixBounds _ _ = error "Error: fixBounds"

  randomPosition (B10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) = RandomT . StateT $ \s ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
        (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
        (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
        (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
        (x6s, s6) = A.unzip $ A.map (uniformRange b6) s5
        (x7s, s7) = A.unzip $ A.map (uniformRange b7) s6
        (x8s, s8) = A.unzip $ A.map (uniformRange b8) s7
        (x9s, s9) = A.unzip $ A.map (uniformRange b9) s8
        (x10s, s10) = A.unzip $ A.map (uniformRange b10) s9
    in return (zipWith10 P10 x1s x2s x3s x4s x5s x6s x7s x8s x9s x10s, s10)
  randomPosition _ = error "Error: randomPosition"

  difference (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (P10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) = P10 (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10)
  difference _ _ = error "Error: difference"

  prod (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (P10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) = P10 (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10)
  prod _ _ = error "Error: prod"

  add (P10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (P10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) = P10 (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10)
  add _ _ = error "Error: sum"


-- R11

data P11 where
  P11_ :: R -> R -> R -> R -> R -> R -> R -> R -> R -> R -> R -> P11
  deriving (Generic, Show)

instance Elt P11

pattern P11 :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp P11
pattern P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = Pattern (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)

data B11 where
  B11_ :: Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> Bound -> B11
  deriving (Generic, Show)

instance Elt B11

pattern B11 :: Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp Bound -> Exp B11
pattern B11 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 = Pattern (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)

instance Boundaries B11 where
  fromValue x = B11_ (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x) (-x, x)  (-x, x)
  fromBound b = B11_ b b b b b b b b b b b

instance Position P11 B11 where
  pmap f (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = P11 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11)
  pmap _ _ = error "Error: pmap"

  psum (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11
  psum _ = error "Error: psum"

  pprod (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10 * x11
  pprod _ = error "Error: pprod"

  projection i (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = i A.== 0 ? (x1, i A.== 1 ? (x2, i A.== 2 ? (x3, i A.== 3 ? (x4, i A.== 4 ? (x5, i A.== 5 ? (x6, i A.== 6 ? (x7, i A.== 7 ? (x8, i A.== 8 ? (x9, i A.== 9 ? (x10, x11))))))))))
  projection _ _ = error "Error: projection"

  randomPosition (B11 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11) = RandomT . StateT $ \s0 ->
    let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s0
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
    in return (zipWith11 P11 x1s x2s x3s x4s x5s x6s x7s x8s x9s x10s x11s, s11)
  randomPosition _ = error "Error: randomPosition"

  fixBounds (B11 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11) (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = 
    let T2 minb1 maxb1 = b1
        T2 minb2 maxb2 = b2
        T2 minb3 maxb3 = b3
        T2 minb4 maxb4 = b4
        T2 minb5 maxb5 = b5
        T2 minb6 maxb6 = b6
        T2 minb7 maxb7 = b7
        T2 minb8 maxb8 = b8
        T2 minb9 maxb9 = b9
        T2 minb10 maxb10 = b10
        T2 minb11 maxb11 = b11

        x1' = x1 A.< minb1 ? (minb1, x1 A.> maxb1 ? (maxb1, x1))
        x2' = x2 A.< minb2 ? (minb2, x2 A.> maxb2 ? (maxb2, x2))
        x3' = x3 A.< minb3 ? (minb3, x3 A.> maxb3 ? (maxb3, x3))
        x4' = x4 A.< minb4 ? (minb4, x4 A.> maxb4 ? (maxb4, x4))
        x5' = x5 A.< minb5 ? (minb5, x5 A.> maxb5 ? (maxb5, x5))
        x6' = x6 A.< minb6 ? (minb6, x6 A.> maxb6 ? (maxb6, x6))
        x7' = x7 A.< minb7 ? (minb7, x7 A.> maxb7 ? (maxb7, x7))
        x8' = x8 A.< minb8 ? (minb8, x8 A.> maxb8 ? (maxb8, x8))
        x9' = x9 A.< minb9 ? (minb9, x9 A.> maxb9 ? (maxb9, x9))
        x10' = x10 A.< minb10 ? (minb10, x10 A.> maxb10 ? (maxb10, x10))
        x11' = x11 A.< minb11 ? (minb11, x11 A.> maxb11 ? (maxb11, x11))
    in P11 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11'
  fixBounds _ _ = error "Error: fixBounds"


  difference (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (P11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) = P11 (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11)
  difference _ _ = error "Error: difference"

  prod (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (P11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) = P11 (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11)
  prod _ _ = error "Error: prod"

  add (P11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (P11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) = P11 (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11)
  add _ _ = error "Error: add"