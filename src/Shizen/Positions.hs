{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Shizen.Positions
  ( module Shizen.Positions
  )
where

import Data.Array.Accelerate as A
import Control.Monad.State
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
    fromValue x = B1_ (-x, x)
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

    fixBounds (B1 b) (P1 x ) =
      let T2 minb1 maxb1 = b
          --
          x' = x A.< minb1 ? (minb1, x A.> maxb1 ? (maxb1, x))
       in P1 x'
    fixBounds _ _ = error "Error: Cannot update position"

    randomPosition (B1 b) = RandomT . StateT $ \(Gen s) ->
      let (xs, s1) = A.unzip $ A.map (uniformRange b) s
        in return (A.map P1 xs , Gen s1)
    randomPosition _ = error "Error: randomPosition"


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
    fromValue x = B2_ (-x, x) (-x, x)
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

    randomPosition (B2 b1 b2) = RandomT . StateT $ \(Gen s) ->
      let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
          (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
        in return (A.zipWith P2 xs ys, Gen s2)
    randomPosition _ = error "Error: randomPosition"


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
    fromValue x = B3_ (-x, x) (-x, x) (-x, x)
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

    randomPosition (B3 b1 b2 b3) = RandomT . StateT $ \(Gen s) ->
      let (xs, s1) = A.unzip $ A.map (uniformRange b1) s
          (ys, s2) = A.unzip $ A.map (uniformRange b2) s1
          (zs, s3) = A.unzip $ A.map (uniformRange b3) s2
        in return (A.zipWith3 P3 xs ys zs, Gen s3)
    randomPosition _ = error "Error: randomPosition"


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
    fromValue x = B4_ (-x, x) (-x, x) (-x, x) (-x, x)
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

    randomPosition (B4 b1 b2 b3 b4) = RandomT . StateT $ \(Gen s) ->
      let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
          (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
          (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
          (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
        in return (A.zipWith4 P4 x1s x2s x3s x4s, Gen s4)
    randomPosition _ = error "Error: randomPosition"

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
    fromValue x = B5_ (-x, x) (-x, x) (-x, x) (-x, x) (-x, x)
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

        randomPosition (B5 b1 b2 b3 b4 b5) = RandomT . StateT $ \(Gen s) ->
            let (x1s, s1) = A.unzip $ A.map (uniformRange b1) s
                (x2s, s2) = A.unzip $ A.map (uniformRange b2) s1
                (x3s, s3) = A.unzip $ A.map (uniformRange b3) s2
                (x4s, s4) = A.unzip $ A.map (uniformRange b4) s3
                (x5s, s5) = A.unzip $ A.map (uniformRange b5) s4
            in return (A.zipWith5 P5 x1s x2s x3s x4s x5s, Gen s5)
        randomPosition _ = error "Error: randomPosition"
