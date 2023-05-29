{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall #-}

-- Module    : Shizen.AntColony.AntColony
-- Description : Continuous Ant Colony Optimization
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : experimental
-- Portability:
--
-- An implementation of Ant Colony optimization algorithm for continuous
-- functions.
module Shizen.AntColony.Types
  ( module Shizen.AntColony.Types,
    -- Reexport types
    module Shizen.Types,
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.SFC as SFC
import Shizen.Types

--------------------------------------------------------------------
--  Basic types to be used
--------------------------------------------------------------------

-- | Type used to represent the ant.
type Ant p = (p, R)

type VectorAnt p = Vector (Ant p)

-- | Ant getters
getPosition :: Elt p => Exp (Ant p) -> Exp p
getPosition = A.fst

getObjective :: Elt p => Exp (Ant p) -> Exp R
getObjective = A.snd

class Position p b => AntPosition p b | p -> b, b -> p where
  updatePosition :: Exp R -> Exp b -> Exp p -> Exp p -> Exp SFC64 -> Exp (p, SFC64)

epsilon :: Exp R
epsilon = 1e-1

-- R1
instance AntPosition P1 B1 where
  updatePosition evr b (P1 md) (P1 std) gen =
    let (T2 x' gen1) = normal md (evr * std + epsilon) gen
        pos = fixBounds b $ P1 x'
     in T2 pos gen1
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R2

instance AntPosition P2 B2 where
  updatePosition evr b (P2 md1 md2) (P2 std1 std2) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        pos = fixBounds b $ P2 x1' x2'
     in T2 pos gen2
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R3

instance AntPosition P3 B3 where
  updatePosition evr b (P3 md1 md2 md3) (P3 std1 std2 std3) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        (T2 x3' gen3) = normal md3 (evr * std3 + epsilon) gen2
        pos = fixBounds b $ P3 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R4

instance AntPosition P4 B4 where
  updatePosition evr b (P4 md1 md2 md3 md4) (P4 std1 std2 std3 std4) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        (T2 x3' gen3) = normal md3 (evr * std3 + epsilon) gen2
        (T2 x4' gen4) = normal md4 (evr * std4 + epsilon) gen3
        pos = fixBounds b $ P4 x1' x2' x3' x4'
     in T2 pos gen4
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R5

instance AntPosition P5 B5 where
  updatePosition evr b (P5 md1 md2 md3 md4 md5) (P5 std1 std2 std3 std4 std5) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        (T2 x3' gen3) = normal md3 (evr * std3 + epsilon) gen2
        (T2 x4' gen4) = normal md4 (evr * std4 + epsilon) gen3
        (T2 x5' gen5) = normal md5 (evr * std5 + epsilon) gen4
        pos = fixBounds b $ P5 x1' x2' x3' x4' x5'
     in T2 pos gen5
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R6

instance AntPosition P6 B6 where
  updatePosition evr b (P6 md1 md2 md3 md4 md5 md6) (P6 std1 std2 std3 std4 std5 std6) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        (T2 x3' gen3) = normal md3 (evr * std3 + epsilon) gen2
        (T2 x4' gen4) = normal md4 (evr * std4 + epsilon) gen3
        (T2 x5' gen5) = normal md5 (evr * std5 + epsilon) gen4
        (T2 x6' gen6) = normal md6 (evr * std6 + epsilon) gen5
        pos = fixBounds b $ P6 x1' x2' x3' x4' x5' x6'
     in T2 pos gen6
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R7

instance AntPosition P7 B7 where
  updatePosition evr b (P7 md1 md2 md3 md4 md5 md6 md7) (P7 std1 std2 std3 std4 std5 std6 std7) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        (T2 x3' gen3) = normal md3 (evr * std3 + epsilon) gen2
        (T2 x4' gen4) = normal md4 (evr * std4 + epsilon) gen3
        (T2 x5' gen5) = normal md5 (evr * std5 + epsilon) gen4
        (T2 x6' gen6) = normal md6 (evr * std6 + epsilon) gen5
        (T2 x7' gen7) = normal md7 (evr * std7 + epsilon) gen6
        pos = fixBounds b $ P7 x1' x2' x3' x4' x5' x6' x7'
     in T2 pos gen7
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R8

instance AntPosition P8 B8 where
  updatePosition evr b (P8 md1 md2 md3 md4 md5 md6 md7 md8) (P8 std1 std2 std3 std4 std5 std6 std7 std8) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1 + epsilon) gen
        (T2 x2' gen2) = normal md2 (evr * std2 + epsilon) gen1
        (T2 x3' gen3) = normal md3 (evr * std3 + epsilon) gen2
        (T2 x4' gen4) = normal md4 (evr * std4 + epsilon) gen3
        (T2 x5' gen5) = normal md5 (evr * std5 + epsilon) gen4
        (T2 x6' gen6) = normal md6 (evr * std6 + epsilon) gen5
        (T2 x7' gen7) = normal md7 (evr * std7 + epsilon) gen6
        (T2 x8' gen8) = normal md8 (evr * std8 + epsilon) gen7
        pos = fixBounds b $ P8 x1' x2' x3' x4' x5' x6' x7' x8'
     in T2 pos gen8
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R9

instance AntPosition P9 B9 where
  updatePosition evr (B9 b1 b2 b3) (P9 md1 md2 md3) (P9 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P9 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P10 B10 where
  updatePosition evr (B10 b1 b2 b3) (P10 md1 md2 md3) (P10 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P10 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P11 B11 where
  updatePosition evr (B11 b1 b2 b3) (P11 md1 md2 md3) (P11 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P11 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P12 B12 where
  updatePosition evr (B12 b1 b2 b3) (P12 md1 md2 md3) (P12 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P12 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P13 B13 where
  updatePosition evr (B13 b1 b2 b3) (P13 md1 md2 md3) (P13 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P13 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P14 B14 where
  updatePosition evr (B14 b1 b2 b3) (P14 md1 md2 md3) (P14 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P14 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P15 B15 where
  updatePosition evr (B15 b1 b2 b3) (P15 md1 md2 md3) (P15 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P15 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P16 B16 where
  updatePosition evr (B16 b1 b2 b3) (P16 md1 md2 md3) (P16 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P16 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P17 B17 where
  updatePosition evr (B17 b1 b2 b3) (P17 md1 md2 md3) (P17 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P17 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P18 B18 where
  updatePosition evr (B18 b1 b2 b3) (P18 md1 md2 md3) (P18 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P18 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P19 B19 where
  updatePosition evr (B19 b1 b2 b3) (P19 md1 md2 md3) (P19 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P19 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P20 B20 where
  updatePosition evr (B20 b1 b2 b3) (P20 md1 md2 md3) (P20 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P20 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P21 B21 where
  updatePosition evr (B21 b1 b2 b3) (P21 md1 md2 md3) (P21 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P21 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P22 B22 where
  updatePosition evr (B22 b1 b2 b3) (P22 md1 md2 md3) (P22 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P22 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P23 B23 where
  updatePosition evr (B23 b1 b2 b3) (P23 md1 md2 md3) (P23 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P23 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P24 B24 where
  updatePosition evr (B24 b1 b2 b3) (P24 md1 md2 md3) (P24 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P24 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P25 B25 where
  updatePosition evr (B25 b1 b2 b3) (P25 md1 md2 md3) (P25 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P25 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P26 B26 where
  updatePosition evr (B26 b1 b2 b3) (P26 md1 md2 md3) (P26 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P26 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P27 B27 where
  updatePosition evr (B27 b1 b2 b3) (P27 md1 md2 md3) (P27 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P27 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P28 B28 where
  updatePosition evr (B28 b1 b2 b3) (P28 md1 md2 md3) (P28 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P28 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P29 B29 where
  updatePosition evr (B29 b1 b2 b3) (P29 md1 md2 md3) (P29 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P29 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P30 B30 where
  updatePosition evr (B30 b1 b2 b3) (P30 md1 md2 md3) (P30 std1 std2 std3) gen =
    let (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x2' gen2) = updatePosition evr b2 md2 std2 gen1
        (T2 x3' gen3) = updatePosition evr b3 md3 std3 gen2
        pos = P30 x1' x2' x3'
     in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"
