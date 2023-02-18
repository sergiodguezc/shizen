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
  updatePosition evr b (P1 md) (P1 std) gen =
    let (T2 x' gen1) = normal md (evr * std) gen
        pos = fixBounds b $ P1 x'
    in T2 pos gen1
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R2

instance AntPosition P2 B2 where
  updatePosition evr b (P2 md1 md2) (P2 std1 std2) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        pos = fixBounds b $ P2 x1' x2'
    in T2 pos gen2
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R3

instance AntPosition P3 B3 where
  updatePosition evr b (P3 md1 md2 md3) (P3 std1 std2 std3) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        pos = fixBounds b $ P3 x1' x2' x3'
    in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R4

instance AntPosition P4 B4 where
  updatePosition evr b (P4 md1 md2 md3 md4) (P4 std1 std2 std3 std4) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        (T2 x4' gen4) = normal md4 (evr * std4) gen
        pos = fixBounds b $ P4 x1' x2' x3' x4'
    in T2 pos gen4
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R5

instance AntPosition P5 B5 where
  updatePosition evr b (P5 md1 md2 md3 md4 md5) (P5 std1 std2 std3 std4 std5) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        (T2 x4' gen4) = normal md4 (evr * std4) gen
        (T2 x5' gen5) = normal md5 (evr * std5) gen
        pos = fixBounds b $ P5 x1' x2' x3' x4' x5'
    in T2 pos gen5
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R6 

instance AntPosition P6 B6 where
  updatePosition evr b (P6 md1 md2 md3 md4 md5 md6) (P6 std1 std2 std3 std4 std5 std6) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        (T2 x4' gen4) = normal md4 (evr * std4) gen
        (T2 x5' gen5) = normal md5 (evr * std5) gen
        (T2 x6' gen6) = normal md6 (evr * std6) gen
        pos = fixBounds b $ P6 x1' x2' x3' x4' x5' x6'
    in T2 pos gen6
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R7

instance AntPosition P7 B7 where
  updatePosition evr b (P7 md1 md2 md3 md4 md5 md6 md7) (P7 std1 std2 std3 std4 std5 std6 std7) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        (T2 x4' gen4) = normal md4 (evr * std4) gen
        (T2 x5' gen5) = normal md5 (evr * std5) gen
        (T2 x6' gen6) = normal md6 (evr * std6) gen
        (T2 x7' gen7) = normal md7 (evr * std7) gen
        pos = fixBounds b $ P7 x1' x2' x3' x4' x5' x6' x7'
    in T2 pos gen7
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R8

instance AntPosition P8 B8 where
  updatePosition evr b (P8 md1 md2 md3 md4 md5 md6 md7 md8) (P8 std1 std2 std3 std4 std5 std6 std7 std8) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        (T2 x4' gen4) = normal md4 (evr * std4) gen
        (T2 x5' gen5) = normal md5 (evr * std5) gen
        (T2 x6' gen6) = normal md6 (evr * std6) gen
        (T2 x7' gen7) = normal md7 (evr * std7) gen
        (T2 x8' gen8) = normal md8 (evr * std8) gen
        pos = fixBounds b $ P8 x1' x2' x3' x4' x5' x6' x7' x8'
    in T2 pos gen8
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"



-- R9

instance AntPosition P9 B9 where
  updatePosition evr b (P9 md1 md2 md3 md4 md5 md6 md7 md8 md9) (P9 std1 std2 std3 std4 std5 std6 std7 std8 std9) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen
        (T2 x3' gen3) = normal md3 (evr * std3) gen
        (T2 x4' gen4) = normal md4 (evr * std4) gen
        (T2 x5' gen5) = normal md5 (evr * std5) gen
        (T2 x6' gen6) = normal md6 (evr * std6) gen
        (T2 x7' gen7) = normal md7 (evr * std7) gen
        (T2 x8' gen8) = normal md8 (evr * std8) gen
        (T2 x9' gen9) = normal md9 (evr * std9) gen
        pos = fixBounds b $ P9 x1' x2' x3' x4' x5' x6' x7' x8' x9'
    in T2 pos gen9
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"
  