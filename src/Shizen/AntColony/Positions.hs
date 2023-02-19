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
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        pos = fixBounds b $ P2 x1' x2'
    in T2 pos gen2
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R3

instance AntPosition P3 B3 where
  updatePosition evr b (P3 md1 md2 md3) (P3 std1 std2 std3) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        pos = fixBounds b $ P3 x1' x2' x3'
    in T2 pos gen3
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R4

instance AntPosition P4 B4 where
  updatePosition evr b (P4 md1 md2 md3 md4) (P4 std1 std2 std3 std4) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        pos = fixBounds b $ P4 x1' x2' x3' x4'
    in T2 pos gen4
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R5

instance AntPosition P5 B5 where
  updatePosition evr b (P5 md1 md2 md3 md4 md5) (P5 std1 std2 std3 std4 std5) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        pos = fixBounds b $ P5 x1' x2' x3' x4' x5'
    in T2 pos gen5
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R6 

instance AntPosition P6 B6 where
  updatePosition evr b (P6 md1 md2 md3 md4 md5 md6) (P6 std1 std2 std3 std4 std5 std6) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        pos = fixBounds b $ P6 x1' x2' x3' x4' x5' x6'
    in T2 pos gen6
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


-- R7

instance AntPosition P7 B7 where
  updatePosition evr b (P7 md1 md2 md3 md4 md5 md6 md7) (P7 std1 std2 std3 std4 std5 std6 std7) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        pos = fixBounds b $ P7 x1' x2' x3' x4' x5' x6' x7'
    in T2 pos gen7
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

-- R8

instance AntPosition P8 B8 where
  updatePosition evr b (P8 md1 md2 md3 md4 md5 md6 md7 md8) (P8 std1 std2 std3 std4 std5 std6 std7 std8) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        pos = fixBounds b $ P8 x1' x2' x3' x4' x5' x6' x7' x8'
    in T2 pos gen8
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"



-- R9

instance AntPosition P9 B9 where
  updatePosition evr b (P9 md1 md2 md3 md4 md5 md6 md7 md8 md9) (P9 std1 std2 std3 std4 std5 std6 std7 std8 std9) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        pos = fixBounds b $ P9 x1' x2' x3' x4' x5' x6' x7' x8' x9'
    in T2 pos gen9
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"
  

instance AntPosition P10 B10 where
  updatePosition evr b (P10 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10) (P10 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        pos = fixBounds b $ P10 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10'
    in T2 pos gen10
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P11 B11 where
  updatePosition evr b (P11 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10 md11) (P11 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10 std11) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        (T2 x11' gen11) = normal md11 (evr * std11) gen10
        pos = fixBounds b $ P11 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11'
    in T2 pos gen11
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P12 B12 where
  updatePosition evr b (P12 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10 md11 md12) (P12 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10 std11 std12) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        (T2 x11' gen11) = normal md11 (evr * std11) gen10
        (T2 x12' gen12) = normal md12 (evr * std11) gen11
        pos = fixBounds b $ P12 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11' x12'
    in T2 pos gen12
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P13 B13 where
  updatePosition evr b (P13 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10 md11 md12 md13) (P13 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10 std11 std12 std13) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        (T2 x11' gen11) = normal md11 (evr * std11) gen10
        (T2 x12' gen12) = normal md12 (evr * std11) gen11
        (T2 x13' gen13) = normal md13 (evr * std13) gen12
        pos = fixBounds b $ P13 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11' x12' x13'
    in T2 pos gen13
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


instance AntPosition P14 B14 where
  updatePosition evr b (P14 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10 md11 md12 md13 md14) (P14 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10 std11 std12 std13 std14) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        (T2 x11' gen11) = normal md11 (evr * std11) gen10
        (T2 x12' gen12) = normal md12 (evr * std11) gen11
        (T2 x13' gen13) = normal md13 (evr * std13) gen12
        (T2 x14' gen14) = normal md14 (evr * std14) gen13
        pos = fixBounds b $ P14 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11' x12' x13' x14'
    in T2 pos gen14
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P15 B15 where
  updatePosition evr b (P15 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10 md11 md12 md13 md14 md15) (P15 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10 std11 std12 std13 std14 std15) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        (T2 x11' gen11) = normal md11 (evr * std11) gen10
        (T2 x12' gen12) = normal md12 (evr * std11) gen11
        (T2 x13' gen13) = normal md13 (evr * std13) gen12
        (T2 x14' gen14) = normal md14 (evr * std14) gen13
        (T2 x15' gen15) = normal md15 (evr * std15) gen14
        pos = fixBounds b $ P15 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11' x12' x13' x14' x15'
    in T2 pos gen15
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"


instance AntPosition P16 B16 where
  updatePosition evr b (P16 md1 md2 md3 md4 md5 md6 md7 md8 md9 md10 md11 md12 md13 md14 md15 md16) (P16 std1 std2 std3 std4 std5 std6 std7 std8 std9 std10 std11 std12 std13 std14 std15 std16) gen =
    let (T2 x1' gen1) = normal md1 (evr * std1) gen
        (T2 x2' gen2) = normal md2 (evr * std2) gen1
        (T2 x3' gen3) = normal md3 (evr * std3) gen2
        (T2 x4' gen4) = normal md4 (evr * std4) gen3
        (T2 x5' gen5) = normal md5 (evr * std5) gen4
        (T2 x6' gen6) = normal md6 (evr * std6) gen5
        (T2 x7' gen7) = normal md7 (evr * std7) gen6
        (T2 x8' gen8) = normal md8 (evr * std8) gen7
        (T2 x9' gen9) = normal md9 (evr * std9) gen8
        (T2 x10' gen10) = normal md10 (evr * std10) gen9
        (T2 x11' gen11) = normal md11 (evr * std11) gen10
        (T2 x12' gen12) = normal md12 (evr * std11) gen11
        (T2 x13' gen13) = normal md13 (evr * std13) gen12
        (T2 x14' gen14) = normal md14 (evr * std14) gen13
        (T2 x15' gen15) = normal md15 (evr * std15) gen14
        (T2 x16' gen16) = normal md16 (evr * std16) gen15
        pos = fixBounds b $ P16 x1' x2' x3' x4' x5' x6' x7' x8' x9' x10' x11' x12' x13' x14' x15' x16'
    in T2 pos gen16
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"

instance AntPosition P17 B17 where
  updatePosition evr b (P17 md1 md17) (P17 std1 std17) gen =
    let B17 b1 b2 = b
        (T2 x1' gen1) = updatePosition evr b1 md1 std1 gen
        (T2 x17' gen17) = normal md17 (evr * std17) gen1
        pos = fixBounds b $ P17 x1' x17'
    in T2 pos gen17
  updatePosition _ _ _ _ _ = error "updatePosition: impossible case"