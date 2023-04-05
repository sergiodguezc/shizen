{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Shizen.Utils
  ( module Shizen.Utils,
  )
where

import Data.Array.Accelerate as A
import Shizen.Types
import Data.Array.Accelerate.System.Random.MWC as MWC
import Data.Array.Accelerate.System.Random.SFC as SFC

-- | Create random number generator. The source of entropy is given by MWC
createGenerator :: Int -> IO (Acc SFC.Gen)
createGenerator n = createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)

-- Distance : D_n
distance :: (Position p b) => Exp Int -> Exp p -> Exp p -> Exp R
distance n p1 p2 =
  let diff = difference p1 p2
   in psum (pmap (\xi -> A.abs xi A.^ n) diff)  A.** (1 A./ A.fromIntegral n)


ds :: (Position p b) => Exp R -> Exp p -> Exp p -> Exp p
ds n p1 p2 =
  let diff = difference p1 p2
   in pmap (\xi -> A.abs xi A.** n) diff

d2 :: (Position p b) => Exp p -> Exp p -> Exp p
d2 p1 p2 = let diff = difference p1 p2 in pmap (A.** 2) diff
