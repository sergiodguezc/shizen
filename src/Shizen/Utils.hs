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
import qualified Prelude as P

-- | Create random number generator. The source of entropy is given by MWC
createGenerator :: Int -> P.IO (Acc SFC.Gen)
createGenerator n = createWith . use P.<$> MWC.randomArray MWC.uniform (Z :. n)

-- | Map fst
mfst :: (Elt a, Elt b) => Acc (Vector (a, b)) -> Acc (Vector a)
mfst = map fst

-- | Map snd
msnd :: (Elt a, Elt b) => Acc (Vector (a, b)) -> Acc (Vector b)
msnd = map snd

-- | Get point with minimum value
minimumPoint :: Elt p => Acc (Vector (Point p)) -> Acc (Scalar (Point p))
minimumPoint = fold1 (\(T2 p1 o1) (T2 p2 o2) -> o1 < o2 ? (T2 p1 o1, T2 p2 o2))

-- | Retrieve the first element of an array
head :: Elt a => Acc (Vector a) -> Exp a
head xs = xs ! I1 0
