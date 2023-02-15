{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Shizen.Utils
  (
    module Shizen.Utils
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC as MWC
import Shizen.AntColony.Types
import Data.Array.Accelerate.System.Random.SFC as SFC

-- | Create random number generator. The source of entropy is given by MWC
createGenerator :: Int -> IO (Acc SFC.Gen)
createGenerator n = createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)

-- | Container
newContainer :: Exp Int -> Acc SFC.Gen -> Container
newContainer it gen = A.lift (unit $ A.lift it, gen)
{-# INLINE newContainer #-}

-- Update Container
updateContainer :: Container -> Exp Int -> Acc SFC.Gen -> Container
updateContainer c it gen =
  let it' = getIt c + it
   in newContainer it' gen
{-# INLINE updateContainer #-}

-- Container Getters
getIt :: Container -> Exp Int
getIt c = the $ A.afst c
{-# INLINE getIt #-}

-- getEvr :: Container -> Exp R
-- getEvr c = A.snd $ the $ A.afst c
-- {-# INLINE getEvr #-}

getGen :: Container -> Acc SFC.Gen
getGen = A.asnd
{-# INLINE getGen #-}

-- | Function that computes the average distance between all ants and the
-- selected one at one component
averageD2 ::
  forall p b. Position p b =>
  -- | index
  Exp Int ->
  -- | selected position
  Exp p ->
  -- | ants
  Acc (Vector p) ->
  Exp R
averageD2 i p ps =
  let value = projection i p :: Exp Double
      sums = A.sum $ A.map (\p' -> (projection i p' - value)A.** 2) ps :: Acc (Scalar Double)
      elems = A.fromIntegral $ A.length ps - 1 :: Exp Double
   in the $ A.map (A./ elems) sums
{-# INLINE averageD2 #-}


