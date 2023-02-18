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
import qualified Data.Array.Accelerate.Data.Sort.Merge as A

-- | Create random number generator. The source of entropy is given by MWC
createGenerator :: Int -> IO (Acc SFC.Gen)
createGenerator n = createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)

-- | Container
newContainer :: Exp Int -> Exp R -> Acc SFC.Gen -> Container
newContainer it evr gen = A.lift (unit $ A.lift (it,evr), gen)
{-# INLINE newContainer #-}

-- Update Container
updateContainer :: Container -> Exp Int -> Acc SFC.Gen -> Container
updateContainer c it gen =
  let it' = getIt c + it
   in newContainer it' (getEvr c) gen
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
{-# INLINE getGen #-}


d6 :: forall p b. Position p b => Exp p -> Exp p -> Exp R
d6 p1 p2 = psum $ prod p1 p2