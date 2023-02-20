{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Shizen.Utils
  ( module Shizen.Utils,
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC as MWC
import Data.Array.Accelerate.System.Random.SFC as SFC

-- | Create random number generator. The source of entropy is given by MWC
createGenerator :: Int -> IO (Acc SFC.Gen)
createGenerator n = createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)