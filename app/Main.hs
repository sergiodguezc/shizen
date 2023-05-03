{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Copyright   :  (c) Sergio Domínguez 2022
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sergdoma@ucm.es
-- Stability   :  unstable
-- Portability :  not portable, uses cuda
--
-- shizen, bio-based algorithms library
module Main (main) where


-- import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.LLVM.PTX as GPU
import Shizen.ParticleSwarm.ParticleSwarm as PSO
-- import Shizen.AntColony.AntColony as ACO
import Shizen.MultimodalFunctions

main :: IO ()
main = do
  test1 <- PSO.pso 20000 b10 f10 (-0.16) 1.89 2.12 200
  -- test2 <- ACO.aco 1000 400 b10 f10 0.9 200
  --
  print $ GPU.run test1
