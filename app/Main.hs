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


import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.LLVM.PTX as GPU
import Shizen.ParticleSwarm.ParticleSwarm as PSO
import Shizen.DifferentialEvolution.DifferentialEvolution as DE
import Shizen.AntColony.AntColony as ACO -- Does not work
import Shizen.MultimodalFunctions

main :: IO ()
main = do
  -- test1 <- PSO.pso 2000 b1' f1' (-0.1618) 1.8903 2.1225 100
  -- print $ GPU.run test1
  --
  test2 <- DE.de 300 b9 f9 0.5 0.8 6000
  print $ GPU.run test2
  --
  -- test3 <- ACO.aco 3000 1000 b1' f1' 0.5 100 -- Does not work
  -- print $ GPU.run test3
