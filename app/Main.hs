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
-- import Shizen.ParticleSwarm.ParticleSwarm
import Shizen.DifferentialEvolution.DifferentialEvolution
-- import Shizen.AntColony.AntColony
import Shizen.Functions

main :: IO ()
main = do
  -- test1 <- pso 15000 b2 f2 (-0.1618) 1.8903 2.1225 100
  -- print $ CPU.run test1
  --
  test2 <- de 300 b2 f2 0.5 0.8 400
  print $ GPU.run test2
  --
  -- test3 <- aco 800 400 b1 f1 0.5 100
  -- print $ CPU.run test3
