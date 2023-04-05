----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

-- import Shizen.RiverFormationDynamics.RiverFormationDynamics as RFD

-- import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Data.Sort.MyMerge
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import Shizen.AntColony.AntColony as ACO
import Shizen.MultimodalFunctions
import Shizen.Types

-- Global parameters
evaporationRate :: R
evaporationRate = 0.9

minimization :: Bool
minimization = True

-- maximization :: Bool
-- maximization = False

-- Individual tests

main :: IO ()
main = do
  -- putStrLn "Ant Colony Optimization Test"
  --
  -- putStrLn "Benchmark 1"
  test1 <- ACO.aco 2000 900 b1 minimization f1 evaporationRate 1600
  print $ CPU.run test1
  --
  -- putStrLn "Benchmark 2"
  -- test2 <- ACO.aco 50 20 b2 minimization f2 evaporationRate 20
  -- print $ CPU.run test2
  --
  -- putStrLn "Benchmark 3"
  -- test3 <- ACO.aco 50 20 b3 minimization f3 evaporationRate 20
  -- print $ CPU.run test3
  --
  -- putStrLn "Benchmark 4"
  -- test4 <- ACO.aco 50 20 b4 minimization f4 evaporationRate 20
  -- print $ CPU.run test4
  --
  -- putStrLn "Benchmark 5"
  -- test5 <- ACO.aco 50 20 b5 minimization f5 evaporationRate 20
  -- print $ CPU.run test5
  --
  -- putStrLn "Benchmark 6"
  -- test6 <- ACO.aco 50 20 b6 minimization f6 evaporationRate 20
  -- print $ CPU.run test6
  --
  -- putStrLn "Benchmark 7"
  --
  -- putStrLn "Benchmark 8"
  -- test8 <- ACO.aco 3500 1500 b8 minimization f8 evaporationRate 1500
  -- print $ CPU.run test8
  --
  -- TEST MERGE
  -- testMerge

--
-- putStrLn "Benchmark 9"
-- test9 <- ACO.aco 50 20 b9 minimization f9 evaporationRate 20
-- print $ CPU.run test9
-- putStrLn "Benchmark 10"
-- test10 <- ACO.aco 50 20 b10 minimization f10 evaporationRate 20
-- print $ CPU.run test10
-- putStrLn "Benchmark 11"
-- test11 <- ACO.aco 50 20 b11 minimization f11 evaporationRate 20
-- print $ CPU.run test11

-- testMerge :: IO ()
-- testMerge = do
--   let xs = use $ A.fromList (Z :. 100) [0, 1 ..] :: Acc (Vector Int)
--       ys = use $ A.fromList (Z :. 100) [2, 4 ..] :: Acc (Vector Int)
--
--       T2 loop _ =
--         A.awhile
--           (\(T2 _ i) -> A.map (A.< 1000) i)
--           ( \(T2 xs' i) ->
--               let i' = the i :: Exp Int
--                   i'' = i' + 1
--                   ys' = A.take i' ys :: Acc (Vector Int)
--                   sorted = A.take 100 $ sort (xs' A.++ ys') :: Acc (Vector Int)
--                in T2 sorted (unit i'')
--           )
--           (T2 xs (unit 0))
--
--   print $ CPU.run $ A.take 20 loop
--   print $ CPU.run $ A.take 20 xs
--   print $ CPU.run $ A.take 20 ys
