{-# LANGUAGE ScopedTypeVariables #-}

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

-- import Shizen.RiverFormationDynamics.RiverFormationDynamics as RFD

-- import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Data.Sort.MyMerge
-- import Shizen.Utils

-- import Data.Array.Accelerate.Interpreter as INT
import Data.Array.Accelerate.LLVM.Native as CPU
-- import Data.Array.Accelerate.LLVM.PTX as GPU
-- import Data.Array.Accelerate.System.Random.MWC as MWC
-- import Data.Array.Accelerate.System.Random.SFC as SFC
-- import Shizen.AntColony.AntColony as ACO
import Shizen.ParticleSwarm.ParticleSwarm as PSO
import Shizen.MultimodalFunctions
-- import Shizen.Types

-- import Data.Array.Accelerate.System.Random.SFC (randomVector, runRandom)

-- Global parameters
-- evaporationRate :: R
-- evaporationRate = 0.9

-- minimization :: Bool
-- minimization = True

-- maximization :: Bool
-- maximization = False

-- Individual tests

main :: IO ()
main = do
  --
  -- TEST
  -- test 100

  -- putStrLn "Ant Colony Optimization Test"
  --
  -- putStrLn "Benchmark 1"
  -- test1 <- ACO.aco 700 350 b1' minimization f1' evaporationRate 1000
  test1 <- PSO.pso 200 b8 f8 (-0.16) 1.89 2.12 900
  -- print test1
  print $ CPU.run test1

-- test1 <- RFD.rfd 20 7 minimization b1' f1' 0.9 0.9 100
-- print $ GPU.run test1

--
-- putStrLn "Benchmark 2"
-- test2 <- ACO.aco 50 20 b2 minimization f2 evaporationRate 20
-- print $ GPU.run test2
--
-- putStrLn "Benchmark 3"
-- test3 <- ACO.aco 50 20 b3 minimization f3 evaporationRate 20
-- print test3
-- print $ GPU.run test3
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
-- putStrLn "Benchmark 9"
-- test9 <- ACO.aco 50 20 b9 minimization f9 evaporationRate 20
-- print $ CPU.run test9
-- putStrLn "Benchmark 10"
-- test10 <- ACO.aco 50 20 b10 minimization f10 evaporationRate 20
-- print $ CPU.run test10
-- putStrLn "Benchmark 11"
-- test11 <- ACO.aco 50 20 b11 minimization f11 evaporationRate 20
-- print $ CPU.run test11

-- test :: Int -> IO ()
-- test n = do
--   g <- createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)
--   let b' = fromBound (0, 1) :: B30
--       b = constant b'
--       (xs, g1) = runRandom g (randomPositions b)
--       os = A.map f xs :: Acc (Vector R)
--       xos = A.zip xs os
--       g2 = A.take 10 g1
--       T3 loop _ _ =
--         A.awhile
--           (\(T3 _ i _) -> A.map (A.< 1000) i)
--           ( \(T3 zs i g') ->
--               let -- i' = the i
--                   (ys, g'') = runRandom g' (randomPositions b)
--                   yss = A.replicate (A.lift (Z :. A.length g2 :. All)) ys
--                   ys' = A.fold1 add yss
--                   os' = A.map f ys'
--                   yos' = A.zip ys os'
--                   zs' = A.take 100 $ sortBy (\(T2 _ x) (T2 _ y) -> A.compare x y) (zs A.++ yos')
--                   j = A.map (+ 1) i :: Acc (Scalar Int)
--                in T3 zs' j g''
--           )
--           (T3 xos (unit 0) g2)
--   print $ GPU.run $ A.take 3 loop
--   where
--     -- print $ GPU.run loop
--
--     f :: forall p b. Position p b => Exp p -> Exp R
--     f p = psum $ difference p p
