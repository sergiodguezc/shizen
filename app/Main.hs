----------------------------------------------------------------------------
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
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Shizen.AntColony.AntColony as ACO
import Shizen.AntColony.Positions
import Shizen.AntColony.Types
import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU

-- Global parameters
evaporationRate :: R
evaporationRate = 0.9

-- Benchmark functions

-- -- Simple function
-- -- Global minimum at x = 3
-- bp1 :: Exp P1 -> Exp Objective
-- bp1 p = x A.<= 1 ? (x**2, (x - 3)**2 -3)
--     where
--         x = projection 0 p

-- -- Archive size
-- as1 :: Int
-- as1 = 10

-- -- New ants
-- na1 :: Int
-- na1 = 4

-- -- Search space
-- ss1 :: B1
-- ss1 = fromBound (0, 10)

-- -- Number of iterations
-- ni1 :: Int
-- ni1 = 100

-- --
-- -- Sphere function R2
-- --
-- -- Global minimum at (x, y) = (0, 0)
-- bp2 :: Exp P2 -> Exp Objective
-- bp2 p = psum $ pmap (** 2) p
-- --
-- -- Archive size
-- as2 :: Int
-- as2 = 10

-- -- New ants
-- na2 :: Int
-- na2 = 4

-- -- Search space
-- ss2 :: B2
-- ss2 = fromValue 10
-- --
-- -- Number of iterations
-- ni2 :: Int
-- ni2 = 250

-- Function with many local minima
-- Global minimum at (x, y, z) = (0, 0, 0)
bp3 :: Exp P9 -> Exp Objective
bp3 p = psum $ pmap (\x -> x ** 2 - 10 * A.cos (2 * pi * x) + 10) p

-- Archive size
as3 :: Int
as3 = 60

-- New ants
na3 :: Int
na3 = 30

-- Search space
ss3 :: B9
ss3 = fromValue 40

-- Number of iterations
ni3 :: Int
ni3 = 40


main :: IO ()
main = do
    -- putStrLn "Ant Colony Optimization Test"
    -- putStrLn "Benchmark 1"
    -- b1 <- ACO.aco as1 na1 ss1 True bp1 evaporationRate ni1
    -- putStrLn "Global minimum: x = 3, f(3) = -3"
    -- print $ CPU.run b1
    -- putStrLn "Benchmark 2"
    -- b2 <- ACO.aco as2 na2 ss2 True bp2 evaporationRate ni2
    -- putStrLn "Global minimum: (x, y) = (0, 0), f(0,0) = 0"
    -- print $ CPU.run b2
    -- putStrLn "Benchmark 3"
    b3 <- ACO.aco as3 na3 ss3 True bp3 evaporationRate ni3
    -- putStrLn "Global minimum: (x, y, z) = (0, 0, 0), f(0,0,0) = 0"
    print $ CPU.run b3
