{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall #-}

module Shizen.DifferentialEvolution.Types
  ( module Shizen.Types,
    module Shizen.DifferentialEvolution.Types,
  )
where

import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Data.Sort.MyMerge
import Data.Array.Accelerate.System.Random.SFC
import Shizen.Types
import qualified Prelude as P

class Position p b => DEPosition p b | p -> b, b -> p where
  updatePosition :: Exp R -> Exp R -> Exp b -> Exp p -> Exp p -> Exp p -> Exp p -> Exp SFC64 -> Exp (p, SFC64)


instance DEPosition P1 B1 where
    updatePosition cr dw bound x a b c g =
        let -- Mutation
            y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
            -- Recombination
            T2 r1 g' = uniform g :: Exp (R, SFC64)
            x1 = r1 < cr ? (projection 0 y, projection 0 x)

            newX = P1 x1
            
        in T2 newX g

instance DEPosition P2 B2 where
    updatePosition cr dw bound x a b c g =
        let -- Mutation
            y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
            -- Recombination
            T2 r1 g' = uniform g :: Exp (R, SFC64)
            x1 = r1 < cr ? (projection 0 y, projection 0 x)
            T2 r2 g'' = uniform g' :: Exp (R, SFC64)
            x2 = r2 < cr ? (projection 1 y, projection 1 x)

            newX = P2 x1 x2

        in T2 newX g''


instance DEPosition P3 B3 where
  updatePosition cr dw bound x a b c g =
    let -- Mutation
        y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
        -- Recombination
        T2 r1 g' = uniform g :: Exp (R, SFC64)
        x1 = r1 < cr ? (projection 0 y, projection 0 x)
        T2 r2 g'' = uniform g' :: Exp (R, SFC64)
        x2 = r2 < cr ? (projection 1 y, projection 1 x)
        T2 r3 g''' = uniform g'' :: Exp (R, SFC64)
        x3 = r3 < cr ? (projection 2 y, projection 2 x)

        newX = P3 x1 x2 x3

    in T2 newX g'''
  
instance DEPosition P4 B4 where
  updatePosition cr dw bound x a b c g =
    let -- Mutation
        y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
        -- Recombination
        T2 r1 g1 = uniform g :: Exp (R, SFC64)
        x1 = r1 < cr ? (projection 0 y, projection 0 x)
        T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
        x2 = r2 < cr ? (projection 1 y, projection 1 x)
        T2 r3 g3 = uniform g2 :: Exp (R, SFC64)
        x3 = r3 < cr ? (projection 2 y, projection 2 x)
        T2 r4 g4 = uniform g3 :: Exp (R, SFC64)
        x4 = r4 < cr ? (projection 3 y, projection 3 x)

        newX = P4 x1 x2 x3 x4

    in T2 newX g4
  
instance DEPosition P5 B5 where
  updatePosition cr dw bound x a b c g =
    let -- Mutation
        y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
        -- Recombination
        T2 r1 g1 = uniform g :: Exp (R, SFC64)
        x1 = r1 < cr ? (projection 0 y, projection 0 x)
        T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
        x2 = r2 < cr ? (projection 1 y, projection 1 x)
        T2 r3 g3 = uniform g2 :: Exp (R, SFC64)
        x3 = r3 < cr ? (projection 2 y, projection 2 x)
        T2 r4 g4 = uniform g3 :: Exp (R, SFC64)
        x4 = r4 < cr ? (projection 3 y, projection 3 x)
        T2 r5 g5 = uniform g4 :: Exp (R, SFC64)
        x5 = r5 < cr ? (projection 4 y, projection 4 x)

        newX = P5 x1 x2 x3 x4 x5

    in T2 newX g5

instance DEPosition P6 B6 where
  updatePosition cr dw bound x a b c g =
    let -- Mutation
        y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
        -- Recombination
        T2 r1 g1 = uniform g :: Exp (R, SFC64)
        x1 = r1 < cr ? (projection 0 y, projection 0 x)
        T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
        x2 = r2 < cr ? (projection 1 y, projection 1 x)
        T2 r3 g3 = uniform g2 :: Exp (R, SFC64)
        x3 = r3 < cr ? (projection 2 y, projection 2 x)
        T2 r4 g4 = uniform g3 :: Exp (R, SFC64)
        x4 = r4 < cr ? (projection 3 y, projection 3 x)
        T2 r5 g5 = uniform g4 :: Exp (R, SFC64)
        x5 = r5 < cr ? (projection 4 y, projection 4 x)
        T2 r6 g6 = uniform g5 :: Exp (R, SFC64)
        x6 = r6 < cr ? (projection 5 y, projection 5 x)

        newX = P6 x1 x2 x3 x4 x5 x6

    in T2 newX g6
  
instance DEPosition P7 B7 where
  updatePosition cr dw bound x a b c g =
    let -- Mutation
        y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
        -- Recombination
        T2 r1 g1 = uniform g :: Exp (R, SFC64)
        x1 = r1 < cr ? (projection 0 y, projection 0 x)
        T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
        x2 = r2 < cr ? (projection 1 y, projection 1 x)
        T2 r3 g3 = uniform g2 :: Exp (R, SFC64)
        x3 = r3 < cr ? (projection 2 y, projection 2 x)
        T2 r4 g4 = uniform g3 :: Exp (R, SFC64)
        x4 = r4 < cr ? (projection 3 y, projection 3 x)
        T2 r5 g5 = uniform g4 :: Exp (R, SFC64)
        x5 = r5 < cr ? (projection 4 y, projection 4 x)
        T2 r6 g6 = uniform g5 :: Exp (R, SFC64)
        x6 = r6 < cr ? (projection 5 y, projection 5 x)
        T2 r7 g7 = uniform g6 :: Exp (R, SFC64)
        x7 = r7 < cr ? (projection 6 y, projection 6 x)

        newX = P7 x1 x2 x3 x4 x5 x6 x7

    in T2 newX g7

instance DEPosition P8 B8 where
  updatePosition cr dw bound x a b c g =
    let -- Mutation
        y = fixBounds bound $ add a (pmap (*dw) $ difference b c)
        -- Recombination
        T2 r1 g1 = uniform g :: Exp (R, SFC64)
        x1 = r1 < cr ? (projection 0 y, projection 0 x)
        T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
        x2 = r2 < cr ? (projection 1 y, projection 1 x)
        T2 r3 g3 = uniform g2 :: Exp (R, SFC64)
        x3 = r3 < cr ? (projection 2 y, projection 2 x)
        T2 r4 g4 = uniform g3 :: Exp (R, SFC64)
        x4 = r4 < cr ? (projection 3 y, projection 3 x)
        T2 r5 g5 = uniform g4 :: Exp (R, SFC64)
        x5 = r5 < cr ? (projection 4 y, projection 4 x)
        T2 r6 g6 = uniform g5 :: Exp (R, SFC64)
        x6 = r6 < cr ? (projection 5 y, projection 5 x)
        T2 r7 g7 = uniform g6 :: Exp (R, SFC64)
        x7 = r7 < cr ? (projection 6 y, projection 6 x)
        T2 r8 g8 = uniform g7 :: Exp (R, SFC64)
        x8 = r8 < cr ? (projection 7 y, projection 7 x)

        newX = P8 x1 x2 x3 x4 x5 x6 x7 x8

    in T2 newX g8

instance DEPosition P9 B9 where
  updatePosition cr dw bound x a b c g =
    let P9 x1 x2 x3 = x
        P9 a1 a2 a3 = a
        P9 b1 b2 b3 = b
        P9 c1 c2 c3 = c
        B9 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P9 newX1 newX2 newX3

    in T2 newX g3

instance DEPosition P10 B10 where
  updatePosition cr dw bound x a b c g =
    let P10 x1 x2 x3 = x
        P10 a1 a2 a3 = a
        P10 b1 b2 b3 = b
        P10 c1 c2 c3 = c
        B10 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P10 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P11 B11 where
  updatePosition cr dw bound x a b c g =
    let P11 x1 x2 x3 = x
        P11 a1 a2 a3 = a
        P11 b1 b2 b3 = b
        P11 c1 c2 c3 = c
        B11 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P11 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P12 B12 where
  updatePosition cr dw bound x a b c g =
    let P12 x1 x2 x3 = x
        P12 a1 a2 a3 = a
        P12 b1 b2 b3 = b
        P12 c1 c2 c3 = c
        B12 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P12 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P13 B13 where
  updatePosition cr dw bound x a b c g =
    let P13 x1 x2 x3 = x
        P13 a1 a2 a3 = a
        P13 b1 b2 b3 = b
        P13 c1 c2 c3 = c
        B13 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P13 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P14 B14 where
  updatePosition cr dw bound x a b c g =
    let P14 x1 x2 x3 = x
        P14 a1 a2 a3 = a
        P14 b1 b2 b3 = b
        P14 c1 c2 c3 = c
        B14 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P14 newX1 newX2 newX3 

    in T2 newX g3
  
instance DEPosition P15 B15 where
  updatePosition cr dw bound x a b c g =
    let P15 x1 x2 x3 = x
        P15 a1 a2 a3 = a
        P15 b1 b2 b3 = b
        P15 c1 c2 c3 = c
        B15 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P15 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P16 B16 where
  updatePosition cr dw bound x a b c g =
    let P16 x1 x2 x3 = x
        P16 a1 a2 a3 = a
        P16 b1 b2 b3 = b
        P16 c1 c2 c3 = c
        B16 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P16 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P17 B17 where
  updatePosition cr dw bound x a b c g =
    let P17 x1 x2 x3 = x
        P17 a1 a2 a3 = a
        P17 b1 b2 b3 = b
        P17 c1 c2 c3 = c
        B17 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P17 newX1 newX2 newX3 

    in T2 newX g3


instance DEPosition P18 B18 where
  updatePosition cr dw bound x a b c g =
    let P18 x1 x2 x3 = x
        P18 a1 a2 a3 = a
        P18 b1 b2 b3 = b
        P18 c1 c2 c3 = c
        B18 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P18 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P19 B19 where
  updatePosition cr dw bound x a b c g =
    let P19 x1 x2 x3 = x
        P19 a1 a2 a3 = a
        P19 b1 b2 b3 = b
        P19 c1 c2 c3 = c
        B19 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P19 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P20 B20 where
  updatePosition cr dw bound x a b c g =
    let P20 x1 x2 x3 = x
        P20 a1 a2 a3 = a
        P20 b1 b2 b3 = b
        P20 c1 c2 c3 = c
        B20 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P20 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P21 B21 where
  updatePosition cr dw bound x a b c g =
    let P21 x1 x2 x3 = x
        P21 a1 a2 a3 = a
        P21 b1 b2 b3 = b
        P21 c1 c2 c3 = c
        B21 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P21 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P22 B22 where
  updatePosition cr dw bound x a b c g =
    let P22 x1 x2 x3 = x
        P22 a1 a2 a3 = a
        P22 b1 b2 b3 = b
        P22 c1 c2 c3 = c
        B22 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P22 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P23 B23 where
  updatePosition cr dw bound x a b c g =
    let P23 x1 x2 x3 = x
        P23 a1 a2 a3 = a
        P23 b1 b2 b3 = b
        P23 c1 c2 c3 = c
        B23 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P23 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P24 B24 where
  updatePosition cr dw bound x a b c g =
    let P24 x1 x2 x3 = x
        P24 a1 a2 a3 = a
        P24 b1 b2 b3 = b
        P24 c1 c2 c3 = c
        B24 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P24 newX1 newX2 newX3 

    in T2 newX g3
  
instance DEPosition P25 B25 where
  updatePosition cr dw bound x a b c g =
    let P25 x1 x2 x3 = x
        P25 a1 a2 a3 = a
        P25 b1 b2 b3 = b
        P25 c1 c2 c3 = c
        B25 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P25 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P26 B26 where
  updatePosition cr dw bound x a b c g =
    let P26 x1 x2 x3 = x
        P26 a1 a2 a3 = a
        P26 b1 b2 b3 = b
        P26 c1 c2 c3 = c
        B26 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P26 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P27 B27 where
  updatePosition cr dw bound x a b c g =
    let P27 x1 x2 x3 = x
        P27 a1 a2 a3 = a
        P27 b1 b2 b3 = b
        P27 c1 c2 c3 = c
        B27 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P27 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P28 B28 where
  updatePosition cr dw bound x a b c g =
    let P28 x1 x2 x3 = x
        P28 a1 a2 a3 = a
        P28 b1 b2 b3 = b
        P28 c1 c2 c3 = c
        B28 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P28 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P29 B29 where
  updatePosition cr dw bound x a b c g =
    let P29 x1 x2 x3 = x
        P29 a1 a2 a3 = a
        P29 b1 b2 b3 = b
        P29 c1 c2 c3 = c
        B29 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P29 newX1 newX2 newX3 

    in T2 newX g3

instance DEPosition P30 B30 where
  updatePosition cr dw bound x a b c g =
    let P30 x1 x2 x3 = x
        P30 a1 a2 a3 = a
        P30 b1 b2 b3 = b
        P30 c1 c2 c3 = c
        B30 bound1 bound2 bound3 = bound

        T2 newX1 g1 = updatePosition cr dw bound1 x1 a1 b1 c1 g
        T2 newX2 g2 = updatePosition cr dw bound2 x2 a2 b2 c2 g1
        T2 newX3 g3 = updatePosition cr dw bound3 x3 a3 b3 c3 g2

        newX = P30 newX1 newX2 newX3 

    in T2 newX g3