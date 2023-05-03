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

