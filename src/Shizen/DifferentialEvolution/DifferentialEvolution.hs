{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Shizen.DifferentialEvolution.DifferentialEvolution where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Sort.MyMerge
import Data.Array.Accelerate.System.Random.SFC
import Data.Function
import Shizen.DifferentialEvolution.Types
import Shizen.Utils
import qualified Prelude as P

de ::
  forall p b.
  DEPosition p b =>
  -- Population size (>= 4)
  Int ->
  -- Bounds
  b ->
  -- Fitness function
  (Exp p -> Exp R) ->
  -- Crossover rate (0.0 <= CR <= 1.0)
  R ->
  -- Differential weight (0.0 <= F <= 2.0)
  R ->
  Int ->
  P.IO (Acc (Vector (Point p)))
de n b f cr dw maxit =
  do
    -- We start by creating the random generator.
    gen <- createGenerator n

    -- Now, we embed some variables into the Exp type of scalar expressions.
    let -- Parameter representing number of ants
        n' = constant n :: Exp Int
        -- Parameter representing search space.
        b' = constant b
        cr' = constant cr :: Exp R
        dw' = constant dw :: Exp R

        -- Parameter representing the iterations of the algorithm loop.
        maxite = constant maxit :: Exp Int

        -- We generate the particles positions randomly and within the boundaries
        (positions, gen') = runRandom gen (randomPositions b')

        -- We compute the function value of each position
        objective = map f positions :: Acc (Vector R)

        points = sortBy (compare `on` snd) (zip positions objective)

        -- Algorithm loop
        T3 loop _ _ =
          A.awhile
            (\(T3 _ it _) -> map (< maxite) it)
            ( \(T3 old it gen1) ->
                let T2 new gen2 = T2 old gen1
                    it' = map (+ 1) it
                 in T3 new it' gen2
            )
            (T3 points (unit 0) gen)
        output = take 1 loop :: Acc (Vector (Point p))
     in -- Return the best ant
        P.return output

updatePoints ::
  forall p b.
  DEPosition p b =>
  Exp Int ->
  Acc (Vector (Point p)) ->
  Exp b ->
  Acc Gen ->
  (Exp p -> Exp R) ->
  Exp R ->
  Exp R ->
  Acc (Vector (Point p), Gen)
updatePoints n old b gen f cr dw = output
  where
    updatePoint :: forall p b. DEPosition p b => Exp (Point p) -> Acc (Vector (Point p)) -> Exp SFC64 -> Exp (Point p)
    updatePoint p ps g =
      let T4 idx1 idx2 idx3 g' = select3 n g
          xa = ps !! idx1
          xb = ps !! idx2
          xc = ps !! idx3

          -- We compute the new position
          T2 newX g'' = updatePosition cr dw b p xa xb xc g'
          newO = f newX


       in p

    -- Function which selects 3 different points
    select3 :: Exp Int -> Exp SFC64 -> Exp (Int, Int, Int, SFC64)
    select3 n g =
      let T2 r1 g1 = uniform g :: Exp (R, SFC64)
          n' = fromIntegral n :: Exp R
          idx1 = floor $ n' * r1 :: Exp Int
          T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
          T2 idx2 g3 =
            while
              (\(T2 x _) -> x == idx1)
              ( \(T2 _ g') ->
                  let T2 r2' g2' = uniform g' :: Exp (R, SFC64)
                   in T2 (floor $ n' * r2') g2'
              )
              (T2 (floor $ n' * r2) g2)
          T2 idx3 g4 =
            while
              (\(T2 x _) -> x == idx1 || x == idx2)
              ( \(T2 _ g') ->
                  let T2 r2' g2' = uniform g' :: Exp (R, SFC64)
                   in T2 (floor $ n' * r2') g2'
              )
              (T2 (floor $ n' * r2) g3)
       in T4 idx1 idx2 idx3 g4
    --
    output = lift (old, gen)
