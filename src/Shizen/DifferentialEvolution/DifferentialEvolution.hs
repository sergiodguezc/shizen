{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Shizen.DifferentialEvolution.DifferentialEvolution where

import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Data.Sort.MyMerge
import Data.Array.Accelerate.System.Random.SFC
-- import Data.Function
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
  P.IO (Acc (Scalar (Point p)))
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

        -- points = sortBy (compare `on` snd) (zip positions objective)
        points = zip positions objective


        step :: Acc (Vector (Point p)) -> Acc Gen  -> Acc (Vector (Point p), Gen)
        step o g = updatePoints n' o b' g f cr' dw'

        -- Algorithm loop
        T3 loop _ _ =
          A.awhile
            (\(T3 _ it _) -> map (< maxite) it)
            ( \(T3 old it gen1) ->
                let T2 new gen2 = step old gen1
                    it' = map (+ 1) it
                 in T3 new it' gen2
            )
            (T3 points (unit 0) gen')
        output = minimumPoint loop :: Acc (Scalar (Point p))
     in -- Return the best ant
        P.return output

minimumPoint :: Elt p => Acc (Vector (Point p)) -> Acc (Scalar (Point p))
minimumPoint = fold1 (\(T2 p1 o1) (T2 p2 o2) -> o1 < o2 ? (T2 p1 o1, T2 p2 o2))

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
    -- updatePoint :: forall p b. DEPosition p b => Exp (Point p) -> Acc (Vector (Point p)) -> Exp SFC64 -> Exp (Point p)
    updatePoint ps idxp (T2 p op) g =
      let idx = unindex1 idxp
          T4 idx1 idx2 idx3 g' = select3 n {- idx  -}g
          T2 xa _ = ps !! idx1
          T2 xb _ = ps !! idx2
          T2 xc _ = ps !! idx3

          -- We compute the new position
          T2 newX g'' = updatePosition cr dw b p xa xb xc g'
          newO = f newX

          newP = op < newO ? (T2 p op, T2 newX newO)
       in lift (newP, g'') :: Exp (Point p, SFC64)

    -- Function which selects 3 different points
    select3 :: Exp Int -> {- Exp Int -> -} Exp SFC64 -> Exp (Int, Int, Int, SFC64)
    select3 k {- idx  -}g =
      let n' = fromIntegral k :: Exp R
          T2 r1 g1 = uniform g :: Exp (R, SFC64)
          idx1 = floor $ n' * r1
          T2 r2 g2 = uniform g1 :: Exp (R, SFC64)
          idx2 = floor $ n' * r2
          T2 r3 g3 = uniform g2 :: Exp (R, SFC64)
          idx3 = floor $ n' * r3
          --
          -- T2 r1 g1 = uniform g :: Exp (R, SFC64)
          -- T2 idx1 g2 =
          --   while
          --     (\(T2 x _) -> x == idx)
          --     ( \(T2 _ g') ->
          --         let T2 r1' g1' = uniform g' :: Exp (R, SFC64)
          --          in T2 (floor $ n' * r1') g1'
          --     )
          --     (T2 (floor $ n' * r1) g1)
          -- --
          -- T2 r2 g3 = uniform g2 :: Exp (R, SFC64)
          -- T2 idx2 g4 =
          --   while
          --     (\(T2 x _) -> x == idx1)
          --     ( \(T2 _ g') ->
          --         let T2 r2' g3' = uniform g' :: Exp (R, SFC64)
          --          in T2 (floor $ n' * r2') g3'
          --     )
          --     (T2 (floor $ n' * r2) g3)
          -- --
          -- T2 r3 g5 = uniform g4 :: Exp (R, SFC64)
          -- T2 idx3 g6 =
          --   while
          --     (\(T2 x _) -> x == idx1 || x == idx2)
          --     ( \(T2 _ g') ->
          --         let T2 r3' g2' = uniform g' :: Exp (R, SFC64)
          --          in T2 (floor $ n' * r3') g2'
          --     )
          --     (T2 (floor $ n' * r3) g5)
       -- in T4 idx1 idx2 idx3 g6
       in T4 idx1 idx2 idx3 g3
    
    newPointsGen = izipWith (updatePoint old) old gen

    newPoints = map fst newPointsGen
    newGen = map snd newPointsGen

    output = lift (newPoints, newGen) :: Acc (Vector (Point p), Gen)
