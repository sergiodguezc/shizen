module Shizen.MultimodalFunctions
  ( module Shizen.MultimodalFunctions,
  )
where

import Data.Array.Accelerate as A
import Shizen.Types

-- Multimodal functions

-----------------------------------------
-- Sphere Model

f1' :: Exp P5 -> Exp R
f1' p = psum $ prod p p

b1' :: B5
b1' = fromValue 10

f1min' :: R
f1min' = 0

f1 :: Exp P30 -> Exp R
f1 p = psum $ prod p p

b1 :: B30
b1 = fromValue 100

f1min :: R
f1min = 0

-----------------------------------------
-- Schwefel's Problem 2.22

f2 :: Exp P30 -> Exp R
f2 p =
  let pabs = pmap A.abs p
   in psum pabs + pprod pabs

b2 :: B30
b2 = fromValue 10

f2min :: R
f2min = 0

-----------------------------------------
-- Schwefel's Problem 1.2
f3 :: Exp P30 -> Exp R
f3 p =
  fst3 $
    A.while
      (\(T3 _ _ it) -> it A.< 30)
      ( \(T3 res accum it) ->
          let accum' = accum + projection it p
           in T3 (res + accum' A.** 2) accum' (it + 1)
      )
      (T3 (projection 0 p A.** 2) (projection 0 p) 1)
  where
    fst3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
    fst3 (T3 x _ _) = x

b3 :: B30
b3 = fromValue 100

f3min :: R
f3min = 0

-----------------------------------------
-- Schwefel's Problem 2.21 (Maximum abs)
f4 :: Exp P30 -> Exp R
f4 p =
  let pabs = pmap A.abs p
      loop =
        A.while
          (\(T2 _ it) -> it A.> 0)
          ( \(T2 currentMax it) ->
              let newMax = projection it pabs
               in A.lift (A.max currentMax newMax, it - 1)
          )
          (T2 (projection 0 pabs) 29)
   in A.fst loop

b4 :: B30
b4 = fromValue 100

f4min :: R
f4min = 0

-----------------------------------------
-- Generalized Rosenbrock's Function
f5 :: Exp P30 -> Exp R
f5 p =
  let loop =
        A.while
          (\(T2 _ it) -> it A.< 29)
          ( \(T2 accum it) ->
              let xi = projection it p
                  xi1 = projection (it + 1) p
               in A.lift (accum + 100 * (xi1 - (xi A.** 2)) A.** 2 + (xi - 1) A.** 2, it + 1)
          )
          (T2 0 0)
   in A.fst loop

b5 :: B30
b5 = fromValue 30

f5min :: R
f5min = 0

-----------------------------------------
-- Step Function

f6 :: Exp P30 -> Exp R
f6 p =
  let p' = pmap (\x -> (A.fromIntegral (A.floor (x + 0.5) :: Exp Int) :: Exp R) A.** 2) p
   in psum p'

b6 :: B30
b6 = fromValue 100

f6min :: R
f6min = 0

-----------------------------------------
--

f8 :: Exp P30 -> Exp R
f8 p =
  let p' = pmap (\xi -> - xi * A.sin (A.sqrt (A.abs xi))) p
   in psum p'

b8 :: B30
b8 = fromValue 500

f8min :: R
f8min = -12569.5

-----------------------------------------
--

f9 :: Exp P30 -> Exp R
f9 p =
  let p' = pmap (\xi -> xi A.** 2 + 10 * A.cos (2 * A.pi * xi) + 10) p
   in psum p'

b9 :: B30
b9 = fromValue 5.12

f9min :: R
f9min = 0

-----------------------------------------
--

f10 :: Exp P30 -> Exp R
f10 p =
  let p1 = -20 * A.exp (-0.2 * A.sqrt ((1 / 30) * psum (prod p p)))
      p2 = -1 * A.exp ((1 / 30) * psum (pmap (\x -> A.cos (2 * A.pi * x)) p))
   in p1 + p2 + 20 + A.exp 1

b10 :: B30
b10 = fromValue 32

f10min :: R
f10min = 0

-----------------------------------------
--

f11 :: Exp P30 -> Exp R
f11 p =
  let p1 = (1 / 4000) * psum (prod p p)
      p2 = A.fst $ A.while (\(T2 _ it) -> it A.< 30) (\(T2 accum it) -> A.lift (accum * A.cos (projection it p / A.sqrt (A.fromIntegral it)), it + 1)) (T2 0 0)
   in p1 - p2 + 1

b11 :: B30
b11 = fromValue 600

f11min :: R
f11min = 0

-----------------------------------------
