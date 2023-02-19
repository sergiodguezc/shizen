{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shizen.Utils
  ( module Shizen.Utils,
  )
where

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Data.Sort.Merge as A
import Data.Array.Accelerate.System.Random.MWC as MWC
import Data.Array.Accelerate.System.Random.SFC as SFC
import Shizen.AntColony.Types

-- | Create random number generator. The source of entropy is given by MWC
createGenerator :: Int -> IO (Acc SFC.Gen)
createGenerator n = createWith . use <$> MWC.randomArray MWC.uniform (Z :. n)

-- | Container
newContainer :: Exp Int -> Acc SFC.Gen -> Container
newContainer it gen = A.lift (unit it , gen)
{-# INLINE newContainer #-}

-- Update Container
updateContainer :: Container -> Exp Int -> Acc SFC.Gen -> Container
updateContainer c it gen =
  let it' = getIt c + it
   in newContainer it' gen
{-# INLINE updateContainer #-}

-- Container Getters
getIt :: Container -> Exp Int
getIt c = the $ A.afst c
{-# INLINE getIt #-}

getGen :: Container -> Acc SFC.Gen
getGen = A.asnd
{-# INLINE getGen #-}

d6 :: forall p b. Position p b => Exp p -> Exp p -> Exp R
d6 p1 p2 = psum $ prod p1 p2

-- | Used to define the zipWith functions on more than two arrays
-- Stolen from Accelerate
zipWithInduction ::
  (Shape sh, Elt a, Elt b) =>
  ((Exp (a, b) -> rest) -> Acc (Array sh (a, b)) -> result) -> -- The zipWith function operating on one fewer array
  (Exp a -> Exp b -> rest) ->
  Acc (Array sh a) ->
  Acc (Array sh b) ->
  result
zipWithInduction prev f as bs = prev (\(T2 a b) -> f a b) (A.zip as bs)

-- | ZipWith with 10 arrays
zipWith10
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
zipWith10 = zipWithInduction zipWith9

-- | ZipWith with 11 arrays
zipWith11
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k -> Exp l)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
    -> Acc (Array sh l)
zipWith11 = zipWithInduction zipWith10


zipWith12
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k -> Exp l -> Exp m)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
    -> Acc (Array sh l)
    -> Acc (Array sh m)
zipWith12 = zipWithInduction zipWith11


zipWith13
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k -> Exp l -> Exp m -> Exp n)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
    -> Acc (Array sh l)
    -> Acc (Array sh m)
    -> Acc (Array sh n)
zipWith13 = zipWithInduction zipWith12

zipWith14
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k -> Exp l -> Exp m -> Exp n -> Exp o)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
    -> Acc (Array sh l)
    -> Acc (Array sh m)
    -> Acc (Array sh n)
    -> Acc (Array sh o)
zipWith14 = zipWithInduction zipWith13

zipWith15
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o, Elt p)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k -> Exp l -> Exp m -> Exp n -> Exp o -> Exp p)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
    -> Acc (Array sh l)
    -> Acc (Array sh m)
    -> Acc (Array sh n)
    -> Acc (Array sh o)
    -> Acc (Array sh p)
zipWith15 = zipWithInduction zipWith14


zipWith16
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o, Elt p, Elt q)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j -> Exp k -> Exp l -> Exp m -> Exp n -> Exp o -> Exp p -> Exp q)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
    -> Acc (Array sh j)
    -> Acc (Array sh k)
    -> Acc (Array sh l)
    -> Acc (Array sh m)
    -> Acc (Array sh n)
    -> Acc (Array sh o)
    -> Acc (Array sh p)
    -> Acc (Array sh q)
zipWith16 = zipWithInduction zipWith15