{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module      : Data.Array.Accelerate.System.Random.SFC
-- Copyright   : [2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Small Fast Carry RNG from the PractRand library <http://pracrand.sourceforge.net>
--

module Data.Array.Accelerate.System.Random.SFC (
  module Data.Array.Accelerate.System.Random.SFC
  -- Random, RandomT(..),
  -- runRandom, evalRandom, evalRandomT,
  -- Gen,
  --
  -- seed,
  -- create,
  -- createWith,
  -- randomVector,
  -- randomRVector,
  -- randomNVector,
  --
  -- UniformR(..),
  -- Normal(..),
  -- Uniform(..),
  -- SFC64,

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Data.Either                            as A
import Data.Array.Accelerate.Data.Bits                              as A

import Control.Monad.Identity
import Control.Monad.State
import Language.Haskell.TH                                          hiding ( Exp )
import Prelude                                                      as P


type Random = RandomT Identity

newtype RandomT m t a = RandomT { runRandomT :: StateT t m a }
  deriving newtype (Functor, Applicative, Monad)

-- | Unwrap a random monad computation as a function, returning both the
-- generated value and the new generator state.
--
runRandom :: t -> Random t a -> (a, t)
runRandom gen r = runIdentity $ runStateT (runRandomT r) gen

-- | Evaluate a computation given the initial generator state and return
-- the final value, discarding the final state.
--
evalRandom :: t -> Random t a -> a
evalRandom gen = runIdentity . evalRandomT gen

-- | Evaluate a computation with the given initial generator state and
-- return the final value, discarding the final state.
--
evalRandomT :: (Monad m) => t -> RandomT m t a -> m a
evalRandomT gen r = evalStateT (runRandomT r) gen

class RNG t where
  type Output t a

  -- | Generate random values. When generating an array the size of the array is
  -- determined by the generator state that was built using 'create' or
  -- 'createWith'.
  --
  random :: (Uniform a, Monad m) => RandomT m t (Output t a)

instance Shape sh => RNG (Acc (Array sh SFC64)) where
  type Output (Acc (Array sh SFC64)) a = Acc (Array sh a)

  random = RandomT $ state (A.unzip . A.map uniform)

instance RNG (Exp SFC64) where
  type Output (Exp SFC64) a = Exp a

  random = RandomT $ state (unlift . uniform)


type Gen = Vector SFC64


data SFC a where
  SFC64_ :: a -> a -> a -> a -> SFC a
  deriving (Generic, Elt)

pattern SFC :: Elt a => Exp a -> Exp a -> Exp a -> Exp a -> Exp (SFC a)
pattern SFC a b c counter = Pattern (a, b, c, counter)
{-# COMPLETE SFC #-}

type SFC64 = SFC Word64

-- | The Small Fast Carry RNG (64-bit)
--
-- Stolen from PractRand v0.95.
--
sfc64 :: Exp SFC64 -> Exp (Word64, SFC64)
sfc64 (SFC a b c counter) =
  let tmp      = a + b + counter
      counter' = counter + 1
      a'       = b `xor` (b `unsafeShiftR` 11)
      b'       = c +     (c `unsafeShiftL` 3)
      c'       = ((c `unsafeShiftL` 24) .|. (c `unsafeShiftR` (64-24))) + tmp
   in
   T2 tmp (SFC a' b' c' counter')

-- | Create a new generator state using default seeds (the array index).
--
-- You'll probably get better random numbers by using 'createWith' and
-- seeding the initial state from a better source of entropy. For example,
-- we can use the 'mwc-random-accelerate' package to generate the seed
-- vector using the system's source of random numbers:
--
-- > gen <- createWith . use <$> MWC.randomArray MWC.uniform (Z :. 100)
--
create :: Shape sh => Exp sh -> Acc Gen
create sh =
  let n   = shapeSize sh
      gen = generate (I1 n) (\(I1 i) -> seedFast (A.fromIntegral i))
  in
  gen

seedFast :: Exp Word64 -- ^ 
  -> Exp SFC64
seedFast s
  = A.snd
  $ while (\(T2 i _) -> i A.< 8)
          (\(T2 i g) -> let T2 _ g' = sfc64 g in T2 (i+1) g')
          (T2 (0 :: Exp Int) (SFC s s s 1))


-- | Create a new generator state using the given seed vector
--
createWith :: Acc (Vector (Word64, Word64, Word64)) -> Acc Gen
createWith = A.map (\(T3 a b c) -> seed a b c)

seed :: Exp Word64 -> Exp Word64 -> Exp Word64 -> Exp SFC64
seed a b c
  = A.snd
  $ while (\(T2 i _) -> i A.< 18)
          (\(T2 i g) -> let T2 _ g' = sfc64 g in T2 (i+1) g')
          (T2 (0 :: Exp Int) (SFC a b c 1))


-- | Generate a vector of random values. The size of the vector is
-- determined by the generator state that was built using 'create' or
-- 'createWith'.
--
randomVector :: (Uniform a, Monad m) => RandomT m (Acc Gen) (Acc (Vector a))
randomVector = RandomT . StateT $ \s ->
  let (r, s') = A.unzip $ A.map uniform s
   in return (r, s')


first :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b) -> Exp (a, c) -> Exp (b, c)
first f (T2 a b) = T2 (f a) b

class Uniform a => Normal a where
    normal :: Exp a -> Exp a -> Exp SFC64 -> Exp (a, SFC64)

instance Normal Double where
    normal m sd s = 
      -- let (x, g1) = unlift $ uniform s :: (Exp Double, Exp SFC64)
      let (u1, g1) = unlift $ uniform s :: (Exp Double, Exp SFC64)
          (u2, g2) = unlift $ uniform g1 :: (Exp Double, Exp SFC64)

          -- u1 = A.max (2 A.^(-63 :: Exp Int)) x 
          -- Box-Muller transformation: z = N(0,1)
          z = A.sqrt (-2 * A.log u1) * A.cos (2 * A.pi * u2)
          -- z*sd + m = N(m,sd)
        in A.lift (z * sd + m, g2)

instance Normal Float where
    normal m sd s = 
      let (x, g1) = unlift $ uniform s :: (Exp Float, Exp SFC64)
          (u2, g2) = unlift $ uniform g1 :: (Exp Float, Exp SFC64)
          u1 = A.max (2 A.^(-31 :: Exp Int)) x 
          -- Box-Muller transformation: z = N(0,1)
          z = A.sqrt (-2 * A.log u1) * A.cos (2 * A.pi * u2)
          -- z*sd + m = N(m,sd)
        in A.lift (z * sd + m, g2)

class Elt a => UniformR a where
  uniformRange :: Exp (a, a) -> Exp SFC64 -> Exp (a, SFC64)

instance UniformR Float where
  uniformRange p s =
    let (l, h) = unlift p :: (Exp Float, Exp Float)
        uni = uniform s :: Exp (Float, SFC64)
        x = A.fst uni :: Exp Float
    in A.lift (x * l + (1 - x) * h, A.snd uni)

instance UniformR Double where
  uniformRange p s =
    let (l, h) = unlift p :: (Exp Double, Exp Double)
        uni = uniform s :: Exp (Double, SFC64)
        x = A.fst uni :: Exp Double
    in A.lift (x * l + (1 - x) * h, A.snd uni)

randomRVector :: (UniformR a, Monad m) => Exp (a, a) -> RandomT m (Acc Gen) (Acc (Vector a))
randomRVector b = RandomT . StateT $ \s ->
  let (r, s') = A.unzip $ A.map (uniformRange b) s
   in return (r, s')

randomNVector :: (Normal a, Monad m) => Exp a -> Exp a -> RandomT m (Acc Gen) (Acc (Vector a))
randomNVector m sd = RandomT . StateT $ \s ->
  let (r, s') = A.unzip $ A.map (normal m sd) s
   in return (r, s')


-- | The class of types for which we can generate random variates. Integral
-- variates are generated in the full range, floating point variates are in
-- the range [0,1].
--
class Elt a => Uniform a where
  uniform :: Exp SFC64 -> Exp (a, SFC64)

instance Uniform Bool   where uniform = first A.even . sfc64
instance Uniform Int    where uniform = first A.fromIntegral . sfc64
instance Uniform Int8   where uniform = first A.fromIntegral . sfc64
instance Uniform Int16  where uniform = first A.fromIntegral . sfc64
instance Uniform Int32  where uniform = first A.fromIntegral . sfc64
instance Uniform Int64  where uniform = first A.fromIntegral . sfc64
instance Uniform Word   where uniform = first A.fromIntegral . sfc64
instance Uniform Word8  where uniform = first A.fromIntegral . sfc64
instance Uniform Word16 where uniform = first A.fromIntegral . sfc64
instance Uniform Word32 where uniform = first A.fromIntegral . sfc64
instance Uniform Word64 where uniform = sfc64

instance Uniform Half where
  uniform = first toFloating . uniform @Double

instance Uniform Float where
  uniform s =
    let cvt :: Exp Word64 -> Exp Float
        cvt v = A.fromIntegral v * (1 / A.fromIntegral (maxBound :: Exp Word64))
     in
     first cvt (sfc64 s)

instance Uniform Double where
  uniform s =
    let cvt :: Exp Word64 -> Exp Double
        cvt v = A.fromIntegral v * (1 / A.fromIntegral (maxBound :: Exp Word64))
     in
     first cvt (sfc64 s)

instance Uniform a => Uniform (Complex a) where
  uniform s0 =
    let T2 r s1 = uniform s0
        T2 c s2 = uniform s1
     in T2 (r ::+ c) s2

instance Uniform a => Uniform (Maybe a) where
  uniform s0 =
    let T2 c s1 = uniform s0
     in if c
           then T2 Nothing_ s1
           else first Just_ (uniform s1)

instance (Uniform a, Uniform b) => Uniform (Either a b) where
  uniform s0 =
    let T2 c s1 = uniform s0
     in if c
           then first Left_  (uniform s1)
           else first Right_ (uniform s1)

-- runQ $ do
--   let
--       tupT :: [TypeQ] -> TypeQ
--       tupT [t] = t
--       tupT tup =
--         let n = P.length tup
--          in foldl (\ts t -> [t| $ts $t |]) (tupleT n) tup
--
--
--       mkTup :: Int -> Q [Dec]
--       mkTup n =
--         let
--             xs          = [ mkName ('x':show i) | i <- [0 .. n-1] ]
--             ss          = [ mkName ('s':show i) | i <- [0 .. n]   ]
--             cst         = tupT (P.map (\x -> [t| Uniform $(varT x) |]) xs)
--             res         = tupT (P.map varT xs)
--             step x s s' = valD [p| T2 $(varP x) $(varP s') |] (normalB [| uniform $(varE s) |]) []
--             steps       = P.zipWith3 step xs ss (P.tail ss)
--             r           = [| T2 $(appsE (conE (mkName ('T':show n)) : P.map varE xs)) $(varE (last ss)) |]
--          in
--          [d| instance ($cst) => Uniform $res where
--                uniform $(varP (P.head ss)) =
--                  $(letE steps r)
--            |]
--   --
--   concat <$> mapM mkTup [2..16]

