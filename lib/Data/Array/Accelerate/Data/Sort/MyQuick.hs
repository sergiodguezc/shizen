{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Sort.Quick
-- Copyright   : [2020] Ivo Gabe de Wolff, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Data.Sort.MyQuick (

  sort,
  sortBy,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe
import Data.Array.Accelerate.Data.Bits


-- | A quick-ish stable sort. This is a special case of 'sortBy' which
-- allows the user to supply their own comparison function.
--
sort :: Ord a => Acc (Vector a) -> Acc (Vector a)
sort = sortBy compare

-- | A non-overloaded version of 'sort'.
--
-- It is often convenient to use this together with 'Data.Function.on', for
-- instance: 'sortBy' ('compare' `on` 'fst')
--
sortBy :: Elt a => (Exp a -> Exp a -> Exp Ordering) -> Acc (Vector a) -> Acc (Vector a)
sortBy cmp input = result
  where
    -- Initially, we have one segment, namely the whole array
    initialFlags = scatter (fill (I1 1) 0 ++ fill (I1 1) (length input)) emptyFlags fullFlags
    emptyFlags   = fill (I1 (1 + length input)) False_
    fullFlags    = fill (I1 2) True_

    -- We stop when each segment contains just one element, as segments of
    -- one element are sorted.
    T2 result _ = awhile condition (step cmp) $ T2 input initialFlags

type State a =
  ( Vector a      -- Values
  , Vector Bool   -- Head flags, denoting the starting points of the unsorted segments
  )

step :: Elt a => (Exp a -> Exp a -> Exp Ordering) -> Acc (State a) -> Acc (State a)
step cmp (T2 values headFlags) = T2 values' headFlags'
  where
    -- Per element, the pivot of the segment of that element
    -- For each segment, we just take the first element as pivot
    pivots = propagateSegmentHead headFlags values

    -- Find which elements are larger than the pivot
    isLarger = zipWith (\v p -> cmp v p /= LT_) values pivots

    -- Propagate the start index of a segment to all elements
    startIndex = propagateSegmentHead headFlags (generate (shape values) unindex1)

    -- Compute the offsets to which the elements must be moved using a scan
    -- The array contains tuples. The first element is the offset among
    -- the larger elements (ie the number of elements before this element,
    -- which also belong to the larger-than segment), and the second element
    -- is the offset among the smaller elements.
    -- Since we use an inclusive scan, the indices are one too large (hence the name PlusOne)
    indicesLargerSmallerPlusOne :: Acc (Vector (Int, Int))
    indicesLargerSmallerPlusOne = postscanSegHead (\(T2 a b) (T2 c d) -> T2 (a + c) (b + d)) headFlags $ map (? (T2 1 0, T2 0 1)) isLarger

    -- Propagate the number of smaller elements to each segment
    -- This is needed as an offset for the larger elements
    countSmaller :: Acc (Vector Int)
    countSmaller = propagateSegmentLast headFlags $ map snd indicesLargerSmallerPlusOne

    -- Compute the new indices of the elements
    permutation = zipWith4 partitionPermuteIndex isLarger startIndex indicesLargerSmallerPlusOne countSmaller

    -- Perform the permutation
    values' = scatter permutation (fill (shape values) undef) values

    -- Update the head flags for the next iteration (the 'recursive call'
    -- in a traditional implementation)
    --
    -- Mark new section starts at:
    --  * the position of the pivot
    --  * the position of the pivot + 1
    headFlags' =
      let
          f :: Int -> Exp Bool -> Exp Int -> Exp Int -> Exp (Maybe DIM1)
          f inc headF start countSmall =
            if headF
               then Just_ (I1 (start + countSmall + constant inc))
               else Nothing_

          writes :: Int -> Acc (Vector (Maybe DIM1))
          writes inc = zipWith3 (f inc) headFlags startIndex countSmaller
      in
      -- Note that (writes 1) may go out of bounds of the values array.
      -- We made the headFlags array one larger to avoid this problem.
      writeFlags (writes 0) $ writeFlags (writes 1) headFlags

-- Checks whether all segments have length 1. If that is the case, then the
-- loop may terminate.
--
condition :: Elt a => Acc (State a) -> Acc (Scalar Bool)
condition (T2 _ headFlags) = any not headFlags

-- Finds the new index of an element of the list, as the result of the
-- partition
--
partitionPermuteIndex :: Exp Bool -> Exp Int -> Exp (Int, Int) -> Exp Int -> Exp Int
partitionPermuteIndex isLarger start (T2 indexIfLarger indexIfSmaller) countSmaller =
  start + (isLarger ? (countSmaller + indexIfLarger - 1, indexIfSmaller - 1))

-- Given head flags, propagates the value of the head to all elements in
-- the segment
--
propagateSegmentHead
    :: Elt a
    => Acc (Vector Bool)
    -> Acc (Vector a)
    -> Acc (Vector a)
propagateSegmentHead headFlags values
  = map fst
  $ scanl1 f
  $ zip values headFlags
  where
    f left (T2 rightValue rightFlag) =
      if rightFlag
         then T2 rightValue True_
         else left

-- Given head flags, propagates the value of the head to all elements in
-- the segment
--
propagateSegmentLast
    :: Elt a
    => Acc (Vector Bool)
    -> Acc (Vector a)
    -> Acc (Vector a)
propagateSegmentLast headFlags values
  = map fst
  $ scanr1 f
  $ zip values
  $ tail headFlags
  where
    f (T2 leftValue leftFlag) right =
      if leftFlag
         then T2 leftValue True_
         else right

-- Segmented postscan, where the segments are defined with head flags
--
postscanSegHead
    :: Elt a
    => (Exp a -> Exp a -> Exp a)
    -> Acc (Vector Bool)
    -> Acc (Vector a)
    -> Acc (Vector a)
postscanSegHead f headFlags values
  = map fst
  $ scanl1 g
  $ zip values headFlags
  where
    g (T2 leftValue leftFlag) (T2 rightValue rightFlag)
      = T2
          (rightFlag ? (rightValue, f leftValue rightValue))
          (leftFlag .|. rightFlag)

-- Writes True to the specified indices in a flags arrays
--
writeFlags
    :: Acc (Vector (Maybe DIM1))
    -> Acc (Vector Bool)
    -> Acc (Vector Bool)
writeFlags writes flags =
  permute const flags (writes !) (fill (shape writes) True_)
