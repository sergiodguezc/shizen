{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -Wall #-}

module Shizen.RiverFormationDynamics.RiverFormationDynamics
  ( module Shizen.RiverFormationDynamics.RiverFormationDynamics,
  )
where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Sort.Quick (sortBy)
import Data.Array.Accelerate.System.Random.SFC
import Shizen.RiverFormationDynamics.Types
import Shizen.RiverFormationDynamics.Utils

-- rfd ::
--   forall p b.
--   DropPosition p b =>
--   -- | Number of drops
--   Int ->
--   -- | Number of steps a drop can be moved without improve.
--   Int ->
--   ProblemType ->
--   -- | Bounds
--   b ->
--   -- | Function to optimize
--   (Exp p -> Exp Objective) ->
--   -- | Percentage that limits the movement
--   R ->
--   -- | Range factor
--   R ->
--   -- | Number of iterations
--   Int ->
--   IO (Acc (VectorDrop p))
-- rfd n maxSteps pt b f percentage rangeFactor iterations =
--   do
--     -- We start by creating the random generator.
--     gen <- createGenerator n

--     -- Now, we embed some variables into the Exp type of scalar expressions.
--     let n' = constant n :: Exp Int
--         maxSteps' = constant maxSteps :: Exp Int
--         pt' = constant pt :: Exp ProblemType
--         b' = constant b
--         percentage' = constant percentage :: Exp R
--         rangeFactor' = constant rangeFactor :: Exp R
--         iterations' = constant iterations :: Exp Int

--         -- We generate the ants positions randomly and within the boundaries
--         (positions, gen') = runRandom gen (randomPosition b')

--         objective = A.map f positions :: Acc (Vector Objective)

--         drops = sortBy (compareDrops pt') $ A.zip3 positions objective (A.fill (I1 n') maxSteps')

--         container = newContainer 0 gen' :: Container

--         -- Main loop
--         loop =
--           A.afst $
--             A.awhile
--               (\(T2 _ container') -> A.map (A.< iterations') (unit (getIt container')))
--               ( \(T2 old container') ->
--                   let genPos1 = getGen container'
--                       (drops', genPos2) = moveDrops b' percentage' rangeFactor' maxSteps' genPos1 f old
--                    in T2 drops' (updateContainer container' 1 genPos2)
--               )
--               (T2 drops container)
--     return drops

-- | Three different strategies are used to move each drop.
-- 1) if the drop has not improved in the last (maxSteps), then the drop is
-- randomly created in the search space depending on the rangeFactor variable,
-- which narrows the search space. Each dimension i is reduced as follows:
--  range(i) = (max(i) - min(i)) * rangeFactor
-- To create a new position of the drop, first we chose a drop (cd) in a random
-- manner depending on its fitness. Those drops with better fitness will be 
-- selected more often. Then, we create a new position of the drop as follows:
--      1) For each dimension i, we chose a a random value in the interval:
--          [position(cd, i) - range(i)/2, position(cd, i) + range(i)/2]
--         where position(cd, i) is the projection at the coordinate i of the
--         position of the chosen drop (cd).
--         without exceeding the limits of the dimensions.
-- moveDrops ::
--   forall p b.
--   DropPosition p b =>
--   Exp b ->
--   Exp R ->
--   Exp R ->
--   Exp Int ->
--   Acc Gen ->
--   (Exp p -> Exp Objective) ->
--   Acc (Vector (Drop p)) ->
--   (Acc (VectorDrop p), Acc  Gen)
-- moveDrops b percentage rangeFactor maxSteps gen f drops =
--   -- First, for each drop, we filter the ones that have not improved in the last (maxSteps)
--   let noImprove = A.afst $ A.filter (\(T3 _ _ steps) -> steps A.== 0) drops
--       -- Then, we create a new position for each drop that has not improved in the last (maxSteps)
--       -- narrowing the search space depending on the rangeFactor variable.