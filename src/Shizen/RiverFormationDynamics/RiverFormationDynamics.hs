{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Shizen.RiverFormationDynamics.RiverFormationDynamics
  ( module Shizen.RiverFormationDynamics.RiverFormationDynamics,
  )
where

import Data.Array.Accelerate as A
import qualified Prelude as P
import Data.Function
import Data.Array.Accelerate.Data.Sort.Quick (sortBy)
import Data.Array.Accelerate.System.Random.SFC
import Shizen.RiverFormationDynamics.Types
import Shizen.Utils

snd3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp b
snd3 (T3 _ x _) = x

-- | This function assigns each Drop a probability depending on its fitness.
assignProbability ::
  forall p b.
  DropPosition p b =>
  -- | Archive size
  Exp Int ->
  -- | Value between 0 and 1 that limits the movement of the drop (learning rate)
  Exp R ->
  -- | Drops
  Acc (VectorDrop p) ->
  Acc (Vector R)
assignProbability n moveLimit drops =
  let weights = map (\(T2 i _) -> gaussian n moveLimit (unindex1 i)) (indexed drops) :: Acc (Vector R)
      weightsS = the $ sum weights
   in map (/ weightsS) weights :: Acc (Vector R)
  where
    gaussian :: Exp Int -> Exp R -> Exp Int -> Exp R
    gaussian archiveSize moveLimit' rank =
      let rank' = fromIntegral rank :: Exp R
          n' = fromIntegral archiveSize :: Exp R
       in -- Harcoded: 0.005 is the standard deviation of the gaussian
          exp (negate ((rank' ** 2) / (2 * (moveLimit' ** 2) * (n' ** 2)))) / (n' * 0.005 * sqrt (2 * pi))


-- | Function which performs the River Formation Dynamics algorithm for 
-- continuous functions.
rfd ::
  forall p b.
  DropPosition p b =>
  -- | Number of drops
  Int ->
  -- | Number of steps a drop can be moved without improve.
  Int ->
  -- | Bounds
  b ->
  -- | Function to optimize
  (Exp p -> Exp R) ->
  -- | Value between 0 and 1 that limits the movement of the drop (learning rate)
  R ->
  -- | Range factor
  R ->
  -- | Number of iterations
  Int ->
  P.IO (Acc (VectorDrop p))
rfd n maxSteps b f moveLimit rangeFactor iterations =
  do
    -- We start by creating the random generator.
    gen <- createGenerator n

    -- Now, we embed some variables into the Exp type of scalar expressions.
    let n' = constant n :: Exp Int
        maxSteps' = constant maxSteps :: Exp Int
        b' = constant b
        moveLimit' = constant moveLimit :: Exp R
        rangeFactor' = constant rangeFactor :: Exp R
        iterations' = constant iterations :: Exp Int

        -- We generate the ants positions randomly and within the boundaries
        (positions, gen1) = runRandom gen (randomPositions b')

        objective = map f positions :: Acc (Vector R)

        drops = sortBy (compare `on` snd3) $ zip3 positions objective (fill (I1 n') maxSteps')

        -- Main loop
        T4 loop _ _ _ =
            A.awhile
              (\(T4 _ _ it _) -> A.map (A.< iterations') it)
              ( \(T4 old rf it g) ->
                  let rf' = the rf
                      ml = moveLimit' A.* rf'
                      (newDrops, newGen, newRf) = moveDrops b' ml rf' maxSteps' g f old
                      it' = A.map (+ 1) it
                   in T4 newDrops newRf it' newGen
              )
              (T4 drops (unit rangeFactor') (unit 0) gen1)
     in -- Return the best ant
        P.return $ A.take 1 loop

pickDrops ::
  forall p b.
  DropPosition p b =>
  -- | Random numbers
  Acc (Vector R) ->
  -- | Drops distribution
  Acc (Vector R) ->
  -- | Drops
  Acc (VectorDrop p) ->
  Acc (VectorDrop p)
pickDrops randoms distribution drops =
  let c = A.length randoms
      randomsMatrix = A.replicate (lift (Z :. c :. All)) randoms
      -- Replicate the distribution vector 'c' times (transposed)
      distributionMatrix = A.replicate (lift (Z :. All :. c)) distribution

      matrixRD = indexed $ A.zipWith (A.<) randomsMatrix distributionMatrix
      pickedIndex = A.fold1 (\(T2 i b1) (T2 j b2) -> b1 ? (T2 i b1, T2 j b2)) matrixRD
   in A.map (\(T2 (I2 _ r) _) -> drops A.!! r) pickedIndex

moveDrops ::
  forall p b.
  DropPosition p b =>
  -- |  Bounds
  Exp b ->
  -- | Percentage that limits the movement
  Exp R ->
  -- | Range factor
  Exp R ->
  -- | Max number of steps
  Exp Int ->
  -- | Random generator
  Acc Gen ->
  -- | Fitness function
  (Exp p -> Exp R) ->
  -- | Old Drops
  Acc (VectorDrop p) ->
  (Acc (VectorDrop p), Acc Gen, Acc (Scalar R))
moveDrops b moveLimit rangeFactor maxSteps gen f drops =
  -- First, for each drop, we filter the ones that have not improved in the last (maxSteps)
  let bestDrop = drops A.!! 0
      (createdDrops, gen2) = createNewDrops b moveLimit rangeFactor maxSteps gen f drops
      (improvedDrops, gen3) = moveImprovedDrops b maxSteps moveLimit gen2 f drops

      newDrops = sortBy (compare `on` snd3) $ createdDrops A.++ improvedDrops
      -- newDrops = sortBy (compareDrops pt) $ flatten (unit newBest) A.++ createdDrops A.++ improvedDrops

      -- Now, we update the range factor
        

      newBestDrop = newDrops A.!! 0
      newRangeFactor = getObjective bestDrop A.> getObjective newBestDrop ? (A.min (rangeFactor * 2) 1, rangeFactor / 2)
   in (newDrops, gen3, unit newRangeFactor)

moveImprovedDrops ::
  forall p b.
  DropPosition p b =>
  Exp b ->
  Exp Int ->
  Exp R ->
  Acc Gen ->
  (Exp p -> Exp R) ->
  Acc (VectorDrop p) ->
  (Acc (VectorDrop p), Acc Gen)
moveImprovedDrops b maxSteps moveLimit gen f old =
  -- let filterImprove = A.filter (\(T2 _ d) -> getNumSteps d A.> 0) $ A.tail (indexed old)
  let filterImprove = A.filter (\(T2 _ d) -> getNumSteps d A.> 0) (indexed old)
      improveWithIndex = A.afst filterImprove
      improve = A.map (\(T2 _ d) -> d) improveWithIndex :: Acc (VectorDrop p)
      c = the $ A.asnd filterImprove :: Exp Int

      condition =
        c A.> 0
          ?| ( let -- Now, we compute the slopes
                   n = A.length old
                   posOld = A.map getPosition old :: Acc (Vector p)
                   objOld = A.map getObjective old :: Acc (Vector R)

                   -- Positions matrix: n x n
                   positionsMatrix = A.replicate (lift (Z :. All :. n)) posOld :: Acc (Matrix p)
                   -- Objective matrix: n x n
                   objectiveMatrix = A.replicate (lift (Z :. All :. n)) objOld :: Acc (Matrix R)

                   -- Distance matrix: n x n
                   distancesMatrix = A.zipWith (distance 6) positionsMatrix (A.transpose positionsMatrix) :: Acc (Matrix R)
                   -- Delta matrix: n x n
                   deltaMatrix = A.zipWith (A.-) objectiveMatrix (A.transpose objectiveMatrix) :: Acc (Matrix R)
                   -- Slopes matrix: n x n
                   slopes = A.imap (\(I2 i j) (T2 delta dist) -> (i A.== j) ? (0, computeSlopes delta dist)) (A.zip deltaMatrix distancesMatrix) :: Acc (Matrix R)

                   -- Convert slopes to probabilities
                   -- Row i on 'slopesSum' contains the sum of the row i of
                   -- the 'slopes' matrix
                   -- slopesSum : n x n
                   slopesSum = A.replicate (lift (Z :. All :. n)) $ A.sum slopes :: Acc (Matrix R)
                   -- Each row of 'slopesCumulative' contains the cumulative
                   -- values of the row i of 'slopes'.
                   -- slopesCumulative : n x n
                   slopesCumulative = A.scanl1 (+) slopes :: Acc (Matrix R)
                   -- slopesDistribution : n x n
                   slopesDistribution = A.zipWith (A./) slopesCumulative slopesSum :: Acc (Matrix R)

                   -- Now, we select the slope for each ant
                   (randoms, gen2) = runRandom gen randomVector :: (Acc (Vector R), Acc Gen)
                   -- randomsMatrix : n x n
                   randomsMatrix = A.replicate (lift (Z :. All :. n)) randoms :: Acc (Matrix R)

                   -- matrixRD : n x n
                   matrixRD = indexed $ A.zipWith (A.<) randomsMatrix slopesDistribution :: Acc (Matrix (DIM2, Bool))

                   -- selectedSlopesIndex : n
                   -- Reduce the innermost dimension of the matrix, i.e. one
                   -- fold for each row.
                   selectedSlopesIndex = A.fold1 (\(T2 i b1) slope' -> b1 ? (T2 i b1, slope')) matrixRD :: Acc (Vector (DIM2, Bool))
                   -- selectedSlopesIndex = A.fold1 (\(T2 (I2 i1 j1) b1) slope' -> b1 A.&& (i1 A./= j1) ? (T2 (I2 i1 j1) b1, slope')) matrixRD :: Acc (Vector (DIM2, Bool))

                   -- Now, we compute the movement direction
                   -- step : n
                   step = A.map (\(T2 (I2 i j) _) ->
                     let limit = pmap (* moveLimit) (boundariesDiameters b)
                         d = difference (posOld A.!! j) (posOld A.!! i) -- Direction
                         s = slopes A.! I2 i j --  Slope
                     in fixBounds (toBoundaries limit) (pmap (*s) d)
                     ) selectedSlopesIndex :: Acc (Vector p)

                   -- Select the 
                   improveIndex = A.map (\(T2 (I1 i) _) -> i) improveWithIndex :: Acc (Vector Int)
                   posImprove = A.gather improveIndex posOld
                   stepImprove = A.gather improveIndex step

                   -- NewPositions = oldPosition + step
                   newPositions =  A.zipWith (\p s -> fixBounds b (add p s)) posImprove stepImprove
                   newObjectives = A.map f newPositions
                   newDrops = A.zip3 newPositions newObjectives (A.fill (lift (Z :. c)) maxSteps) :: Acc (VectorDrop p)

                   improvedDrops = A.zipWith (\oldd newd -> (compare `on` snd3) oldd newd == GT_ ? (newd, decreaseNumSteps oldd)) improve newDrops
                in lift (improvedDrops, gen2),
               lift (improve, gen) -- Improve is an empty array
             ) ::
          Acc (VectorDrop p, Gen)
   in unlift condition

-- | This function creates new drops for each drop that has not improved 
-- in the last (maxSteps) narrowing the search space depending on the
-- rangeFactor variable. Each drop selects another drop as a reference and
-- it creates a new drop in an open interval around the reference.
createNewDrops ::
  forall p b.
  DropPosition p b =>
  Exp b ->
  Exp R ->
  Exp R ->
  Exp Int ->
  Acc Gen ->
  (Exp p -> Exp R) ->
  Acc (VectorDrop p) ->
  (Acc (VectorDrop p), Acc Gen)
createNewDrops b moveLimit rangeFactor maxSteps gen f old =
  -- let filterNoImprove = A.filter (\d -> getNumSteps d A.== 0) $ A.tail old
  let filterNoImprove = A.filter (\d -> getNumSteps d A.== 0) old
      noImprove = A.afst filterNoImprove
      c = the $ A.asnd filterNoImprove

      condition =
        c A.> 0
          ?| ( let distribution = A.scanl1 (+) (assignProbability (A.length old) moveLimit old) :: Acc (Vector R)
                   (randoms, gen1) = runRandom gen randomVector :: (Acc (Vector R), Acc Gen)
                   referenceDrops = A.take c $ pickDrops randoms distribution old
                   newBoundaries = A.map (\rd -> narrowBoundaries rangeFactor (getPosition rd) b) referenceDrops
                   -- Compute the reference drops for the newDrops
                   newDropsGen = A.zipWith (\newB g -> createRandomDrop newB b g maxSteps f) newBoundaries gen1
                   newDrops = A.map (\(T2 d _) -> d) newDropsGen :: Acc (VectorDrop p)
                   gen2 = A.map (\(T2 _ g) -> g) newDropsGen A.++ A.drop c gen1 :: Acc Gen
                in lift (newDrops, gen2),
               lift (noImprove, gen)
             )
   in unlift condition

computeSlopes :: Exp R -> Exp R -> Exp R
computeSlopes delta dist = delta A.> 0 ? (delta / dist, delta A.< 0 ? (0.1 A./ A.abs (delta A./ dist) + 1, epsilon))
  where
    epsilon = 10 A.** (-12) :: Exp R

createRandomDrop ::
  forall p b.
  DropPosition p b =>
  Exp b -> -- drop range
  Exp b -> -- general boundaries
  Exp SFC64 -> -- random generator
  Exp Int -> -- max steps
  (Exp p -> Exp R) -> -- objective function
  Exp (Drop p, SFC64)
createRandomDrop newB b g ms f =
  let (T2 newPos g1) = randomPosition newB g
      fixedPos = fixBounds b newPos
      newDrop = T3 fixedPos (f fixedPos) ms
   in T2 newDrop g1
