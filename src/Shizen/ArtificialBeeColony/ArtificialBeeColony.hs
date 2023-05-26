{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Shizen.ArtificialBeeColony.ArtificialBeeColony where

import Data.Array.Accelerate as A
import qualified Prelude as P

import Shizen.Types
import Shizen.Utils

-- pso ::
--   forall p b.
--   Position p b =>
--   -- | Archive size
--   Int ->
--   -- | Boundaries
--   b ->
--   -- | Objective function
--   (Exp p -> Exp R) ->
--   -- | InertiaWeight
--   R ->
--   -- | CognitiveParam
--   R ->
--   -- | SocialParam
--   R ->
--   -- | Iterations
--   Int ->
--   P.IO (Acc (Vector (Point p)))
-- pso n b f w p g maxit =
--   do
--     -- We start by creating the random generator.
--     gen <- createGenerator n
--
--     -- Now, we embed some variables into the Exp type of scalar expressions.
--     let -- Parameter representing number of ants
--         n' = constant n :: Exp Int
--         -- Parameter representing search space.
--         b' = constant b
--         -- PSO Parameters
--         w' = constant w :: Exp R
--         p' = constant p :: Exp R
--         g' = constant g :: Exp R
--         -- Parameter representing the iterations of the algorithm loop.
--         maxite = constant maxit :: Exp Int
--
--         -- We generate the particles positions randomly and within the boundaries
--         (positions, gen') = runRandom gen (randomPositions b')
--
--         -- We compute the function value of each position
--         objective = map f positions :: Acc (Vector R)
--
--         points = zip positions objective
--
--         bestPoint = minimumPoint points
--         best = replicate (I1 n') bestPoint :: Acc (Vector (Point p))
--
--         -- -- Create the boundaries for the velocities
--         -- velb = toBoundaries $ boundariesDiameters b'
--
--         -- We generate the particles velocities randomly and within the boundaries
--         (velocities, gen'') = runRandom gen' (randomPositions b')
--
--         particles = zip4 points points best velocities :: Acc (Vector (Particle p))
--
--         step :: Acc (Vector (Particle p)) -> Acc Gen -> Acc (Vector (Particle p), Gen)
--         step = updateParticles n' b' f w' p' g'
--
--         -- Algorithm loop
--         T3 loop _ _ =
--           A.awhile
--             (\(T3 _ it _) -> map (< maxite) it)
--             ( \(T3 old it gen1) ->
--                 let T2 new gen2 = step old gen1
--                     it' = map (+ 1) it
--                  in T3 new it' gen2
--             )
--             (T3 particles (unit 0) gen'')
--         output = map getBestGlobal (take 1 loop) :: Acc (Vector (Point p))
--      in -- Return the best ant
--         P.return output
