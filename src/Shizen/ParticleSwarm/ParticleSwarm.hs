{-# LANGUAGE ScopedTypeVariables, GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Shizen.ParticleSwarm.ParticleSwarm where

import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.SFC
import Shizen.Types
import Shizen.Utils
import qualified Prelude as P

minimumPoint :: Elt p => Acc (Vector (Point p)) -> Acc (Scalar (Point p))
minimumPoint = fold1 (\(T2 p1 o1) (T2 p2 o2) -> o1 < o2 ? (T2 p1 o1, T2 p2 o2))

head :: Elt a => Acc (Vector a) -> Exp a
head xs = xs ! I1 0

type Particle p = (Point p, Point p, Point p, p)

getCurrentPoint :: forall p b. Position p b => Exp (Particle p) -> Exp (Point p)
getCurrentPoint (T4 p _ _ _) = p

getBestLocal :: forall p b. Position p b => Exp (Particle p) -> Exp (Point p)
getBestLocal (T4 _ p _ _) = p

getBestGlobal :: forall p b. Position p b => Exp (Particle p) -> Exp (Point p)
getBestGlobal (T4 _ _ p _) = p

getVelocity :: forall p b. Position p b => Exp (Particle p) -> Exp p
getVelocity (T4 _ _ _ p) = p

-- | Function that performs the ant colony algorithm
pso ::
  forall p b.
  Position p b =>
  -- | Archive size
  Int ->
  -- | Boundaries
  b ->
  -- | Objective function
  (Exp p -> Exp R) ->
  -- | InertiaWeight
  R ->
  -- | CognitiveParam
  R ->
  -- | SocialParam
  R ->
  -- | Iterations
  Int ->
  P.IO (Acc (Vector (Point p)))
pso n b f w p g maxit =
  do
    -- We start by creating the random generator.
    gen <- createGenerator n

    -- Now, we embed some variables into the Exp type of scalar expressions.
    let -- Parameter representing number of ants
        n' = constant n :: Exp Int
        -- Parameter representing search space.
        b' = constant b
        -- PSO Parameters
        w' = constant w :: Exp R
        p' = constant p :: Exp R
        g' = constant g :: Exp R
        -- Parameter representing the iterations of the algorithm loop.
        maxite = constant maxit :: Exp Int

        -- We generate the particles positions randomly and within the boundaries
        (positions, gen') = runRandom gen (randomPositions b')

        -- We compute the function value of each position
        objective = map f positions :: Acc (Vector R)

        points = zip positions objective

        bestPoint = minimumPoint points
        best = replicate (I1 n') bestPoint :: Acc (Vector (Point p))

        -- Create the boundaries for the velocities
        -- velb = toBoundaries $ boundariesDiameters b'

        -- We generate the particles velocities randomly and within the boundaries
        (velocities, gen'') = runRandom gen' (randomPositions b')

        particles = zip4 points points best velocities :: Acc (Vector (Particle p))

        -- Algorithm loop
        T3 loop _ _ =
          A.awhile
            (\(T3 _ it _) -> map (< maxite) it)
            ( \(T3 old it gen1) ->
                let T2 new gen2 = updateParticles n' b' f w' p' g' old gen1
                    it' = map (+ 1) it
                 in T3 new it' gen2
            )
            (T3 particles (unit 0) gen'')
        output = map getBestGlobal (take 1 loop) :: Acc (Vector (Point p))
     in -- Return the best ant
        P.return output

updateParticles ::
  forall p b.
  Position p b =>
  Exp Int ->
  Exp b ->
  (Exp p -> Exp R) ->
  Exp R ->
  Exp R ->
  Exp R ->
  Acc (Vector (Particle p)) ->
  Acc Gen ->
  Acc (Vector (Particle p), Gen)
updateParticles n b f w p g old gen = output
  where

    updateParticle (T4 (T2 c _) (T2 bl blo) (T2 bg bgo) v) gen1 =
      let T2 rp gen2 = uniform gen1 :: Exp (R, SFC64)
          T2 rg gen3 = uniform gen2 :: Exp (R, SFC64)
          -- v' = w*v + p*rp*(bl-c) + g*rg*(bg-c)
          newVelocity = add (add (pmap (* w) v) (pmap (\xi -> xi * rp * p) (difference bl c))) (pmap (\xi -> xi * rg * g) (difference bg c))

          -- Compute new position
          newPos = fixBounds b $ add c newVelocity
          newO = f newPos

          -- Update local best
          newBl' = newO < blo ? (T2 newPos newO, T2 bl blo)
       in T2 (T4 (T2 newPos newO) newBl' (T2 bg bgo) newVelocity) gen3

    newParticlesGen = zipWith updateParticle old gen

    newParticles = map fst newParticlesGen
    newGen = map snd newParticlesGen

    -- Now we update the global best
    newBl = map getBestLocal newParticles
    bestPoint = minimumPoint newBl
    newBg = replicate (I1 n) bestPoint :: Acc (Vector (Point p))

    finalParticles = zipWith (\(T4 c bl _ v) bg' -> T4 c bl bg' v) newParticles newBg

    output = T2 finalParticles newGen
