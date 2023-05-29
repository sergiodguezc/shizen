{-# LANGUAGE ScopedTypeVariables #-}

module Shizen.ParticleSwarm.Types
  ( module Shizen.ParticleSwarm.Types,
    module Shizen.Types
  )
where

import Data.Array.Accelerate as A
import Shizen.Types
import qualified Prelude as P

type Particle p = (Point p, Point p, Point p, p)

getCurrentPoint :: forall p b. Position p b => Exp (Particle p) -> Exp (Point p)
getCurrentPoint (T4 p _ _ _) = p

getBestLocal :: forall p b. Position p b => Exp (Particle p) -> Exp (Point p)
getBestLocal (T4 _ p _ _) = p

getBestGlobal :: forall p b. Position p b => Exp (Particle p) -> Exp (Point p)
getBestGlobal (T4 _ _ p _) = p

getVelocity :: forall p b. Position p b => Exp (Particle p) -> Exp p
getVelocity (T4 _ _ _ p) = p
