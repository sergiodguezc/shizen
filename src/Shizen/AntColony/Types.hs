{-# LANGUAGE FunctionalDependencies #-}
-- |
-- Module    : Shizen.AntColony.Types
-- Copyright : (c) Sergio Domínguez
-- License   : BSD3
--
-- Maintainer: Sergio Domínguez <sergdoma@ucm.es>
-- Stability : provisional
-- Portability:
-- |
-- |
--
module Shizen.AntColony.Types
  ( module Shizen.AntColony.Types,
    -- Tipos básicos
    module Shizen.Types
  )
where

import Data.Array.Accelerate as A
import Control.Monad.State
import Data.Array.Accelerate.System.Random.SFC as SFC
import Shizen.Types

--------------------------------------------------------------------
--  Basic types to be used
--------------------------------------------------------------------

-- | Type used to represent the ant.
type Ant p = (p, R)

type VectorAnt p = Vector (Ant p)

-- | Ant getters
getPosition :: Elt p => Exp (Ant p) -> Exp p
getPosition = A.fst

getObjective :: Elt p => Exp (Ant p) -> Exp R
getObjective = A.snd

class Position p b => AntPosition p b | p -> b, b -> p where
    updatePosition :: Exp R -> Exp b -> Exp p -> Exp p -> Exp SFC64 -> Exp (p, SFC64)
