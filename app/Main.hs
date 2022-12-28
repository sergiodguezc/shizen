----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergio Domínguez 2022
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sergdoma@ucm.es
-- Stability   :  unstable
-- Portability :  not portable, uses cuda
--
-- shizen, bio-based algorithms library
--
-----------------------------------------------------------------------------

module Main (main) where

import Shizen.AntColony.AntColony as ACO

main :: IO ()
main = ACO.prueba
