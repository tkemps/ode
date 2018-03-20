{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.ODE.Util (maxScaledError, maxScaledError1) where

import Data.Array.Repa (Array(..), Source, Shape)
import qualified Data.Array.Repa as R

maxScaledError :: (Source r2 Double, Source r1 Double, Shape sh) =>
                  Array r1 sh Double -> Array r2 sh Double -> Double -> Double
{-# INLINE maxScaledError #-}
maxScaledError yerr yscal eps =
  R.foldAllS max 0.0 (R.zipWith (\yerr yscal → abs (yerr/yscal) / eps)
                       yerr yscal)

maxScaledError1 :: Double -> Double -> Double -> Double
{-# INLINE maxScaledError1 #-}
maxScaledError1 yerr yscal eps = abs ((yerr/yscal) / eps)
