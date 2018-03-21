{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.ODE.BS where

import Data.Array.Repa (Array(..), D, (+^), Source, DIM1)
import Data.Array.Repa.Util
import Numeric.ODE.Stepper
import Numeric.ODE.Util

kMax ∷ Int
kMax = 8

iMax = kMax+1

safe1, safe2 ∷ Double
safe1 = 0.25
safe2 = 0.7

redMax, redMin ∷ Double
redMax = 1.0e-5
redMin = 0.7

tiny ∷ Double
tiny = 1.0e-30

scalMax ∷ Double
scalMax = 0.1

bs ∷ ∀ r1 r2 r3 r4 s .
  (Source r4 Double, Source r3 Double,
   Source r2 Double, Source r1 Double) =>
  Array r1 DIM1 Double -- ^ starting point \(y ∈ ℝ^n\)
   → Array r2 DIM1 Double -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ^n\) at starting point \((x, y)\)
   → Double -- ^ starting point \(x ∈ ℝ\)
   → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
   → Double -- ^ required accuracy \(ε > 0\)
   → Array r4 DIM1 Double -- ^ scaling vector \(y_{scal} ∈ ℝ^n\) against which the error \(y_{err} ∈ ℝ^n\) is scaled in order to calculate \(errmax\)
   → (∀ r . (Source r Double) ⇒ Double -> Array r DIM1 Double -> Array r3 DIM1 Double) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ^n\)
   → (Double,
      Array D DIM1 Double,
      Double,
      Double,
      Array D DIM1 Double,
      Double) -- ^ advanced \(x_1 ∈ ℝ\), advanced vector \(y_1 ∈ ℝ^n\), actually used step size \(h > 0\), proposed next step size \(h_{next} > 0\), \(y_{err} ∈ ℝ^n\) and \(errmax > 0\)
bs y dydx x htry eps yscal f = undefined

bs1 ∷ Double -- ^ starting point \(y ∈ ℝ\)
   → Double -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ\) at starting point \((x, y)\)
   → Double -- ^ starting point \(x ∈ ℝ\)
   → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
   → Double -- ^ required accuracy \(ε > 0\)
   → Double -- ^ scaling vector \(y_{scal} ∈ ℝ\) against which the error \(y_{err} ∈ ℝ\) is scaled in order to calculate \(errmax\)
   → (Double -> Double -> Double) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ\)
   → (Double,
      Double,
      Double,
      Double,
      Double,
      Double) -- ^ advanced \(x_1 ∈ ℝ\), advanced value \(y_1 ∈ ℝ\), actually used step size \(h > 0\), proposed next step size \(h_{next} > 0\), \(y_{err} ∈ ℝ\) and \(errmax > 0\)
bs1 y dydx x htry eps yscal f = undefined

data BS = BS

instance Stepper BS where
  stepperName _ = "Bulisch-Stoer"
  step _ y dydx x h eps yscal f = bs y dydx x h eps yscal f
  step1 _ y dydx x h eps yscal f = bs1 y dydx x h eps yscal f
