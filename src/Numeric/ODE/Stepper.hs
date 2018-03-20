{-# LANGUAGE UnicodeSyntax, RankNTypes, FlexibleContexts,
  OverloadedStrings, TupleSections, BangPatterns #-}
module Numeric.ODE.Stepper (
  Stepper(..)
  ) where

import Data.Array.Repa (Array(..), D, Source, DIM1)
import Data.Text (Text)

-- |This class represents a stepper that moves the solution of an ODE
-- one step further. The function `step` can be used for systems of
-- ODE and the function `step1` for simple ODE.
class Stepper s where
  name ∷ s → Text
  
  step ∷ ∀ r1 r2 r3 r4 . (Source r4 Double, Source r3 Double, Source r2 Double, Source r1 Double) =>
         s -- ^ The stepper to be used
       → Array r1 DIM1 Double -- ^ starting point \(y ∈ ℝ^n\)
       → Array r2 DIM1 Double -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ^n\) at starting point \((x, y)\)
       → Double -- ^ starting point \(x ∈ ℝ\)
       → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
       → Double -- ^ required accuracy ε > 0
       → Array r4 DIM1 Double -- ^ scaling vector against which the error is scaled
       → (∀ r . (Source r Double) ⇒ Double -> Array r DIM1 Double -> Array r3 DIM1 Double) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ^n\)
       → (Double,
          Array D DIM1 Double,
          Double,
          Double,
          Array D DIM1 Double,
          Double) -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R^n, actually used step size h > 0, proposed next step size hnext > 0, and errmax > 0

  step1 ∷ s
        → Double -- ^ starting point \(y ∈ ℝ\)
        → Double -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ\) at starting point \((x, y)\)
        → Double -- ^ starting point \(x ∈ ℝ\)
        → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
        → Double -- ^ required accuracy ε > 0
        → Double -- ^ scaling factor against which the error is scaled
        → (Double → Double → Double) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ\)
        → (Double,
           Double,
           Double,
           Double,
           Double,
           Double) -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R, actually used step size h > 0, proposed next step size hnext > 0, and errmax > 0
