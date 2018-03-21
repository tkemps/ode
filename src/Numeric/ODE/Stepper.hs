{-# LANGUAGE UnicodeSyntax, RankNTypes, FlexibleContexts, FlexibleInstances, UndecidableInstances, OverloadedStrings, TupleSections, BangPatterns #-}
module Numeric.ODE.Stepper (
  Stepper(..), SimpleStepper(..),
  ) where

import Data.Array.Repa (Source)
import Data.Array.Repa.Util
import Data.Text (Text)
import qualified Data.Text as T

-- |This class represents a stepper that moves the solution of an ODE
-- one step further. The function `step` can be used for systems of
-- ODE and the function `step1` for simple ODE.
class Stepper s where
  stepperName ∷ s → Text
  
  step ∷ ∀ r1 r2 r3 r4 . (Source r4 Double, Source r3 Double, Source r2 Double, Source r1 Double) =>
         s -- ^ The stepper to be used
       → Vec r1 -- ^ starting point \(y ∈ ℝ^n\)
       → Vec r2 -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ^n\) at starting point \((x, y)\)
       → Double -- ^ starting point \(x ∈ ℝ\)
       → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
       → Double -- ^ required accuracy ε > 0
       → Vec r4 -- ^ scaling vector against which the error is scaled
       → (∀ r . (Source r Double) ⇒ Double -> Vec r -> Vec r3) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ^n\)
       → (Double,
          VecD,
          Double,
          Double,
          VecD,
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

class SimpleStepper s where
  simpleStepperName ∷ s → Text
  
  simpleStep ∷ ∀ r1 r2 r3 . (Source r3 Double, Source r2 Double, Source r1 Double) =>
         s -- ^ The stepper to be used
       → Vec r1 -- ^ starting point \(y ∈ ℝ^n\)
       → Vec r2 -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ^n\) at starting point \((x, y)\)
       → Double -- ^ starting point \(x ∈ ℝ\)
       → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
       → Double -- ^ required accuracy ε > 0
       → (∀ r . (Source r Double) ⇒ Double -> Vec r -> Vec r3) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ^n\)
       → (Double,
          VecD,
          Double) -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R^n, actually used step size h > 0, proposed next step size hnext > 0

  simpleStep1 ∷ s
        → Double -- ^ starting point \(y ∈ ℝ\)
        → Double -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ\) at starting point \((x, y)\)
        → Double -- ^ starting point \(x ∈ ℝ\)
        → Double -- ^ (proposed) stepsize \(h ∈ ℝ\)
        → Double -- ^ required accuracy ε > 0
        → (Double → Double → Double) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ\)
        → (Double,
           Double,
           Double) -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R, actually used step size h > 0, proposed next step size hnext > 0

instance {-# OVERLAPPABLE #-} Stepper s ⇒ SimpleStepper s where
  simpleStepperName s = T.concat ["Simplified ", stepperName s]
  simpleStep st y dydx x h eps f =
    let (xnext, ynext, hdid, _, _, _) = step st y dydx x h eps y f
    in (xnext, ynext, hdid)
  simpleStep1 st y dydx x h eps f =
    let (xnext, ynext, hdid, _, _, _) = step1 st y dydx x h eps y f
    in (xnext, ynext, hdid)
