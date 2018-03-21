{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.ODE.ModifiedMidpoint (mmid, mmid1, MMID(..)) where

import Data.Array.Repa ((+^), Source, delay)
import Data.Array.Repa.Util
import Numeric.ODE.Stepper

mmid ∷ ∀ r1 r2 r3 .
       (Source r3 Double, Source r2 Double, Source r1 Double) =>
       Vec r1 -- ^ starting point \(y ∈ ℝ^n\)
     → Vec r2 -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ^n\) at starting point \((x, y)\)
     → Double -- ^ starting point \(x ∈ ℝ\)
     → Double -- ^ total stepsize \(h_{tot} ∈ ℝ\)
     → Int -- ^ number of steps to do
     → (∀ r . (Source r Double) ⇒ Double -> Vec r -> Vec r3) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ^n\)
     → (Double, VecD, Double) -- ^ advanced \(x_1 ∈ ℝ\), advanced vector \(y_1 ∈ ℝ^n\), actually used step size \(h > 0\)
{-# INLINE mmid #-}
mmid y dydx x htot n f =
  let -- first step
      ym = y
      yn = y +^ h*.dydx
      x' = x+h
      dydx' = f x' yn
      -- n-1 general steps
      (yn'', ym'', dydx'') = loop 1 x' yn (delay ym) dydx'
      -- last step
      y'' = 0.5*.(ym'' +^ yn'' +^ h*.dydx'')
  in (x+htot, y'', htot)
  where
    h = htot / (fromIntegral n)
    h2 = 2.0 * h
    loop ∷ Int -> Double -> VecD -> VecD -> Vec r3 -> (VecD, VecD, Vec r3)
    loop k x yn ym dydx = let yn' = ym +^ h2*.dydx
                              x' = x+h
                              dydx' = f x' yn'
                        in if k<n-1
                           then loop (k+1) x' yn' yn dydx'
                           else (yn', yn, dydx')
    
mmid1 ∷ Double -- ^ starting point \(y ∈ ℝ\)
   → Double -- ^ derivative \(\frac{dy}{dx} = f(x,y) ∈ ℝ\) at starting point \((x, y)\)
   → Double -- ^ starting point \(x ∈ ℝ\)
   → Double -- ^ total stepsize \(h_{tot} ∈ ℝ\)
   → Int -- ^ number of steps to do
   → (Double -> Double -> Double) -- ^ derivative function \(f(x, y) = \frac{dy}{dx} ∈ ℝ\)
   → (Double, Double, Double) -- ^ advanced \(x_1 ∈ ℝ\), advanced value \(y_1 ∈ ℝ\), actually used step size \(h > 0\)
{-# INLINE mmid1 #-}
mmid1 y dydx x htot n f =
  let -- first step
      ym = y
      yn = y + h*dydx
      x' = x+h
      dydx' = f x' yn
      -- n-1 general steps
      (yn'', ym'', dydx'') = loop 1 x' yn ym dydx'
      -- last step
      y'' = 0.5*(ym'' + yn'' + h*dydx'')
  in (x+htot, y'', htot)
  where
    h = htot / (fromIntegral n)
    h2 = 2.0 * h
    loop ∷ Int -> Double -> Double -> Double -> Double -> (Double, Double, Double)
    loop k x yn ym dydx = let yn' = ym + h2*dydx
                              x' = x+h
                              dydx' = f x' yn'
                        in if k<n-1
                           then loop (k+1) x' yn' yn dydx'
                           else (yn', yn, dydx')

data MMID = MMID Int

instance SimpleStepper MMID where
  simpleStepperName _ = "Modified midpoint method"
  simpleStep (MMID n) y dydx x h _ f = mmid y dydx x h n f
  simpleStep1 (MMID n) y dydx x h _ f = mmid1 y dydx x h n f
