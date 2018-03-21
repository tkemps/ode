{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.ODE.ODEInt (
  adaptiveStepSizeControl, adaptiveStepSizeControl1,
  dumb, dumb1
  ) where

import Control.Exception
import Data.Array.Repa (Array(..), Z(..), (:.)(..), (+^), Source, DIM1,
                        delay, extent, fromListUnboxed)
import qualified Data.Array.Repa as R
import Numeric.ODE.Exceptions
import Numeric.ODE.Stepper
import Data.Array.Repa.Util

adaptiveStepSizeControl ::
  ∀ r1 r2 s . (Source r1 Double, Source r2 Double, Stepper s)
  ⇒ Vec r1 -- ^ starting point y ∈ R^n
  → (Double, Double) -- ^ starting and end point x0, x1 ∈ R, x1 > x0
  → (∀ r . (Source r Double) ⇒ Double → Vec r → Vec r2)
  → Double
  → Double
  → Double
  → s
  → [(Double, VecD, Double, Double)]
adaptiveStepSizeControl y0 (x0, x1) f htry hmin eps st =
  assert (x1 > x0 && htry>0 && hmin>0 && htry>hmin && eps>0) $
  (x0, delay y0, 0, 0):loop x0 (delay y0) htry
  where
    shY0@(Z:.n) = extent y0
    tiny = fromListUnboxed shY0 (replicate n 1.0e-30)
    loop ∷ Double
         → VecD
         → Double
         → [(Double, VecD, Double, Double)]
    loop x y htry =
      let dydx = f x y
          yscal = (R.map abs y) +^ h*.(R.map abs dydx) +^ tiny
          h = if x+htry > x1 then x1-x else htry
          (xnext, ynext, hdid, hnext, _, errmax) = step st y dydx x h eps yscal f
      in assert (extent dydx == shY0) $
         if (xnext >= x1)
         then [(xnext, ynext, hdid, errmax)]
         else if hnext >= hmin
              then (xnext, ynext, hdid, errmax):loop xnext ynext hnext
              else throw (StepSizeUnderflow xnext hnext)

adaptiveStepSizeControl1 :: (Stepper s)
        ⇒ Double -- ^ starting point y ∈ R^n
        → (Double, Double) -- ^ starting and end point x0, x1 ∈ R, x1 > x0
        → (Double → Double → Double)
        → Double
        → Double
        → Double
        → s
        → [(Double, Double, Double, Double)]
adaptiveStepSizeControl1 y0 (x0, x1) f htry hmin eps st =
  assert (x1 > x0 && htry>0 && hmin>0 && htry>hmin && eps>0) $
  (x0, y0, 0, 0):loop x0 y0 htry
  where
    tiny ∷ Double
    tiny = 1.0e-30
    loop x y htry =
      let dydx = f x y
          yscal = (abs y) + h*(abs dydx) + tiny
          h = if x+htry > x1 then x1-x else htry
          (xnext, ynext, hdid, hnext, _, errmax) = step1 st y dydx x h eps yscal f
      in if (xnext >= x1)
         then [(xnext, ynext, hdid, errmax)]
         else if hnext >= hmin
              then (xnext, ynext, hdid, errmax):loop xnext ynext hnext
              else throw (StepSizeUnderflow xnext hnext)

dumb ∷ ∀ r1 r2 s . (Source r1 Double, Source r2 Double, SimpleStepper s) ⇒
         Array r1 DIM1 Double
       → (Double, Double)
       → (∀ r . (Source r Double) ⇒ Double → Vec r → Vec r2)
       → Int
       → Double
       → s
       → [(Double, VecD)]
dumb y0 (x0, x1) f nStep eps st = rkIter nStep x0 (delay y0)
  where
    h = (x1-x0)/(fromIntegral nStep)

    rkIter ∷ Int
           → Double
           → VecD
           → [(Double, VecD)]
    rkIter k x v | k>0 = let (x', v') = rkStep x v
                         in (x, v) : (rkIter (k-1) x' v')
                 | otherwise = [(x, v)]

    rkStep ∷ Double
           → VecD
           → (Double, VecD)
    rkStep x v =
      let dv = f x v
          (x', v', _) = simpleStep st v dv x h eps f
      in if x==x' then throw (StepSizeUnderflow x h) else (x', v')

dumb1 :: ∀ s . (SimpleStepper s) ⇒
         Double
      → (Double, Double)
      → (Double → Double → Double)
      → Int
      → Double
      → s
      → [(Double, Double)]
dumb1 y0 (x0, x1) f nStep eps st = rkIter nStep x0 y0
  where
    h = (x1-x0)/(fromIntegral nStep)
    
    rkIter ∷ Int → Double → Double → [(Double, Double)]
    rkIter k x v | k>0 = let (x', v') = rkStep x v
                         in (x, v) : (rkIter (k-1) x' v')
                 | otherwise = [(x, v)]

    rkStep ∷ Double → Double → (Double, Double)
    rkStep x v =
      let dv = f x v
          (x', v', _) = simpleStep1 st v dv x h eps f
      in if x==x' then throw (StepSizeUnderflow x h) else (x', v')
