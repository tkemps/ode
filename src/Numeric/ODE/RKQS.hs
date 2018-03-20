{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, GADTs, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.ODE.RKQS (rkqs, rkqs1, RKQS(..), RKQSCK(..)) where

import Control.Exception
import Data.Array.Repa (Array(..), D, Source, DIM1)
import qualified Data.Text as T
import Numeric.ODE.Exceptions
import Numeric.ODE.RKCK
import Numeric.ODE.Stepper
import Numeric.ODE.Util

rkqs :: ∀ r1 r2 r3 r4 s .
     (Source r2 Double, Source r1 Double,
      Source r3 Double, Source r4 Double,
      Stepper s) =>
     s
     → Array r3 DIM1 Double -- ^ starting point y ∈ R^n
     → Array r1 DIM1 Double -- ^ derivative dy/dx = f(x,y) ∈ R^n at starting point (x, y)
     → Double -- ^ starting point x ∈ R
     → Double -- ^ stepsize htry ∈ R initially attempted
     → Double -- ^ required accuracy ε > 0
     → Array r4 DIM1 Double -- ^ scaling vector against which the error is scaled
     → (∀ r . (Source r Double) ⇒ Double → Array r DIM1 Double → Array r2 DIM1 Double) -- ^ derivative f(x, y) = dy/dx ∈ R^n
     → (Double,
        Array D DIM1 Double,
        Double,
        Double,
        Array D DIM1 Double,
        Double) -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R^n, actually used step size h > 0, proposed next step size hnext > 0, error vector, and scaled maximum error
rkqs st y dydx x htry eps yscal f = loop htry
  where
    loop ∷ Double → (Double, Array D DIM1 Double, Double, Double, Array D DIM1 Double, Double)
    loop h = let (_, ytemp, _, _, yerr, _) = step st y dydx x h eps yscal f
                 errmax = maxScaledError yerr yscal eps
             in if (errmax <= 1.0)
                then let hnext = if (errmax > errCon)
                                 then safety*h*(errmax**pGrow)
                                 else 5.0*h
                     in (x+h, ytemp, h, hnext, yerr, errmax)
                else let htemp = safety*h*(errmax**pShrnk)
                         hnew = if (h >= 0.0)
                                then max htemp (0.1*h)
                                else min htemp (0.1*h)
                     in if x+hnew==x
                        then throw (StepSizeUnderflow x hnew)
                             else loop hnew

safety, pGrow, pShrnk, errCon :: Double
safety = 0.9
pGrow = -0.2
pShrnk = -0.25
errCon = 1.89e-4

rkqs1 :: (Stepper s)
      ⇒ s
      → Double -- ^ starting point y ∈ R^n
      → Double -- ^ derivative dy/dx = f(x,y) ∈ R^n at starting point (x, y)
      → Double -- ^ starting point x ∈ R
      → Double -- ^ stepsize htry ∈ R initially attempted
      → Double -- ^ required accuracy ε > 0
      → Double -- ^ scaling factor against which the error is scaled
      → (Double → Double → Double) -- ^ derivative f(x, y) = dy/dx ∈ R^n
      → (Double, Double, Double, Double, Double, Double) -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R^n, used step size h > 0, next step size hnext > 0, error, and scaled maximum error
rkqs1 st y dydx x htry eps yscal f = loop htry
  where
    loop ∷ Double → (Double, Double, Double, Double, Double, Double)
    loop h = let (_, ytemp, _, _, yerr, _) = step1 st y dydx x h eps yscal f
                 errmax = maxScaledError1 yerr yscal eps
             in if (errmax <= 1.0)
                then let hnext = if (errmax > errCon)
                                 then safety*h*(errmax**pGrow)
                                 else 5.0*h
                     in (x+h, ytemp, h, hnext, yerr, errmax)
                else let htemp = safety*h*(errmax**pShrnk)
                         hnew = if (h >= 0.0)
                                then max htemp (0.1*h)
                                else min htemp (0.1*h)
                     in if x+hnew==x
                        then throw (StepSizeUnderflow x hnew)
                             else loop hnew

data RKQS = ∀ s . (Stepper s) ⇒ RKQS s

instance Stepper RKQS where
  name st = T.concat ["Quality controlled stepper based on ", name st]
  step (RKQS st) y dydx x h eps yscal f = rkqs st y dydx x h eps yscal f
  step1 (RKQS st) y dydx x h eps yscal f = rkqs1 st y dydx x h eps yscal f

data RKQSCK = RKQSCK

instance Stepper RKQSCK where
  name _ = "Quality controlled Cash–Karp"
  step _ y dydx x h eps yscal f = rkqs RKCK y dydx x h eps yscal f
  step1 _ y dydx x h eps yscal f = rkqs1 RKCK y dydx x h eps yscal f
