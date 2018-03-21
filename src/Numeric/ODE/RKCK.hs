{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |The Cash-Karp method is a member of the Runge–Kutta family of ODE
-- solvers. It uses six function evaluations to calculate fourth- and
-- fifth-order accurate solutions. The difference between these
-- solutions is then taken to be the error of the (fourth order)
-- solution. This error estimate is very convenient for adaptive
-- stepsize integration algorithms. Other similar integration methods
-- are Fehlberg (RKF) and Dormand–Prince (RKDP).
module Numeric.ODE.RKCK (RKCK(..), rkck, rkck1) where

import Data.Array.Repa (Array(..), D, (+^), Source, DIM1)
import Data.Array.Repa.Util
import Numeric.ODE.Stepper
import Numeric.ODE.Util

{-
odeintV ∷ VUDouble -- ^ starting point y ∈ R^n
        → (Double, Double) -- ^ starting point x ∈ R
        → (Double → VUDouble → VUDouble) -- ^ derivative f(x, y) = dy/dx ∈ R^n
        → Double -- ^ stepsize htry ∈ R initially attempted
        → Double -- ^ required accuracy ε > 0
        → Double -- ^ vector yscal ∈ R^n against which the error is scaled
        → [(Double, VUDouble, Double, Double)] -- ^ advanced x1 ∈ R and , advanced vector y1 ∈ R^n, used step size h > 0, next step size hnext > 0, errmax > 0
odeintV y0 (x0, x1) f htry hmin eps =
  let n = VG.length y0
      sh = (Z:.n)
      from ∷ VU.Vector Double -> Array U DIM1 Double
      from = fromUnboxed sh
      to ∷ Array D DIM1 Double -> VU.Vector Double
      to = toUnboxed . computeUnboxedS
      fR ∷ ∀ r . (Source r Double, Load r DIM1 Double) ⇒
           Double -> Array r DIM1 Double -> Array U DIM1 Double
      fR x y = from $ f x (to (delay y))
      sol = odeint (from y0) (x0, x1) fR htry hmin eps
  in map (\(x,y,hdid,errmax) → (x,to y,hdid,errmax)) sol
-}

rkck ∷ ∀ r1 r2 r3 . (Source r3 Double, Source r2 Double, Source r1 Double) ⇒
       Array r1 DIM1 Double -- ^ starting point y ∈ ℝⁿ
     → Array r2 DIM1 Double -- ^ derivative dy/dx = f(x,y) ∈ ℝⁿ at starting point (x, y)
     → Double -- ^ starting point x ∈ ℝ
     → Double -- ^ stepsize h ∈ ℝ
     → (∀ r . (Source r Double) ⇒ Double → Array r DIM1 Double → Array r3 DIM1 Double) -- ^ derivative function f(x, y) = dy/dx ∈ ℝⁿ
     → (Array D DIM1 Double, Array D DIM1 Double) -- ^ pair of (1) advanced solution vector y1 ∈ ℝⁿ and (2) error estimate ∈ ℝⁿ for this step
{-# INLINE rkck #-}
rkck y dydx x h f = {-# SCC "rkck" #-}
  let ak2, ak3, ak4, ak5, ak6 ∷ Array r3 DIM1 Double
      ak2 = f (x+a2*h) (y +^ (b21*h)*.dydx)
      ak3 = f (x+a3*h) (y +^ (b31*h)*.dydx +^ (b32*h)*.ak2)
      ak4 = f (x+a4*h) (y +^ (b41*h)*.dydx +^ (b42*h)*.ak2 +^ (b43*h)*.ak3)
      ak5 = f (x+a5*h) (y +^ (b51*h)*.dydx +^ (b52*h)*.ak2 +^ (b53*h)*.ak3
                         +^ (b54*h)*.ak4)
      ak6 = f (x+a6*h) (y +^ (b61*h)*.dydx +^ (b62*h)*.ak2 +^ (b63*h)*.ak3
                         +^ (b64*h)*.ak4 +^ (b65*h)*.ak5)
      yout, yerr ∷ Array D DIM1 Double
      yout = y +^ (h*c1)*.dydx +^ (h*c3)*.ak3 +^ (h*c4)*.ak4 +^ (h*c6)*.ak6
      yerr = (h*dc1)*.dydx +^ (h*dc3)*.ak3 +^ (h*dc4)*.ak4 +^ (h*dc5)*.ak5
             +^ (h*dc6)*.ak6
  in (yout, yerr)

rkck1 :: Double -- ^ starting point y ∈ ℝ
     → Double -- ^ derivative dy/dx = f(x,y) ∈ ℝ at starting point (x, y)
     → Double -- ^ starting point x ∈ ℝ
     → Double -- ^ stepsize h ∈ ℝ
     → (Double → Double → Double)-- ^ derivative function f(x, y) = dy/dx ∈ ℝ
     → (Double, Double) -- ^ pair of (1) advanced solution vector y1 ∈ ℝ and (2) error estimate ∈ ℝ for this step
{-# INLINE rkck1 #-}
rkck1 y dydx x h f = {-# SCC "rkck1" #-}
  let ak2 = f (x+a2*h) (y + (b21*h)*dydx)
      ak3 = f (x+a3*h) (y + (b31*h)*dydx + (b32*h)*ak2)
      ak4 = f (x+a4*h) (y + (b41*h)*dydx + (b42*h)*ak2 + (b43*h)*ak3)
      ak5 = f (x+a5*h) (y + (b51*h)*dydx + (b52*h)*ak2 + (b53*h)*ak3
                         + (b54*h)*ak4)
      ak6 = f (x+a6*h) (y + (b61*h)*dydx + (b62*h)*ak2 + (b63*h)*ak3
                         + (b64*h)*ak4 + (b65*h)*ak5)
      yout = y + (h*c1)*dydx + (h*c3)*ak3 + (h*c4)*ak4 + (h*c6)*ak6
      yerr = (h*dc1)*dydx + (h*dc3)*ak3 + (h*dc4)*ak4 + (h*dc5)*ak5
             + (h*dc6)*ak6
  in (yout, yerr)

a2, a3, a4, a5, a6 ∷ Double
a2 = 0.2
a3 = 0.3
a4 = 0.6
a5 = 1.0
a6 = 0.875
b21, b31, b32, b41, b42, b43, b51, b52, b53, b54, b61, b62, b63, b64, b65 ∷ Double
b21 = 0.2
b31 = 3.0/40.0
b32 = 9.0/40.0
b41 = 0.3
b42 = -0.9
b43 = 1.2
b51 = -11.0/54.0
b52 = 2.5
b53 = -70.0/27.0
b54 = 35.0/27.0
b61 = 1631.0/55296.0
b62 = 175.0/512.0
b63 = 575.0/13824.0
b64 = 44275.0/110592.0
b65 = 253.0/4096.0
c1, c3, c4, c6 ∷ Double
c1 = 37.0/378.0
c3 = 250.0/621.0
c4 = 125.0/594.0
c6 = 512.0/1771.0
dc5, dc1, dc3, dc4, dc6 ∷ Double
dc5 = -277.00/14336.0
dc1 = c1-2825.0/27648.0
dc3 = c3-18575.0/48384.0
dc4 = c4-13525.0/55296.0
dc6 = c6-0.25

data RKCK = RKCK

instance Stepper RKCK where
  stepperName _ = "Cash–Karp"
  step _ y dydx x h eps yscal f = let (y1, yerr) = rkck y dydx x h f
                                  in (x+h, y1, h, h, yerr,
                                      maxScaledError yerr yscal eps)
  step1 _ y dydx x h eps yscal f = let (y1, yerr) = rkck1 y dydx x h f
                                   in (x+h, y1, h, h, yerr,
                                      maxScaledError1 yerr yscal eps)

