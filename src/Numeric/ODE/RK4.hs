{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |Let an initial value problem be specified as follows: \(\dot y=f(t,y)\), \(y(t_0)=y_0\). Here \(y\) is an unknown function (scalar or vector) of time \(t\), which we would like to approximate; we are told that \(\dot y\), is a function of \(t\) and of \(y\) itself. At the initial time \(t_0\) the corresponding \(y\) value is \(y_0\).
--
-- Now pick a step-size \(h > 0\) and define \(y_{n+1} = y_{n}+\frac{h}{6}(k_1+2k_2+2k_3+k_4)\) and \(t_{n+1}=t_{n}+h\) for \(n = 0, 1, 2, 3, \ldots\), using
--
-- \[k_1=f(t_{n},y_{n}),\]
--
-- \[k_2=f\left(t_{n}+\frac{h}{2}, y_{n}+\frac{h}{2} k_1\right),\]
--
-- \[k_3=f\left(t_{n}+\frac{h}{2},y_{n}+\frac{h}{2} k_2\right),\]
--
-- \[k_4=f(t_{n}+h,y_{n}+h k_3).\]
--
-- Here \(y_{n+1}\) is the RK4 approximation of \(y(t_{n+1})\), and the next value \(y_{n+1}\) is determined by the present value \(y_{n}\) plus the weighted average of four increments, where each increment is the product of the size of the interval h, and an estimated slope specified by function f on the right-hand side of the differential equation.
--
-- The RK4 method is a fourth-order method, meaning that the local truncation error is on the order of \(O(h^5)\), while the total accumulated error is on the order of \(O(h^4)\).
module Numeric.ODE.RK4 (RK4(..), rk4, rk41) where

import Data.Array.Repa ((+^), Source)
import Data.Array.Repa.Util
import Numeric.ODE.Stepper

rk4 ∷ ∀ r1 r2 r3 . (Source r3 Double, Source r2 Double, Source r1 Double) ⇒
       Vec r1 -- ^ starting point y ∈ ℝⁿ
     → Vec r2 -- ^ derivative dy/dx = f(x,y) ∈ ℝⁿ at starting point (x, y)
     → Double -- ^ starting point x ∈ ℝ
     → Double -- ^ stepsize h ∈ ℝ
     → (∀ r . (Source r Double) ⇒ Double → Vec r → Vec r3) -- ^ derivative function f(x, y) = dy/dx ∈ ℝⁿ
     -> VecD -- ^ advanced solution vector y1 ∈ ℝⁿ
{-# INLINE rk4 #-}
rk4 y y' x h f = {-# SCC "rk4" #-}
  let yt1 = y +^ (0.5*h)*.y'
      dyt = f (x + 0.5*h) yt1
      yt2 = y +^ (0.5*h)*.dyt
      dym1 = f (x + 0.5*h) yt2
      yt3 = y +^ h*.dym1
      dym2 = dym1 +^ dyt
      dyt2 = f (x+h) yt3
  in y +^ (h/6.0)*.(y' +^ dyt2 +^ 2*.dym2)

rk41 :: Double -- ^ starting point y ∈ ℝ
     → Double -- ^ derivative dy/dx = f(x,y) ∈ ℝ at starting point (x, y)
     → Double -- ^ starting point x ∈ ℝ
     → Double -- ^ stepsize h ∈ ℝ
     → (Double → Double → Double)-- ^ derivative function f(x, y) = dy/dx ∈ ℝ
     → Double -- ^ advanced solution y1 ∈ ℝ
{-# INLINE rk41 #-}
rk41 !y !y' !x !h f = {-# SCC "rk41" #-}
  let dyt = f (x + 0.5*h) (y + 0.5*h*y')
      dym1 = f (x + 0.5*h) (y + 0.5*h*dyt)
      dyt2 = f (x+h) (y + h*dym1)
  in y + h/6.0*(y' + dyt2 + 2*(dym1 + dyt))

data RK4 = RK4

instance {-# OVERLAPPING #-} SimpleStepper RK4 where
  simpleStepperName _ = "4th order Runge-Kutta"
  simpleStep _ y y' x h _ f =
    let y1 = rk4 y y' x h f
    in (x+h, y1, h)
  simpleStep1 _ y y' x h _ f =
    let y1 = rk41 y y' x h f
    in (x+h, y1, h)
