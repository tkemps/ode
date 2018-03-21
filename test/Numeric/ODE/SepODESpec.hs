{-# LANGUAGE UnicodeSyntax, ImplicitParams, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances #-}
module Numeric.ODE.SepODESpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Numeric.ODE

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

approxEq ∷ Double → Double → Double -> Bool
approxEq x y eps = abs (x-y) < eps

isSol (x1',y1',_,_) = approxEq x1' x1 eps && approxEq y1' y1 eps
  where eps = 1e-8
isSol' (x1',y1') = approxEq x1' x1 eps && approxEq y1' y1 eps
  where eps = 1e-8

f ∷ Double → Double → Double
{-# INLINE f #-}
f x y = x*y

x0,x1,y0,y1 ∷ Double
y0 = 1.0
x0 = 0.0
x1 = 2.0
y1 = y0*exp(0.5*x1^^2)

whatItDoes = "integrates y'=x*y, y(0)=1 to y(2)≅7.38905609893065 with eps = 1e-8"

spec :: Spec
spec = do
  describe "dumb1 with RK4" $ do
    it whatItDoes $ do
      (last $ dumb1 y0 (x0, x1) f 500 0.0 RK4)
        `shouldSatisfy` isSol'
  describe "dumb1 with RKCK" $ do
    it whatItDoes $ do
      (last $ dumb1 y0 (x0, x1) f 100 0.0 RKCK)
        `shouldSatisfy` isSol'
  describe "dumb1 with ModifiedMidpoint" $ do
    it whatItDoes $ do
      (last $ dumb1 y0 (x0, x1) f 5000 0.0 (MMID 10))
        `shouldSatisfy` isSol'
  describe "adaptiveStepSizeControl1 with RKCK" $ do
    it whatItDoes $ do
      (last $ adaptiveStepSizeControl1 y0 (x0, x1) f 1e-2 0 1e-10 RKCK)
        `shouldSatisfy` isSol
  describe "adaptiveStepSizeControl1 with RKQSCK" $ do
    it whatItDoes $ do
      (last $ adaptiveStepSizeControl1 y0 (x0, x1) f 1e-2 0 1e-10 RKQSCK)
        `shouldSatisfy` isSol
