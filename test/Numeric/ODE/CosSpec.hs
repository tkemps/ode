{-# LANGUAGE UnicodeSyntax, ImplicitParams, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, TypeOperators, FlexibleInstances #-}
module Numeric.ODE.CosSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Numeric.ODE

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

approxEq ∷ Double → Double → Double -> Bool
approxEq x y eps = abs (x-y) < eps

isCosSol (x1,y1,_,_) = approxEq x1 (pi/2) eps && approxEq y1 1.0 eps
  where eps = 1e-8
isCosSol' (x1,y1) = approxEq x1 (pi/2) eps && approxEq y1 1.0 eps
  where eps = 1e-8

f ∷ Double → Double → Double
{-# INLINE f #-}
f x y = cos x

x0,x1,y0 ∷ Double
y0 = 0.0
x0 = 0.0
x1 = pi/2

spec :: Spec
spec = do
  describe "dumb1 with RK4" $ do
    it "integrates y'=cos(x), y(0)=0 to y(pi/2)≅1" $ do
      (last $ dumb1 y0 (x0, x1) f 100 0.0 RK4)
        `shouldSatisfy` isCosSol'
  describe "dumb1 with RKCK" $ do
    it "integrates y'=cos(x), y(0)=0 to y(pi/2)≅1" $ do
      (last $ dumb1 y0 (x0, x1) f 100 0.0 RKCK)
        `shouldSatisfy` isCosSol'
  describe "dumb1 with ModifiedMidpoint" $ do
    it "integrates y'=cos(x), y(0)=0 to y(pi/2)≅1" $ do
      (last $ dumb1 y0 (x0, x1) f 1000 0.0 (MMID 10))
        `shouldSatisfy` isCosSol'
  describe "adaptiveStepSizeControl1 with RKCK" $ do
    it "integrates y'=cos(x), y(0)=0 to y(pi/2)≅1" $ do
      (last $ adaptiveStepSizeControl1 y0 (x0, x1) f 1e-2 0 1e-10 RKCK)
        `shouldSatisfy` isCosSol
  describe "adaptiveStepSizeControl1 with RKQSCK" $ do
    it "integrates y'=cos(x), y(0)=0 to y(pi/2)≅1" $ do
      (last $ adaptiveStepSizeControl1 y0 (x0, x1) f 1e-2 0 1e-10 RKQSCK)
        `shouldSatisfy` isCosSol
