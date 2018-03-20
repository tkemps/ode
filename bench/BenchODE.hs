{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Criterion
import Criterion.Main
import Criterion.Types
import Data.Array.Repa (Z(..), (!), (:.)(..), sumAllS)
import Data.Vector.Util
import Numeric.ODE

setupEnv :: Monad m => m (Int, Int)
setupEnv = do
  let small = 40
  let big = 200
  return (small, big)

f ∷ Double → Double → Double
{-# INLINE f #-}
f !x !y = 7*y^(2∷Int)*x^(3∷Int)

main :: IO ()
main = defaultMainWith
  defaultConfig {
    timeLimit = 10.0,
    reportFile = Just "bench-ODE.html",
    verbosity = Verbose}
  [
  env setupEnv $ \ ~(small,big) -> bgroup "ODE" [
      bgroup "small, y'=7*y^0.5*x^3, y(2)=3" [
          bench "RK4+rkDumb1" $ nf
            (\n → let sol = dumb1 3.0 (2.0, 5.0) f n 1e-6 RK4
                  in  snd $ last sol) small
          , bench "RKCK+odeint" $ nf
            (\n → let x1 = 2.0
                      x2 = 5.0
                      sol = adaptiveStepSizeControl (singleton 3.0)
                              (x1, x2)
                              (\x _ → singleton (cos x))
--                              (\x y → let y0 = y!(Z:.0)
--                                      in singleton (7*y0**0.5*x^(3∷Int)))
                              ((x2-x1)/fromIntegral n)
                              0
                              (1e-8/fromIntegral n)
                              RKCK
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - (sumAllS y1)) sol)
                  in sumAbsErr) small
          , bench "RKCK+odeint1" $ nf
            (\n → let sol = adaptiveStepSizeControl1 3.0
                            (2.0, 5.0)
                            (\x y → 7*y**0.5*x^(3∷Int))
                            (1.0/fromIntegral n)
                            0
                            (1e-10/fromIntegral n)
                            RKCK
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - y1) sol)
                  in sumAbsErr) small]
      ,  bgroup "big, y'=7*y^0.5*x^3, y(2)=3" [
          bench "RK4+rkDumb1" $ nf
            (\n → let f ∷ Double → Double → Double
                      f x y = 7*y**0.5*x^(3∷Int)
                      sol = dumb1 3.0 (2.0, 5.0) f n 1e-6 RK4
                  in  snd $ last sol) big
          , bench "RKCK+odeint" $ nf
            (\n → let sol = adaptiveStepSizeControl (singleton 3.0)
                              (2.0, 5.0)
                              (\x y → let y0 = y!(Z:.0)
                                      in singleton (7*y0**0.5*x^(3∷Int)))
                              (1.0/fromIntegral n)
                              0
                              (1e-10/fromIntegral n)
                              RKCK
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - (sumAllS y1)) sol)
                  in sumAbsErr) big
          , bench "RKCK+odeint1" $ nf
            (\n → let sol = adaptiveStepSizeControl1 3.0
                              (2.0, 5.0)
                              (\x y → 7*y**0.5*x^(3∷Int))
                              (1.0/fromIntegral n)
                              0
                              (1e-10/fromIntegral n)
                              RKCK
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - y1) sol)
                  in sumAbsErr) big]
      ]
  ]
