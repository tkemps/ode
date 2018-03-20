{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Strict #-}
module Numeric.ODE.Exceptions (StepSizeUnderflow(..)) where

import Control.Exception

data StepSizeUnderflow = StepSizeUnderflow {
  stepSizeUnderflowX ∷ Double,
  stepSizeUnderflowH ∷ Double}
  deriving Show

instance Exception StepSizeUnderflow

