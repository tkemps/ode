{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Array.Repa.Util where

import Data.Array.Repa (Array(..), D, DIM1, Source, Shape, computeUnboxedS)
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

type VecD = Array D DIM1 Double
type Vec r = Array r DIM1 Double

-- | Multiplication of a repa array with a scalar -- each element is multiplied with the scalar.
(*.) :: (Num d, Source r d, Shape sh) =>
        d -> Array r sh d -> Array D sh d
x *. v = R.map (\vi → x*vi) v
infixr 7 *.

instance (Shape sh, Show sh, VU.Unbox e, Show e) ⇒ Show (Array D sh e) where
  show v = show (computeUnboxedS v)
