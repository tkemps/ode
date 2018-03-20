{-# LANGUAGE UnicodeSyntax, MonoLocalBinds, FlexibleContexts #-}
{-# LANGUAGE Strict #-}
module Data.Vector.Util where

import qualified Data.Vector.Unboxed as VU
import Data.Array.Repa (Array, U, Z(..), DIM1, Shape, (:.)(..), fromListUnboxed)

type VUDouble = VU.Vector Double

singleton :: (VU.Unbox a, Shape DIM1) =>
             a -> Array U DIM1 a
singleton x = fromListUnboxed (Z:.1) [x]
