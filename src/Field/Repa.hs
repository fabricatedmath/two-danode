module Field.Repa where

import Data.Array.Repa
import Data.Vector.Unboxed

import Linear

import Field

buildFieldRepa
  :: (Fractional a, Monad m, Unbox b)
  => FieldDescription a
  -> (V2 a -> V2 b)
  -> m (Array U DIM3 (V2 b))
buildFieldRepa fd fg =
  let
    V2 resY resX = _fdRes fd
    coordinator = generateCoords fd
    aa' = _fdAA fd
    dim = Z :. resY :. resX :. aa'*aa'
  in
    computeUnboxedP $ fromFunction dim
    (\(Z :. y :. x :. i) -> fg . coordinator $ (V3 y x i))
{-# INLINABLE buildFieldRepa #-}
