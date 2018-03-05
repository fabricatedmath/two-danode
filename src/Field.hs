{-# LANGUAGE ScopedTypeVariables #-}

module Field where

import Linear

import Control.Monad.Identity (runIdentity)

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Linear as A

import Data.Array.Repa

import Data.Function (on)

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as UV

data FieldDescription a =
  FromCenter
  { _res :: !(V2 Int)
  , _center :: !(V2 a)
  , _h :: !a
  } deriving Show

generateCoords :: Fractional a => FieldDescription a -> (V2 Int -> V2 a)
generateCoords  (FromCenter res center h) =
  let
    res'@(V2 resY' resX') = fmap fromIntegral res
    w = let aspect = resX' / resY' in aspect * h
    offset' = center - V2 h w
    multiplier' = 2 * V2 h w / res'
  in
    (\loc -> multiplier' * fmap fromIntegral loc + offset')

{-# SPECIALIZE generateCoords :: FieldDescription Float -> (V2 Int -> V2 Float) #-}
{-# SPECIALIZE generateCoords :: FieldDescription Double -> (V2 Int -> V2 Double) #-}

buildFieldRepa
  :: (Fractional a, Unbox b)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> Array U DIM2 (V2 b)
buildFieldRepa fd f g =
  let
    V2 resY resX = _res fd
    coordinator = generateCoords fd
  in
    runIdentity $ computeUnboxedP $ fromFunction (Z :. resY :. resX)
    (\(Z :. y :. x) -> (\v -> V2 (f v) (g v)) . coordinator $ V2 y x)

buildFieldAccelerate
  :: (A.Elt b, Fractional a, Unbox b)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> A.Array A.DIM2 (A.V2 b)
buildFieldAccelerate fd f g =
  let
    V2 resY resX = _res fd
    dim = A.Z A.:. resY A.:. resX
    coordinator = generateCoords fd
  in
    A.fromFunction dim
    (\(A.Z A.:. y A.:. x) -> (\v -> A.V2 (f v) (g v)) . coordinator $ A.V2 y x)

buildField
  :: forall a b. (Unbox b, Fractional a)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> UV.Vector (V2 b)
buildField fd f g =
  let
    coordinator :: Int -> V2 a
    coordinator i = generateCoords fd $ V2 (i `div` resX) (i `rem` resX)
    V2 resY resX = _res fd
  in
    UV.generate (resY*resX) $ (\v -> V2 (f v) (g v)) . coordinator
