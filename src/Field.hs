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
  , _aa :: Int
  } deriving Show

generateCoords :: Fractional a => FieldDescription a -> (V3 Int -> V2 a)
generateCoords (FromCenter res center h aa) =
  let
    res'@(V2 resY' resX') = fmap fromIntegral res
    w = let aspect = resX' / resY' in aspect * h
    offset' = center - V2 h w
    multiplier' = 2 * V2 h w / res'
    aaSq = aa*aa
    aaSq' = fromIntegral aaSq
  in
    (\(V3 y x i) ->
       let
         aaoffset = fmap ((/aaSq') . fromIntegral) $ V2 (i `div` aa) (i `rem` aa)
       in
         multiplier' * (fmap fromIntegral (V2 y x) + aaoffset) + offset'
    )

-- {-# INLINABLE generateCoords #-}
{-# SPECIALIZE generateCoords :: FieldDescription Float -> (V3 Int -> V2 Float) #-}
{-# SPECIALIZE generateCoords :: FieldDescription Double -> (V3 Int -> V2 Double) #-}

buildFieldRepa
  :: forall a b. (Fractional a, Unbox b)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> Array U DIM3 (V2 b)
buildFieldRepa fd f g =
  let
    V2 resY resX = _res fd
    coordinator = generateCoords fd
    aa = _aa fd
    dim = Z :. resY :. resX :. aa*aa
  in
    runIdentity $ computeUnboxedP $ fromFunction dim
    (\(Z :. y :. x :. i) ->
        (\v -> V2 (g v) (f v)) . coordinator $ (V3 y x i)
    )

buildFieldAccelerate
  :: (A.Elt b, Fractional a, Unbox b)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> A.Array A.DIM3 (A.V2 b)
buildFieldAccelerate fd f g =
  let
    V2 resY resX = _res fd
    aa = _aa fd
    dim = A.Z A.:. resY A.:. resX A.:. aa*aa
    coordinator = generateCoords fd
  in
    A.fromFunction dim
    (\(A.Z A.:. y A.:. x A.:. i) ->
       (\v -> A.V2 (g v) (f v)) . coordinator $ A.V3 y x i
    )

buildField
  :: forall a b. (Unbox b, Fractional a)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> UV.Vector (V2 b)
buildField fd f g =
  let
    aa = _aa fd
    aaSq = aa*aa
    coordinator :: Int -> V2 a
    coordinator i = generateCoords fd $ V3 (i `div` (resX*aaSq)) (i `rem` (resX*aaSq) `div` aaSq) (i `rem` aaSq)
    V2 resY resX = _res fd
  in
    UV.generate (resY*resX*aaSq) $ (\v -> V2 (g v) (f v)) . coordinator
