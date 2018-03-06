{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Field where

import Control.Lens
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as UV

import Linear

data FieldDescription a =
  FromCenter
  { _res :: !(V2 Int)
  , _center :: !(V2 a)
  , _h :: !a
  , _aa :: Int
  } deriving Show

defaultFieldDescription :: Num a => FieldDescription a
defaultFieldDescription = FromCenter (V2 1080 1920) (V2 0 0) 1 1

makeLenses ''FieldDescription

generateCoords :: Fractional a => FieldDescription a -> (V3 Int -> V2 a)
generateCoords fd =
  let
    h' = _h fd
    res'@(V2 resY' resX') = fmap fromIntegral $ _res fd
    w = let aspect = resX' / resY' in aspect * h'
    offset' = _center fd - V2 h' w
    multiplier' = 2 * V2 h' w / res'
    aa' = _aa fd
    aaSq = aa'*aa'
    aaSq' = fromIntegral aaSq
  in
    (\(V3 y x i) ->
       let
         aaoffset =
           fmap ((/aaSq') . fromIntegral) $ V2 (i `div` aa') (i `rem` aa')
       in
         multiplier' * (fmap fromIntegral (V2 y x) + aaoffset) + offset'
    )

{-# INLINABLE generateCoords #-}
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
    aa' = _aa fd
    dim = Z :. resY :. resX :. aa'*aa'
  in
    runIdentity $ computeUnboxedP $ fromFunction dim
    (\(Z :. y :. x :. i) ->
        (\v -> V2 (g v) (f v)) . coordinator $ (V3 y x i)
    )

buildField
  :: forall a b. (Unbox b, Fractional a)
  => FieldDescription a
  -> (V2 a -> b)
  -> (V2 a -> b)
  -> Vector (V2 b)
buildField fd f g =
  let
    aa' = _aa fd
    aaSq = aa'*aa'
    coordinator :: Int -> V2 a
    coordinator i =
      generateCoords fd $
      V3 (i `div` (resX*aaSq)) (i `rem` (resX*aaSq) `div` aaSq) (i `rem` aaSq)
    V2 resY resX = _res fd
  in
    UV.generate (resY*resX*aaSq) $ (\v -> V2 (g v) (f v)) . coordinator
