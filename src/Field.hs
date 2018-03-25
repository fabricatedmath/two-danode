{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Field where

import Control.Lens

import Data.Aeson
  (ToJSON, FromJSON, toEncoding, defaultOptions, genericToEncoding)
import Data.Aeson.Linear ()

import Data.Function (on)

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed as UV

import GHC.Generics (Generic)

import Linear

data FieldDescription a =
  FromCenter
  { _fdAspect :: !a
  , _fdCenter :: !(V2 a)
  , _fdHeight :: !a
  , _fdAA :: Int
  , _fdWidth :: Int
  } deriving (Generic, Show, Read)

_fdRes
  :: (Fractional a, RealFrac a)
  => FieldDescription a
  -> V2 Int
_fdRes fd =
  let
    aspect = _fdAspect fd
    width = _fdWidth fd
    width' = fromIntegral width
    height = round $ width' * recip aspect
  in
    V2 height width

fdRes
  :: RealFrac a
  => Lens (FieldDescription a) (FieldDescription a) (V2 Int) (V2 Int)
fdRes = lens _fdRes setter
  where
    setter fd (V2 height width) =
      let
        aspect = ((/) `on` fromIntegral) width height
      in
        fd { _fdWidth = width, _fdAspect = aspect}

instance ToJSON a => ToJSON (FieldDescription a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (FieldDescription a)

makeLenses ''FieldDescription

defaultFieldDescription :: Num a => FieldDescription a
defaultFieldDescription = FromCenter 1 (V2 0 0) 1 1 1080

v2ToTuple :: Getter (V2 a) (a,a)
v2ToTuple = to (\(V2 y x) -> (x,y))

tupleToV2 :: Getter (a,a) (V2 a)
tupleToV2 = to (\(x,y) -> V2 y x)

generateCoords
  :: (Fractional a, RealFrac a)
  => FieldDescription a
  -> (V3 Int -> V2 a)
generateCoords fd =
  let
    h' = _fdHeight fd
    res'@(V2 resY' resX') = fmap fromIntegral $ _fdRes fd
    w = let aspect = resX' / resY' in aspect * h'
    offset' = _fdCenter fd - V2 h' w
    multiplier' = 2 * V2 h' w / res'
    aa' = _fdAA fd
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

buildField
  :: forall a b. (Unbox b, Fractional a, RealFrac a)
  => FieldDescription a
  -> (V2 a -> V2 b)
  -> Vector (V2 b)
buildField fd fg =
  let
    aa' = _fdAA fd
    aaSq = aa'*aa'
    coordinator :: Int -> V2 a
    coordinator i =
      generateCoords fd $
      V3 (i `div` (resX*aaSq)) (i `rem` (resX*aaSq) `div` aaSq) (i `rem` aaSq)
    V2 resY resX = _fdRes fd
  in
    UV.generate (resY*resX*aaSq) $ fg . coordinator
