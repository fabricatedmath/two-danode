{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Identity (runIdentity)

import Codec.Picture

import Data.Array.Repa (Array,DIM2,DIM3,U,deepSeqArray,(:.)(..),Z(..))
import qualified Data.Array.Repa as R

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL

import Data.Vector (convert)
import qualified Data.Vector.Storable as VS
import Data.Vector.Unboxed (Unbox)

import Data.Word (Word8)

import Linear

import Config

import Field
import Field.Hint
import Field.Hint.Repa

import Pipes.Graphics

main :: IO ()
main =
  do
    descr <- loadConfigFromArgs
    let
      hintDescr = _optHintDescr descr
      fd = _hintDescrFD hintDescr
      (file,_mext) = _optFile descr
      logMul = _optLogMultiplier descr
    eSpace <- buildPhaseSpace hintDescr
    either
      print
      (writePng (file ++ ".png") . repaToImage . makeImage fd logMul)
      eSpace

makeImage
  :: (Epsilon a, Unbox a, RealFrac a, Floating a, Ord a, RealFloat a)
  => FieldDescription a
  -> Maybe a --logMul
  -> Array U DIM3 (V2 a)
  -> Array U DIM2 (V3 Word8)
makeImage fd logMul vectorField =
  let
    aa' = _fdAA fd
    aaSq' = fromIntegral $ aa'*aa'
  in
    runIdentity $
      do
        maxV <- R.foldAllP max 0 $ R.map norm $ vectorField
        image' <- R.sumP $ R.map (renderPoint logMul maxV) vectorField
        image <- R.computeUnboxedP $
          R.map (fmap (round . (*255) . (/aaSq'))) image'
        maxV `seq` image' `deepSeqArray` return image

renderPoint
  :: (Epsilon a, RealFrac a, Floating a, Ord a, RealFloat a)
  => Maybe a --logMul
  -> a --maxV
  -> V2 a
  -> V3 a
renderPoint mlogMul maxV v@(V2 y x) =
  let
    theta = atan2 y x
    applyLogFilter n =
      case mlogMul of
        Nothing -> n
        Just logMul -> log $ (n*logMul + 1)
    h' = 360*theta/(2*pi)
    s' = 0.5
    v' = (*0.6) $ applyLogFilter (norm v) / applyLogFilter maxV
  in uncurryRGB V3 $ hsl h' s' v'
{-# INLINABLE renderPoint #-}
