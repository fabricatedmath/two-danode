{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa (Array,DIM2,DIM3,U,deepSeqArray)
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL

import Data.Vector.Unboxed (Unbox)

import Data.Word (Word8)

import Linear

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field
import Field.Hint.Repa

data Options =
  Options
  { _optFD :: FieldDescription Double
  , _optFile :: FilePath
  , _optLogMultiplier :: Maybe Double
  , _optPolar :: Bool
  , _optR :: String
  , _optT :: String
  , _optF :: String
  , _optG :: String
  } deriving Show

makeLenses ''Options

startOptions :: Options
startOptions =
  Options
  { _optFD = defaultFieldDescription
  , _optF = "y"
  , _optG = "-sin x"
  , _optPolar = False
  , _optR = "r*(1-r*r)"
  , _optT = "1"
  , _optLogMultiplier = Nothing
  , _optFile = "default.bmp"
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "F" []
    (ReqArg
     (\arg opt -> pure $ optF .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for 'x dot', F (x,y) = _"
                ,"Default: " ++ show (_optF startOptions)]
  , Option "G" []
    (ReqArg
     (\arg opt -> pure $ optG .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for 'y dot', G (x,y) = _"
                ,"Default: " ++ show (_optG startOptions)]
  , Option "P" []
    (NoArg
     (\opt -> pure $ optPolar .~ True $ opt))
    $ unlines $ ["Set to use polar coordinates in terms of r and theta"
                ,"as r-dot and theta-dot"]
  , Option "R" []
    (ReqArg
     (\arg opt -> pure $ optR .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for polar 'r dot', R (r,theta) = _"
                ,"Default: " ++ show (_optR startOptions)]
  , Option "T" []
    (ReqArg
     (\arg opt -> pure $ optT .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for polar 'theta dot', T (r,theta) = _"
                ,"Default: " ++ show (_optT startOptions)]
  , Option "f" []
    (ReqArg
     (\arg opt -> pure $ optFile .~ arg $ opt)
      "File")
    $ unlines $ ["BMP file save name"
                ,"Default: " ++ show (_optFile startOptions)]
  , Option "a" ["aa"]
    (ReqArg
      (\arg opt -> pure $ optFD.fdAA .~ read arg $ opt)
      "Int")
    $ unlines $ ["Anti-Aliasing"
                ,"Default: " ++ show (startOptions ^. optFD.fdAA)]
  , Option "r" ["res"]
    (ReqArg
     (\arg opt -> pure $ optFD.fdRes .~ read arg ^. tupleToV2 $ opt)
      "(Int,Int)")
    $ unlines $
    ["Resolution of output image, needs quotes in cmd line"
    ,"Default: " ++ show (startOptions ^. optFD.fdRes.v2ToTuple.to show)]
  , Option "c" ["center"]
    (ReqArg
     (\arg opt -> pure $ optFD.fdCenter .~ read arg ^. tupleToV2 $ opt)
      "(Double,Double)")
    $ unlines $
    ["Center of field View, needs quotes in cmd line"
    ,"Default: " ++ show (startOptions ^. optFD.fdCenter.v2ToTuple.to show)]
  , Option "H" ["height"]
    (ReqArg
     (\arg opt -> pure $ optFD.fdHeight .~ read arg $ opt)
      "Double")
    $ unlines $ ["Height of field view"
                ,"Default: " ++ show (startOptions ^. optFD.fdHeight)]
  , Option "m" ["multiplier"]
    (ReqArg
     (\arg opt -> pure $ optLogMultiplier .~ Just (read arg)  $ opt)
      "Double")
    $ unlines $
    ["Multiplier of log function to convert colors,"
    ,"a higher number means less intensity variability "
    ,"between low and high magnitude vectors"
    ,"Default: " ++ show (startOptions ^. optLogMultiplier)]
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      )
    ) "Show help"
  ]

main :: IO ()
main =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let
      fieldS
        | _optPolar opts = Polar (_optR opts) (_optT opts)
        | otherwise = Cartesian (_optF opts) (_optG opts)
      file = _optFile opts
      fd = _optFD opts
      logMul = _optLogMultiplier opts
    eSpace <- buildPhaseSpace fd fieldS
    either print (writeImageToBMP file . makeImage fd logMul) eSpace

makeImage
  :: (Epsilon a, Unbox a, RealFrac a, Floating a, Ord a)
  => FieldDescription a
  -> Maybe a --logMul
  -> Array U DIM3 (V2 a)
  -> Array U DIM2 (Word8,Word8,Word8)
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
          R.map (\v ->
                    let
                      V3 r g b = fmap (round . (*255) . (/aaSq')) v
                    in (r,g,b)
                ) image'
        maxV `seq` image' `deepSeqArray` return image

renderPoint
  :: (Epsilon a, RealFrac a, Floating a, Ord a)
  => Maybe a --logMul
  -> a --maxV
  -> V2 a
  -> V3 a
renderPoint mlogMul maxV v@(V2 y _x) =
  let
    theta | y < 0 = 2*pi - theta'
          | otherwise = theta'
      where theta' = acos $ (V2 0 1) `dot` normalize v
    applyLogFilter n =
      case mlogMul of
        Nothing -> n
        Just logMul -> log $ (n*logMul + 1)
    h' = 360*theta/(2*pi)
    s' = 0.5
    v' = (*0.6) $ applyLogFilter (norm v) / applyLogFilter maxV
  in uncurryRGB V3 $ hsl h' s' v'
{-# INLINABLE renderPoint #-}
