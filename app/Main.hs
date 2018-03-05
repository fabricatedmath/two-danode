{-# LANGUAGE TypeOperators #-}

module Main where

import Field

import Control.Monad.Identity (runIdentity)

import Data.Array.Repa (Array,DIM2,DIM3,U,Z(..),(:.)(..),deepSeqArray)
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL

import Data.Vector.Unboxed (Vector,Unbox)
import qualified Data.Vector.Unboxed as UV

import Data.Word (Word8)

import Linear

import Language.Haskell.Interpreter

import System.Console.GetOpt
import System.Console.Readline
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

getUsage :: IO String
getUsage =
  do
    prg <- getProgName
    return $ usageInfo prg options

data Options =
  Options
  { optRes :: V2 Int
  , optFieldCenter :: V2 Double
  , optFieldHeight :: Double
  , optAA :: Int
  , optFile :: FilePath
  , optLogMultiplier :: Double
  , optF :: String
  , optG :: String
  }

startOptions :: Options
startOptions =
  Options
  { optF = "y"
  , optG = "-sin x"
  , optRes = V2 1080 1920
  , optFieldCenter = V2 0 0
  , optFieldHeight = 2
  , optAA = 1
  , optLogMultiplier = 2
  , optFile = "default.bmp"
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "F" []
    (ReqArg
     (\arg opt -> return opt {optF = arg})
      "String")
    "Function String for F (x,y) = _\nDefault: \"y\""
  , Option "G" []
    (ReqArg
     (\arg opt -> return opt {optG = arg})
      "String")
    "Function String for G (x,y) = _\nDefault: \"-sin x\""
  , Option "f" []
    (ReqArg
     (\arg opt -> return opt {optG = arg})
      "File")
    "BMP file save name\nDefault: default.bmp"
  , Option "a" ["aa"]
    (ReqArg
     (\arg opt -> return opt {optAA = read arg})
      "Int")
    "Anti-Aliasing\nDefault: 1"
  , Option "r" ["res"]
    (ReqArg
     (\arg opt -> return opt {optRes = let (x,y) = read arg in V2 y x})
      "(Int,Int)")
    "Resolution of output image\nDefault: (1920,1080)"
  , Option "c" ["center"]
    (ReqArg
     (\arg opt -> return opt {optFieldCenter = let (x,y) = read arg in V2 y x})
      "(Double,Double)")
    "Center of field View\nDefault: (0,0)"
  , Option "H" ["height"]
    (ReqArg
     (\arg opt -> return opt {optFieldHeight = read arg})
      "Double")
    "Height of field view\nDefault: 1"
  , Option "m" ["multiplier"]
    (ReqArg
     (\arg opt -> return opt {optLogMultiplier = read arg})
      "Double")
    "Multiplier of log function to convert colors,\na higher number means less color variability between low and high magnitude vectors\nDefault: 2"
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
      file = optFile opts
      fd = FromCenter (optRes opts) (optFieldCenter opts) (optFieldHeight opts) (optAA opts)
      dim = let V2 y x = _res fd
                aa = _aa fd
            in Z :. y :. x :. aa*aa :: DIM3
      fs = optF opts
      gs = optG opts
      logMul = optLogMultiplier opts

    result <-
      runInterpreter $
      do
        let command =
              unwords
              [ "toUnboxed $ buildFieldRepa"
              , "(" ++ show fd ++ ")"
              , "(\\(V2 y x) -> " ++ fs ++ ")"
              , "(\\(V2 y x) -> " ++ gs ++ ")"
              ]
        set [languageExtensions := [TypeOperators]]
        setImports [ "Data.Array.Repa"
                   , "Data.Vector.Unboxed"
                   , "Linear"
                   , "Prelude"
                   , "Field"
                   ]
        interpret command (as :: Vector (V2 Double))
    case result of
      Left _ -> print result
      Right v -> writeImageToBMP file $ makeImage fd logMul $ R.fromUnboxed dim v

makeImage
  :: (Epsilon a, Unbox a, RealFrac a, Floating a, Ord a)
  => FieldDescription a
  -> a
  -> Array U DIM3 (V2 a)
  -> Array U DIM2 (Word8,Word8,Word8)
makeImage fd logMul vectorField =
  let
    aa = _aa fd
    aaSq' = fromIntegral $ aa*aa
    maxV = runIdentity $ R.foldAllP max 0 $ R.map norm $ vectorField
    image' = runIdentity $ R.sumP $ R.map (renderPoint logMul maxV) vectorField
    image =
      runIdentity $ R.computeUnboxedP $
      R.map (\v ->
               let
                 V3 r g b = fmap (round . (*255) . (/aaSq')) v
               in (r,g,b)
            ) image'
  in
    maxV `seq` image' `deepSeqArray` image

renderPoint
  :: (Epsilon a, RealFrac a, Floating a, Ord a)
  => a
  -> a
  -> V2 a
  -> V3 a
renderPoint logMul maxV v@(V2 y _x) =
  let
    theta =
      let
        theta' = acos $ (V2 0 1) `dot` normalize v
      in if y < 0 then 2*pi - theta' else theta'
    applyLogFilter n = log $ (n*logMul + 1)
    h' = 360*theta/(2*pi)
    s' = 0.5
    v' = (*0.6) $ applyLogFilter (norm v) / applyLogFilter maxV
  in uncurryRGB V3 $ hsl h' s' v'
