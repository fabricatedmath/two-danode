{-# LANGUAGE TemplateHaskell #-}

module Main where

import Field

import Control.Lens
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa (Array,DIM2,DIM3,U,Z(..),(:.)(..),deepSeqArray)
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL

import Data.Vector.Unboxed (Vector,Unbox)

import Data.Word (Word8)

import Linear

import Language.Haskell.Interpreter

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

data Options =
  Options
  { _optFieldDescription :: FieldDescription Double
  , _optFile :: FilePath
  , _optLogMultiplier :: Double
  , _optF :: String
  , _optG :: String
  } deriving Show

makeLenses ''Options

startOptions :: Options
startOptions =
  Options
  { _optFieldDescription = defaultFieldDescription
  , _optF = "y"
  , _optG = "-sin x"
  , _optLogMultiplier = 2
  , _optFile = "default.bmp"
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "F" []
    (ReqArg
     (\arg opt -> pure $ optF .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for F (x,y) = _","(Default: \"y\")"]
  , Option "G" []
    (ReqArg
     (\arg opt -> pure $ optG .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for G (x,y) = _","(Default: \"-sin x\")"]
  , Option "f" []
    (ReqArg
     (\arg opt -> pure $ optFile .~ arg $ opt)
      "File")
    $ unlines $ ["BMP file save name","(Default: default.bmp)"]
  , Option "a" ["aa"]
    (ReqArg
      (\arg opt -> pure $ optFieldDescription.aa .~ (read arg) $ opt)
      "Int")
    $ unlines $ ["Anti-Aliasing","(Default: 1)"]
  , Option "r" ["res"]
    (ReqArg
     (\arg opt ->
        let (x,y) = read arg
            v = V2 y x
        in pure $ optFieldDescription.res .~ v $ opt)
      "(Int,Int)")
    $ unlines $ ["Resolution of output image, needs quotes in cmd line"
                ,"(Default: (1920,1080))"]
  , Option "c" ["center"]
    (ReqArg
     (\arg opt ->
        let (x,y) = read arg
            v = V2 y x
        in pure $ optFieldDescription.center .~ v $ opt)
      "(Double,Double)")
    $ unlines $ ["Center of field View, needs quotes in cmd line"
                ,"(Default: (0,0))"]
  , Option "H" ["height"]
    (ReqArg
     (\arg opt -> pure $ optFieldDescription.h .~ (read arg) $ opt)
      "Double")
    $ unlines $ ["Height of field view","(Default: 1)"]
  , Option "m" ["multiplier"]
    (ReqArg
     (\arg opt -> pure $ optLogMultiplier .~ read arg  $ opt)
      "Double")
    $ unlines $ ["Multiplier of log function to convert colors,"
                ,"a higher number means less color variability "
                ,"between low and high magnitude vectors"
                ,"(Default: 2)"]
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      )
    ) "Show help"
  ]

getUsage :: IO String
getUsage =
  do
    prg <- getProgName
    return $ usageInfo prg options

main :: IO ()
main =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let
      fs = _optF opts
      gs = _optG opts
      file = _optFile opts
      fd = _optFieldDescription opts
      dim = let V2 y x = _res fd
                aa' = _aa fd
            in Z :. y :. x :. aa'*aa' :: DIM3
      logMul = _optLogMultiplier opts
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
        setImports [ "Data.Array.Repa"
                   , "Data.Vector.Unboxed"
                   , "Linear"
                   , "Prelude"
                   , "Field"
                   ]
        interpret command (as :: Vector (V2 Double))
    case result of
      Left _ -> print result
      Right v ->
        writeImageToBMP file $ makeImage fd logMul $ R.fromUnboxed dim v

makeImage
  :: (Epsilon a, Unbox a, RealFrac a, Floating a, Ord a)
  => FieldDescription a
  -> a
  -> Array U DIM3 (V2 a)
  -> Array U DIM2 (Word8,Word8,Word8)
makeImage fd logMul vectorField =
  let
    aa' = _aa fd
    aaSq' = fromIntegral $ aa'*aa'
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
