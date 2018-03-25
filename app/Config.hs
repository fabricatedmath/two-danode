{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( loadConfigFromArgs, Descr(..)
  , optHintDescr, optDescrLogMultiplier, optDescrFile
  , breakFileExtension
  )
where

import Data.Aeson (decode')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Arrow ((***))
import Control.Lens
import Control.Monad (when)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field
import Field.Hint

data Descr =
  Descr
  { _optHintDescr :: HintDescr Double
  , _optDescrLogMultiplier :: Maybe Double
  , _optDescrFile :: (FilePath, Maybe String)
  } deriving (Show, Read)

data Options =
  Options
  { _optFD :: FieldDescription Double
  , _optFile :: (FilePath, Maybe String)
  , _optLogMultiplier :: Maybe Double
  , _optPolar :: Bool
  , _optR :: String
  , _optT :: String
  , _optF :: String
  , _optG :: String
  , _optReadJSON :: Maybe FilePath
  , _optWriteJSON :: Bool
  , _optStop :: Bool
  } deriving (Show, Read)

optionsToDescr :: Options -> Descr
optionsToDescr o =
  let
    hintDescr =
      let
        fieldStrings
          | _optPolar o =
            Polar
            { _rString = _optR o
            , _tString = _optT o
            }
          | otherwise =
            Cartesian
            { _fString = _optF o
            , _gString = _optG o
            }
      in
        HintDescr
        { _hintDescrFD = _optFD o
        , _hintDescrFS = fieldStrings
        }
  in
    Descr
    { _optHintDescr = hintDescr
    , _optDescrLogMultiplier = _optLogMultiplier o
    , _optDescrFile = _optFile o
    }

makeLenses ''Descr
makeLenses ''Options

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (pure startOptions) actions
    descr <-
      do
        let descr = optionsToDescr opts
        case _optReadJSON opts of
          Just path ->
            do
              mjson <- decode' <$> BS.readFile path
              case mjson of
                Nothing -> die "Failed to read JSON file"
                Just json -> pure $ (optHintDescr .~ json) descr
          Nothing -> pure descr

    when (_optWriteJSON opts) $
      do
        BS.writeFile (fst (_optFile opts) ++ ".json") $
          encodePretty (_optHintDescr descr)
    case _optStop opts of
      True ->
        do
          putStrLn "Stopping due to flag"
          exitSuccess
      False -> pure $ optionsToDescr opts

breakFileExtension :: FilePath -> (FilePath, Maybe String)
breakFileExtension fp =
  let
    (filename,path) =
      reverse *** reverse $ break (=='/') $ reverse fp
    (extension,name) =
      reverse *** (reverse . drop 1) $ break (=='.') $ reverse filename
  in (path ++ name, if null extension then Nothing else Just extension)


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
  , _optFile = ("default", Just ".png")
  , _optReadJSON = Nothing
  , _optWriteJSON = False
  , _optStop = False
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
     (\arg opt -> pure $ optFile .~ breakFileExtension arg $ opt)
      "File")
    $ unlines $ ["PNG file save name"
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
  , Option "" ["write-json"]
    (NoArg
      (\opt -> pure $ optWriteJSON .~ True $ opt)
    ) "Write JSON description of field equations"
  , Option "" ["read-json"]
    (ReqArg
      (\arg opt -> pure $ optReadJSON .~ Just arg $ opt)
      "FILE")
    "Read JSON description of field equations"
  , Option "" ["stop"]
    (NoArg
      (\opt -> pure $ optStop .~ True $ opt)
    ) "Don't generate field image"
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      )
    ) "Show help"
  ]
