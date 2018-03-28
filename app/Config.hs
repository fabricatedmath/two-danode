{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( loadConfigFromArgs, Descr(..)
  , optHintDescr, optLogMultiplier, optFile
  , breakFileExtension
  )
where

import Control.Arrow ((***))
import Control.Lens
import Data.Maybe (fromMaybe)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field.Hint
import Field.Hint.Config

data Descr =
  Descr
  { _optHintDescr :: HintDescr Double
  , _optLogMultiplier :: Maybe Double
  , _optFile :: (FilePath, Maybe String)
  } deriving (Show, Read)

makeLenses ''Descr

defaultDescr :: Descr
defaultDescr =
  Descr
  { _optHintDescr = defaultHintDescr
  , _optLogMultiplier = Nothing
  , _optFile = ("default", Just ".png")
  }

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt Permute options args
    opts <- foldl (>>=) (pure defaultDescr) actions
    hintDescr <- loadHintDescrFromArgs (fst $ _optFile opts) args
    pure $ optHintDescr .~ hintDescr $ opts

breakFileExtension :: FilePath -> (FilePath, Maybe String)
breakFileExtension fp =
  let
    (filename,path) =
      reverse *** reverse $ break (=='/') $ reverse fp
    (extension,name) =
      reverse *** (reverse . drop 1) $ break (=='.') $ reverse filename
  in (path ++ name, if null extension then Nothing else Just extension)

options :: [OptDescr (Descr -> IO Descr)]
options =
  [ Option "f" []
    (ReqArg
      (\arg -> (\opt -> pure $ optFile .~ breakFileExtension arg $ opt))
      "File")
    $ unlines $ ["PNG file save name"
                ,"Default: " ++ show (
                    let (p,s) = defaultDescr ^. optFile
                    in p ++ fromMaybe "" s)]
  , Option "m" ["multiplier"]
    (ReqArg
     (\arg -> (\opt -> pure $ optLogMultiplier .~ Just (read arg) $ opt))
      "Double")
    $ unlines $
    ["Multiplier of log function to convert colors,"
    ,"a higher number means less intensity variability "
    ,"between low and high magnitude vectors"
    ,"Default: " ++ show (defaultDescr ^. optLogMultiplier)]
  , Option "h" ["help"]
    (NoArg
      ((\_ -> do
          prg <- getProgName
          let stripType = map (fmap $ const ())
          hPutStrLn stderr $
            usageInfo prg
            ( stripType (hintDescrOptions :: [OptDescr (HintOption Double)]) ++
              stripType options
            )
          exitWith ExitSuccess
      ))
    ) "Show help"
  ]
