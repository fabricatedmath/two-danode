{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( loadConfigFromArgs, Descr(..)
  , optHintDescr, optLogMultiplier, optFile
  , breakFileExtension
  )
where

import Control.Arrow ((***))
import Control.Lens
import Control.Monad (when)

import Data.Aeson (decode')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, listToMaybe)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field
import Field.Hint

data Descr =
  Descr
  { _optHintDescr :: HintDescr Double
  , _optLogMultiplier :: Maybe Double
  , _optFile :: (FilePath, Maybe String)
  , _optWriteJSON :: Bool
  , _optStop :: Bool
  } deriving (Show, Read)

makeLenses ''Descr

defaultDescr :: Descr
defaultDescr =
  Descr
  { _optHintDescr = defaultHintDescr
  , _optLogMultiplier = Nothing
  , _optFile = ("default", Just ".png")
  , _optWriteJSON = False
  , _optStop = False
  }

fromLeft :: a -> Either a b -> a
fromLeft a (Right _) = a
fromLeft _ (Left a) = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b (Left _) = b

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
        (lefts,rights) = partitionEithers actions
    descr <-
      do
        let descr = defaultDescr
        descr' <-
          case listToMaybe lefts of
            Just fp ->
              do
                mjson <- decode' <$> BS.readFile fp
                case mjson of
                  Nothing -> die "Failed to read JSON file"
                  Just json -> pure $ (optHintDescr .~ json) descr
            Nothing -> pure descr
        foldl (>>=) (pure descr') rights
    when (_optWriteJSON descr) $
      do
        BS.writeFile (fst (_optFile descr) ++ ".json") $
          encodePretty (_optHintDescr descr)
        putStrLn "Wrote json file"
    when (_optStop descr) $
      do
        putStrLn "Stopping due to flag"
        exitSuccess
    return descr

breakFileExtension :: FilePath -> (FilePath, Maybe String)
breakFileExtension fp =
  let
    (filename,path) =
      reverse *** reverse $ break (=='/') $ reverse fp
    (extension,name) =
      reverse *** (reverse . drop 1) $ break (=='.') $ reverse filename
  in (path ++ name, if null extension then Nothing else Just extension)

options :: [OptDescr (Either FilePath (Descr -> IO Descr))]
options =
  [ Option "F" []
    (ReqArg
      (\arg ->
         pure (\opt ->
                 pure $ optHintDescr.hintDescrFS %~
                 (\a ->
                    Left $ fString .~ arg $ fromLeft defaultCartesian a
                 ) $ opt
              )
      )
      "String")
    $ unlines $ ["Function String for 'x dot', F (x,y) = _"
                ,"Default: " ++ show (defaultCartesian ^. fString)]
  , Option "G" []
    (ReqArg
      (\arg ->
         pure (\opt ->
                  pure $ optHintDescr.hintDescrFS %~
                  (\a ->
                     Left $ gString .~ arg $ fromLeft defaultCartesian a
                  ) $ opt
              )
      )
      "String")
    $ unlines $ ["Function String for 'y dot', G (x,y) = _"
                ,"Default: " ++ show (defaultCartesian ^. gString)]
  , Option "P" []
    (NoArg
      (pure
        (\opt -> pure $ optHintDescr.hintDescrFS .~ Right defaultPolar $ opt)
      )
    )
    $ unlines $ ["Set to use polar coordinates in terms of r and theta"
                ,"as r-dot and theta-dot"]
  , Option "R" []
    (ReqArg
      (\arg ->
         pure (\opt ->
                  pure $ optHintDescr.hintDescrFS %~
                  (\a ->
                     Right $ rString .~ arg $ fromRight defaultPolar a
                  ) $ opt
              )
      )
      "String")
    $ unlines $ ["Function String for polar 'r dot', R (r,theta) = _"
                ,"Default: " ++ show (defaultPolar ^. rString)]
  , Option "T" []
    (ReqArg
      (\arg ->
         pure (\opt ->
                  pure $ optHintDescr.hintDescrFS %~
                  (\a ->
                     Right $ rString .~ arg $ fromRight defaultPolar a
                  ) $ opt
              )
      )
      "String")
    $ unlines $ ["Function String for polar 'theta dot', T (r,theta) = _"
                ,"Default: " ++ show (defaultPolar ^. tString)]
  , Option "f" []
    (ReqArg
      (\arg -> pure (\opt -> pure $ optFile .~ breakFileExtension arg $ opt))
      "File")
    $ unlines $ ["PNG file save name"
                ,"Default: " ++ show (
                    let (pre,suf) = defaultDescr ^. optFile
                    in pre ++ fromMaybe "" suf)]
  , Option "a" ["aa"]
    (ReqArg
      (\arg ->
         pure (\opt ->
                  pure $ optHintDescr.hintDescrFD.fdAA .~ read arg $ opt
              )
      )
      "Int")
    $ unlines $ ["Anti-Aliasing"
                ,"Default: " ++
                 show (defaultDescr ^. optHintDescr.hintDescrFD.fdAA)]
  , Option "r" ["res"]
    (ReqArg
      (\arg ->
         pure (\opt ->
                 pure $
                 optHintDescr.hintDescrFD.fdRes .~ read arg ^. tupleToV2 $ opt
              )
      )
      "(Int,Int)")
    $ unlines $
    ["Resolution of output image, needs quotes in cmd line"
    ,"Default: " ++
      show (defaultDescr ^. optHintDescr.hintDescrFD.fdRes.v2ToTuple.to show)]
  , Option "c" ["center"]
    (ReqArg
     (\arg ->
        pure (\opt ->
                pure $
                optHintDescr.hintDescrFD.fdCenter .~ read arg ^. tupleToV2 $ opt
             )
     )
      "(Double,Double)")
    $ unlines $
    ["Center of field View, needs quotes in cmd line"
    ,"Default: " ++
     show (defaultDescr ^. optHintDescr.hintDescrFD.fdCenter.v2ToTuple.to show)]
  , Option "H" ["height"]
    (ReqArg
     (\arg ->
        pure (\opt ->
                pure $ optHintDescr.hintDescrFD.fdHeight .~ read arg $ opt
             )
     )
      "Double")
    $ unlines $ ["Height of field view"
                ,"Default: " ++
                 show (defaultDescr ^. optHintDescr.hintDescrFD.fdHeight)]
  , Option "m" ["multiplier"]
    (ReqArg
     (\arg -> pure (\opt -> pure $ optLogMultiplier .~ Just (read arg) $ opt))
      "Double")
    $ unlines $
    ["Multiplier of log function to convert colors,"
    ,"a higher number means less intensity variability "
    ,"between low and high magnitude vectors"
    ,"Default: " ++ show (defaultDescr ^. optLogMultiplier)]
  , Option "" ["write-json"]
    (NoArg
      (pure (\opt -> pure $ optWriteJSON .~ True $ opt))
    ) "Write JSON description of field equations"
  , Option "" ["read-json"]
    (ReqArg
      (\arg -> Left arg)
      "FILE")
    "Read JSON description of field equations"
  , Option "" ["stop"]
    (NoArg
      (pure (\opt -> pure $ optStop .~ True $ opt))
    ) "Don't generate field image"
  , Option "h" ["help"]
    (NoArg
      (pure (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      ))
    ) "Show help"
  ]
