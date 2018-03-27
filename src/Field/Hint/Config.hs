{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Field.Hint.Config
  ( loadHintDescrFromArgs
  , hintDescrOptions
  )
where

import Control.Lens
import Control.Monad (when)

import Data.Aeson (decode', ToJSON(..), FromJSON(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)

import System.Console.GetOpt
import System.Exit

import Field
import Field.Hint

data Descr a =
  Descr
  { _optDescr :: HintDescr a
  , _optWriteJSON :: Bool
  , _optStop :: Bool
  } deriving (Show)

makeLenses ''Descr

defaultDescr :: Num a => Descr a
defaultDescr =
  Descr
  { _optDescr = defaultHintDescr
  , _optWriteJSON = False
  , _optStop = False
  }

--TODO: base 4.11 introduced theses into Data.Maybe
fromLeft :: a -> Either a b -> a
fromLeft a (Right _) = a
fromLeft _ (Left a) = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b (Left _) = b

loadHintDescrFromArgs
  :: (ToJSON a, Num a, RealFrac a, Read a, FromJSON a, Show a)
  => FilePath --out file path
  -> [String]
  -> IO (HintDescr a)
loadHintDescrFromArgs outfp args =
  do
    let (actions,_,_) = getOpt RequireOrder hintDescrOptions args
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
                  Just json -> pure $ (optDescr .~ json) descr
            Nothing -> pure descr
        foldl (>>=) (pure descr') rights
    when (_optWriteJSON descr) $
      do
        BS.writeFile (outfp ++ ".json") $
          encodePretty (_optDescr descr)
        putStrLn "Wrote json file"
    when (_optStop descr) $
      do
        putStrLn "Stopping due to flag"
        exitSuccess
    return $ _optDescr descr

hintDescrOptions
  :: forall a. (RealFrac a, Read a, Show a)
  => [OptDescr (Either FilePath (Descr a -> IO (Descr a)))]
hintDescrOptions =
  let def :: HintDescr a
      def = defaultHintDescr
  in
  [ Option "F" []
    (ReqArg
      (\arg ->
         pure (\opt ->
                 pure $ optDescr.hintDescrFS %~
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
                  pure $ optDescr.hintDescrFS %~
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
        (\opt -> pure $ optDescr.hintDescrFS .~ Right defaultPolar $ opt)
      )
    )
    $ unlines $ ["Set to use polar coordinates in terms of r and theta"
                ,"as r-dot and theta-dot"]
  , Option "R" []
    (ReqArg
      (\arg ->
         pure (\opt ->
                  pure $ optDescr.hintDescrFS %~
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
                  pure $ optDescr.hintDescrFS %~
                  (\a ->
                     Right $ rString .~ arg $ fromRight defaultPolar a
                  ) $ opt
              )
      )
      "String")
    $ unlines $ ["Function String for polar 'theta dot', T (r,theta) = _"
                ,"Default: " ++ show (defaultPolar ^. tString)]
  , Option "a" ["aa"]
    (ReqArg
      (\arg ->
         pure (\opt ->
                  pure $ optDescr.hintDescrFD.fdAA .~ read arg $ opt
              )
      )
      "Int")
    $ unlines $ ["Anti-Aliasing"
                ,"Default: " ++
                 show (def ^. hintDescrFD.fdAA :: Int)]
  , Option "r" ["res"]
    (ReqArg
      (\arg ->
         pure
         (\opt ->
             pure $ optDescr.hintDescrFD.fdRes .~ read arg ^. tupleToV2 $ opt
         )
      )
      "(Int,Int)")
    $ unlines $
    ["Resolution of output image, needs quotes in cmd line"
    ,"Default: " ++
      show (def ^. hintDescrFD.fdRes.v2ToTuple.to show)]
  , Option "c" ["center"]
    (ReqArg
     (\arg ->
        pure
        (\opt ->
            pure $ optDescr.hintDescrFD.fdCenter .~ read arg ^. tupleToV2 $ opt
        )
     )
      "(Double,Double)")
    $ unlines $
    ["Center of field View, needs quotes in cmd line"
    ,"Default: " ++
     show (def ^. hintDescrFD.fdCenter.v2ToTuple.to show)]
  , Option "H" ["height"]
    (ReqArg
     (\arg ->
        pure
        (\opt ->
            pure $ optDescr.hintDescrFD.fdHeight .~ read arg $ opt
        )
     )
      "Double")
    $ unlines $ ["Height of field view"
                ,"Default: " ++
                 show (def ^. hintDescrFD.fdHeight)]
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
  ]
