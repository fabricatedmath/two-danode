{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Field.Hint where

import Control.Lens

import Data.Aeson
  (ToJSON, FromJSON, toEncoding, defaultOptions, genericToEncoding)

import GHC.Generics (Generic)

import Field

data HintDescr a =
  HintDescr
  { _hintDescrFD :: FieldDescription a
  , _hintDescrFS :: FieldStrings
  } deriving (Generic, Show, Read)

instance ToJSON a => ToJSON (HintDescr a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (HintDescr a)

data FieldStrings =
  Cartesian
  { _fString :: String
  , _gString :: String
  } |
  Polar
  { _rString :: String
  , _tString :: String
  } deriving (Generic, Show, Read)

instance ToJSON FieldStrings where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldStrings

makeLenses ''HintDescr

createFunction :: FieldStrings -> String
createFunction (Polar rs ts) =
    "(\\(V2 y x) -> let " ++
    "r = sqrt $ x*x + y*y" ++ "; " ++
    "theta = atan2 y x" ++ "; " ++
    "rdot = " ++ rs ++ "; " ++
    "thetadot = " ++ ts ++ "; " ++
    "xdot = rdot*cos theta - r*sin theta * thetadot" ++ "; " ++
    "ydot = rdot*sin theta + r*cos theta * thetadot" ++ "; " ++
    "in V2 ydot xdot)"
createFunction (Cartesian fs gs) =
    "(\\(V2 y x) -> let " ++
    "xdot = " ++ fs ++ "; " ++
    "ydot = " ++ gs ++ "; " ++
    "in V2 ydot xdot)"
