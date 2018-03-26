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
  , _hintDescrFS :: Either Cartesian Polar
  } deriving (Generic, Show, Read)

data Cartesian =
  Cartesian
  { _fString :: String
  , _gString :: String
  } deriving (Generic, Show, Read)

data Polar =
  Polar
  { _rString :: String
  , _tString :: String
  } deriving (Generic, Show, Read)

makeLenses ''HintDescr
makeLenses ''Cartesian
makeLenses ''Polar

defaultHintDescr :: Num a => HintDescr a
defaultHintDescr =
  HintDescr
  { _hintDescrFD = defaultFieldDescription
  , _hintDescrFS = Left defaultCartesian
  }

defaultCartesian :: Cartesian
defaultCartesian =
  Cartesian
  { _fString = "y"
  , _gString = "-sin x"
  }

defaultPolar :: Polar
defaultPolar =
  Polar
  { _rString = "r*(1-r*r)"
  , _tString = "1"
  }

instance ToJSON a => ToJSON (HintDescr a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (HintDescr a)

instance ToJSON Cartesian where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Cartesian

instance ToJSON Polar where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Polar

createFunction :: Either Cartesian Polar -> String
createFunction =
  either createFunctionCartesian createFunctionPolar

createFunctionCartesian :: Cartesian -> String
createFunctionCartesian (Cartesian fs gs) =
  "(\\(V2 y x) -> let " ++
  "xdot = " ++ fs ++ "; " ++
  "ydot = " ++ gs ++ "; " ++
  "in V2 ydot xdot)"

createFunctionPolar :: Polar -> String
createFunctionPolar (Polar rs ts) =
  "(\\(V2 y x) -> let " ++
  "r = sqrt $ x*x + y*y" ++ "; " ++
  "theta = atan2 y x" ++ "; " ++
  "rdot = " ++ rs ++ "; " ++
  "thetadot = " ++ ts ++ "; " ++
  "xdot = rdot*cos theta - r*sin theta * thetadot" ++ "; " ++
  "ydot = rdot*sin theta + r*cos theta * thetadot" ++ "; " ++
  "in V2 ydot xdot)"
