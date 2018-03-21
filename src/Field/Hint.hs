module Field.Hint where

data FieldStrings =
  Cartesian
  { _fString :: String
  , _gString :: String
  } |
  Polar
  { _rString :: String
  , _tString :: String
  } deriving (Show, Read)

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
