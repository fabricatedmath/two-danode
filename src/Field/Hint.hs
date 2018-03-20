module Field.Hint
  (InterpreterError(..),buildPhaseSpace, FieldStrings(..))
where

import Data.Array.Repa (Array,DIM3,U,Z(..),(:.)(..))
import qualified Data.Array.Repa as R
import Data.Vector.Unboxed (Vector)

import Language.Haskell.Interpreter
import Linear

import Field

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

buildPhaseSpace
  :: FieldDescription Double
  -> FieldStrings
  -> IO (Either InterpreterError (Array U DIM3 (V2 Double)))
buildPhaseSpace fd fieldS =
  do
    let funcS = createFunction fieldS
        dim =
          let V2 y x = _res fd
              aa' = _aa fd
          in Z :. y :. x :. aa'*aa' :: DIM3
    result <-
      runInterpreter $
      do
        let command =
              unwords
              [ "toUnboxed $ runIdentity $ buildFieldRepa"
              , "(" ++ show fd ++ ")"
              , funcS
              ]
        setImports [ "Control.Monad.Identity"
                   , "Data.Array.Repa"
                   , "Data.Vector.Unboxed"
                   , "Linear"
                   , "Prelude"
                   , "Field"
                   ]
        interpret command (as :: Vector (V2 Double))
    pure $ (R.fromUnboxed dim) <$> result
    {-case result of
      Left err -> Left err
      Right v -> Right $ R.fromUnboxed dim v
-}
