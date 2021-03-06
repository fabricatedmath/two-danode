module Field.Hint.Repa
  ( buildPhaseSpace, InterpreterError(..)
  ) where

import Data.Array.Repa hiding ((++))
import Data.Vector.Unboxed hiding ((++))

import Language.Haskell.Interpreter

import Linear

import Field
import Field.Hint

buildPhaseSpace
  :: HintDescr Double
  -> IO (Either InterpreterError (Array U DIM3 (V2 Double)))
buildPhaseSpace hintDescr =
  do
    let
      fd = _hintDescrFD hintDescr
      fieldS = _hintDescrFS hintDescr
      funcS = createFunction fieldS
      dim =
        let V2 y x = _fdRes fd
            aa' = _fdAA fd
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
                   , "Field.Repa"
                   ]
        interpret command (as :: Vector (V2 Double))
    pure $ fromUnboxed dim <$> result
