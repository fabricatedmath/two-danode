module Main where

import Field

import qualified Data.Vector.Unboxed as UV

import Linear

import Language.Haskell.Interpreter

main :: IO ()
main =
  do
    let fd = FromCenter (V2 1440 2560) (V2 0 0) 1
        x' :: V2 Float -> Float
        x' (V2 y x) = y

        y' :: V2 Float -> Float
        y' (V2 y x) = sin x

    let v = UV.sum $ buildField fd x' y'
    print v
    --v `seq` return ()
