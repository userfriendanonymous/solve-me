module Binary where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)

data Value
    = Add
    | Sub
    | Mul

instance Show Value where
  show v = case v of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"

solve :: Value -> Int -> Int -> Int
solve v = case v of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)

random :: Effect Value
random = do
    r <- randomInt 0 2
    pure $ case r of
        0 -> Add
        1 -> Sub
        _ -> Mul