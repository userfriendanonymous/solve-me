module Expr where

import Prelude

import Binary as Binary
import Effect (Effect)
import Effect.Random (randomInt)

data Value
    = Binary Binary.Value Value Value
    | Num Int

instance Show Value where
  show (Binary b x y) = "(" <> show x <> show b <> show y <> ")"
  show (Num n) = show n

solve :: Value -> Int
solve (Binary b x y) = Binary.solve b (solve x) (solve y)
solve (Num n) = n

random :: Int -> Effect Value
random difficulty = do
    r <- randomInt 0 difficulty
    case r of
        0 -> Num <$> randomInt (-20) 20
        _ -> do
            b <- Binary.random
            x <- random (difficulty - 1)
            y <- random (difficulty - 1)
            pure $ Binary b x y