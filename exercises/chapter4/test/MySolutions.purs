module Test.MySolutions where

import Prelude

import Data.Array (concat, filter, head, length, null, tail)
import Data.Maybe (fromMaybe)
import Test.Examples (factors)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n = 
  if n >= 0 then n `mod` 2 == 0
  else isEven (-1 * n)

countEven :: Array Int -> Int
countEven xs = 
  if null xs then 0
  else 
    if isEven safeArrayFirstValue then 1 + countEven safeArrayTail
    else 0 + countEven safeArrayTail
      where
        safeArrayFirstValue = fromMaybe 1 $ head xs
        safeArrayTail = fromMaybe [] $ tail xs

countEven' :: Array Int -> Int
countEven' xs = 
  if null xs then 0
  else oneIfEven safeArrayFirstValue + countEven safeArrayTail
    where
      oneIfEven n = if isEven n then 1 else 0
      safeArrayFirstValue = fromMaybe 1 $ head xs
      safeArrayTail = fromMaybe [] $ tail xs

squared :: Array Number -> Array Number
squared = map square
  where square x = x * x

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter isPositive
  where
    isPositive :: Number -> Boolean
    isPositive x = x >= 0.0

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = isPositive <$?> xs
  where
    isPositive :: Number -> Boolean
    isPositive x = x >= 0.0

isPrime :: Int -> Boolean
isPrime x = 
  if x == 0 then false
  else if x == 1 then false
  else if x == 2 then true
  else (length $ factors x) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure [a, b]