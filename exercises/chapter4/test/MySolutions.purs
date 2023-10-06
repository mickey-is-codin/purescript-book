module Test.MySolutions where

import Prelude

import Data.Array (head, tail, null)
import Data.Maybe (fromMaybe)

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