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
    if isEven blah then 1 + countEven grug
    else 0 + countEven grug
      where
        blah = fromMaybe 1 $ head xs
        grug = fromMaybe [] $ tail xs

countEven' :: Array Int -> Int
countEven' xs = 
  if null xs then 0
  else oneIfEven blah + countEven grug
    where
      oneIfEven n = if isEven n then 1 else 0
      blah = fromMaybe 1 $ head xs
      grug = fromMaybe [] $ tail xs