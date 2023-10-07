module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (concat, filter, head, length, null, tail, (..), (:))
import Data.Foldable (foldl, foldr)
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path, filename, isDirectory, ls)
import Test.Examples (allFiles, factors)

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

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ ((a * a) + (b * b) == (c * c))
  pure [a,b,c]

primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
    factorize :: Int -> Int -> Array Int
    factorize _ 1 = []
    factorize fst snd = 
      if snd `mod` fst == 0 then
        fst : (factorize fst (snd / fst))
      else
        factorize (fst + 1) snd

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc curr -> acc && curr) true

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where 
    fib' :: Int -> Int -> Int -> Int -> Int
    fib' limit count n1 n2 = 
      if limit == count then n1 + n2
      else fib' limit (count + 1) n2 (n1 + n2)

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc curr -> [curr] <> acc) []

onlyFiles :: Path -> Array Path
onlyFiles path = filter isFile $ allFiles path
  where isFile = not isDirectory
