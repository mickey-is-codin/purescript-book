module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
             | otherwise = factorial n / (factorial k * (factorial (n - k)))