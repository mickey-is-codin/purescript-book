module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x - 1)