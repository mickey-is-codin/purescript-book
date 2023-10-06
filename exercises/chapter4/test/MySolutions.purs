module Test.MySolutions where

import Prelude

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n = 
  if n >= 0 then n `mod` 2 == 0
  else isEven (-1 * n)