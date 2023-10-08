module Test.MySolutions where

import Prelude

import Data.Person (Person)
import Data.Picture (Shape(..))

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
             | otherwise = factorial n / (factorial k * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
-- sameCity person1 person2 = person1.address.city == person2.address.city
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default

circleAtOrigin :: Shape
circleAtOrigin = Circle { x: 0.0, y: 0.0 } 10.0