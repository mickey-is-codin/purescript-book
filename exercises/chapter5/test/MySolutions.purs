module Test.MySolutions where

import Prelude

import Data.Person (Person)
import Data.Picture (Shape(..), getCenter, origin)

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
circleAtOrigin = Circle origin 10.0

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape line@(Line s e) = Line (s - delta) (e - delta)
  where
    delta = getCenter line
centerShape (Text _ text) = Text origin text

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line s e) = Line (s * scale) (e * scale)
  where
    scale = {x: i, y: i}
scaleShape _ text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0