module Point where

import Prelude

newtype Point = Point { x :: Int, y :: Int }
instance eqPoint :: Eq Point where
  eq (Point { x, y }) (Point { x: x2, y: y2}) = x == x2 && y == y2

instance ordPoint :: Ord Point where
  compare p1@(Point { x, y }) p2@(Point { x: x2, y: y2}) 
    | p1 == p2 = EQ
    | x < x2 || y < y2 = LT
    | otherwise = GT

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> (show x) <> ", " <> (show y) <> ")"

point :: Int -> Int -> Point
point x y = Point { x, y }