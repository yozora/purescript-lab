module Data.Picture where

import Prelude

import Data.Foldable

data Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x: x, y: y }) =
    "(Point {x: " ++ show x ++ ", y: " ++ show y ++ "})"

data Shape
  = Circle Point Number
  | Rectangle Point Point
  | Line Point Point
  | Text Point String

instance showShape :: Show Shape where
  show (Circle c r) =
    "(Circle " ++ show c ++ " " ++ show r ++ ")"
  show (Rectangle tl br) =
    "(Rectangle " ++ show tl ++ " " ++ show br ++ ")"
  show (Line p1 p2) =
    "(Line " ++ show p1 ++ " " ++ show p2 ++ ")"
  show (Text p text) =
    "(Text " ++ show p ++ " " ++ show text ++ ")"

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

instance showBounds :: Show Bounds where
  show (Bounds b) =
    "(Bounds {top: " ++ show b.top ++
    ", left: " ++ show b.left ++
    ", bottom: " ++ show b.bottom ++
    ", right: " ++ show b.right ++ "})"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point {x: x, y: y}) r) = Bounds
  { top: y - r
  , left: x - r
  , bottom: y + r
  , right: y + r
  }
shapeBounds (Rectangle (Point p1) (Point p2)) = Bounds
  { top: Math.min p1.y p2.y
  , left: Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right: Math.max p1.x p2.x
  }
shapeBounds (Line p1 p2) =
  shapeBounds (Rectangle p1 p2)
shapeBounds (Text (Point {x: x, y: y}) _) = Bounds
  { top: y
  , left: x
  , bottom: y
  , right: x
  }

(\/) :: Bounds -> Bounds -> Bounds
(\/) (Bounds b1) (Bounds b2) = Bounds
  { top: Math.min b1.top b2.top
  , left: Math.min b1.left b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right: Math.max b1.right b2.right
  }

(/\) :: Bounds -> Bounds -> Bounds
(/\) (Bounds b1) (Bounds b2) = Bounds
  { top: Math.max b1.top b2.top
  , left: Math.max b1.left b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right: Math.min b1.right b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top: Global.infinity
  , left: Global.infinity
  , bottom: -Global.infinity
  , right: -Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldr combine emptyBounds
  where
  combine :: Shape -> Bounds -> Bounds
  combine s b = (shapeBounds s) \/ b

type Picture = Array Shape
