data Point = Point {
             x :: Float,
             y :: Float
           } deriving (Show)

data Shape = Circle {
             point :: Point,
             radius :: Float
           } | Rectangle {
             bottomLeft :: Point,
             topRight :: Point
           } deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

data Person = Person{ 
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

data Maybe a = Nothing | Just a deriving (Show)

data Car = Car {
           make :: String,
           model :: String,
           year :: Int
         } deriving (Show)

tellCar :: Car -> String
tellCar (Car make model year) = "This " ++ make ++ " " ++ model ++ " was made in " ++ (show year)


data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vmult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vmult` (Vector l m n) = Vector (i * l) (j * m) (k * n)

vscalarmulti :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `vscalarmulti` (Vector l m n) = i * l + j * m + k * n
