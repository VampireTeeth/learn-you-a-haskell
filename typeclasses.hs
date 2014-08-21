import qualified Data.Map as Map

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
         } deriving (Show, Eq, Read)

tellCar :: Car -> String
tellCar (Car make model year) = "This " ++ make ++ " " ++ model ++ " was made in " ++ (show year)


data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vmult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vmult` (Vector l m n) = Vector (i * l) (j * m) (k * n)

vscalarmulti :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `vscalarmulti` (Vector l m n) = i * l + j * m + k * n


data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
left :: Prelude.Either a b -> Prelude.Maybe a
left (Prelude.Left a) = Prelude.Just a
left (Prelude.Right b) = Prelude.Nothing

right :: Prelude.Either a b -> Prelude.Maybe b
right (Prelude.Left a) = Prelude.Nothing
right (Prelude.Right b) = Prelude.Just b


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Prelude.Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Prelude.Nothing -> Prelude.Left $ "Number " ++ show lockerNumber ++ " does not exist!"
        Prelude.Just (state, code) -> if state == Taken
                                   then Prelude.Left $ "Locker " ++ show lockerNumber ++ " is alread taken!"
                                   else Prelude.Right code

lockers :: LockerMap
lockers = Map.fromList [
     (100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]
