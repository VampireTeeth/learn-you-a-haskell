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

instance (Eq m) => Eq (Main.Maybe m) where
    Main.Just x == Main.Just y = x == y
    Main.Nothing == Main.Nothing = True
    _ == _ = False

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


data Either a b = Left a | Right b deriving (Eq, Ord, Read)
instance (Show a, Show b) => Show (Main.Either a b) where
    show (Main.Left a) = "Main.Left " ++ show a 
    show (Main.Right b) = "Main.Right " ++ show b

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



infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right) 
  | x == y = Node x left right
  | x < y  = Node y (treeInsert x left) right
  | x > y  = Node y left (treeInsert x right)

infixr 5 .+
(.+) :: (Ord a) => a -> Tree a -> Tree a
x .+ t = treeInsert x t

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x < y =  treeElem x left
  | x > y =  treeElem x right

data TrafficLight = Yellow | Red | Green
instance Eq TrafficLight where
    Yellow == Yellow = True
    Red == Red = True
    Green == Green = True
    _ == _ = False


class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance Functor Main.Maybe where
    fmap f (Main.Just x) = Main.Just (f x)
    fmap f Main.Nothing = Main.Nothing

instance Functor (Main.Either a) where
    fmap f (Main.Left x) = Main.Left x
    fmap f (Main.Right x) = Main.Right (f x)


class Tofu t where
    tofu :: j a -> t a j

data Frank a j = Frank {frankField :: j a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba :: t k} deriving (Show)

instance Functor (Barry t k) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

