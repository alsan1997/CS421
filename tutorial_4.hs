import Data.List
import System.IO 

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

-- [1,2,3,4]: x = 1 | xs = [2,3,4]
-- [2,3,4] : x = 2 | xs = [3,4]
-- [3,4] : x = 3 | xs = [4]
-- [4] : x = 4 | xs = []

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int --Parameter receive a function that takes an Int and return an Int
doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3
fourPlus3 = adds3 4

threePlusList = map adds3 [1,2,3,4,5]

dbl1To10 = map (\x -> x * 2) [1..10]

-- < > <= >= == /= && || not otherwise

doubleEvenNumber y =
 if (y `mod` 2 /= 0)
 then y
 else y * 2

getClass :: Int -> String 
getClass n = case n of
 5 -> "Go to Kindergarden"
 6 -> "Go to elementary school"
 _ -> "Go away"

-- module SampFunctions (getClass, doubleEvennumbers) where
-- import SampFunctions

data BaseballPlayer = Pitcher | Catcher | Infielder | Outfield deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOF = print(barryBonds Outfield)

data Customer = Customer String String Double deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double 
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Rock Scissors = "Rock Beats Scissors"
shoot Scissors Paper = "Scissors Beat Paper"
shoot Scissors Rock = "Scissors Loses to Rock"
shoot Paper Scissors = "Paper Loses to Scissors"
shoot Rock Paper = "Rock Loses to Paper"
shoot _ _ = "Error"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show
-- First two float is the center coordinate. For rectangle, each four floats represent each coordinates
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs(y2 - y))

-- dot product and dollar sign

sumValue = putStrLn (show(1+2))
sumValue2 = putStrLn . show $ 1 + 2

--
data Employee = Employee {name :: String, position :: String, idNum :: Int} deriving(Eq, Show)

samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001}

isSamPam = samSmith == pamMarx
samSmithData = show samSmith

--
data ShirtSize =  S | M | L

instance Eq ShirtSize where
 S == S = True
 M == M = True
 L == L = True
 _ == _ = False

instance Show ShirtSize where
 show S = "Small"
 show M = "Medium"
 show L = "Large"

smallAvail = S `elem` [S, M, L]
theSize = show S

--
class MyEq a where
 areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
 areEqual S S = True
 areEqual M M  = True
 areEqual L L = True
 areEqual _ _ = False

newSize = areEqual M M