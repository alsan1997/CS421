import Data.List
import System.IO 

addMe :: Int -> Int -> Int
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
whatAge :: Int -> String
sumNaturals :: Int -> Int

-- funcName param1 param2 = operations (returned value)

addMe x y = x + y
subMe x y = x - y -- No function declaration

addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge 16 = "You can Drive"
whatAge 18 = "You can Vote"
whatAge 21 = "You're an adult"
whatAge _ = "Nothing to say"

sumNaturals n = sum [0..n]
