module Lib
    ( Exp(..)
    , parse
    ) where

import Text.Regex.TDFA

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if s == x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | let var E E

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving (Show,Eq)

parse xx = parseE (words xx)

parseE ("+":xs) =
  let (e1,r1) = parseE xs
      (e2,r2) = parseE r1
   in (PlusExp e1 e2, r2)

parseE (x:xs) | isInt x =
                (IntExp (read x), xs)

parseE ("let":xs) =
  let (VarExp s, r1)=parseE xs
      (e2,r2)=parseE r1
      (e3,r3)=parseE r2
  in (LetExp s e2 e3, r3)

parseE (x:xs) = (VarExp x, xs)