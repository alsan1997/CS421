import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import Data.List (intersperse)

data Entity = Var String
            | Object String [Entity]
       deriving (Eq)

instance Show Entity where
   show (Var s) = s
   show (Object s []) = s
   show (Object f xx) = concat $ f : "(" : intersperse "," (map show xx) ++ [")"]

isVar (Var _) = True
isVar _ = False

-- Environment functions

type Env = H.HashMap String Entity

initial :: Env
initial = H.empty

add :: String -> Entity -> Env -> Env
add x y b = H.insert x y b

contains :: String -> Env -> Bool
contains x b = H.member x b

-- Functions you get to write

phi :: Env -> Entity -> Entity
phi env (Var s) | contains s env =  H.lookup s env
phi env x@(Var s) = x
phi env (Object s xx) = Object s (map (phi env) xx)

occurs :: String -> Entity -> Bool
occurs carkeys (Var s) = s == carkeys
occurs carkeys (Object s xx) = any (occurs carkeys) xx

unify :: [(Entity,Entity)] -> Env
unify [] = initial
unify ((s,t):c') = undefined
