--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`
factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\v -> k (n * v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`
evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk xx k1 k2 = aux xx k1 k2
    where aux [x] k1 k2 | x `mod` 2 == 0 = k1 x
                        | otherwise = k2 x 
          aux xx@(x:xs) k1 k2 | x `mod` 2 == 0 = aux xs (\va -> k1 $ va + x) k2
                              | otherwise = aux xs k1 (\vb -> k2 $ vb + x) 

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (LamExp _ _) = True
isSimple (AppExp _ _) = False
isSimple (IfExp e1 e2 e3) = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp _ e2 e3) = isSimple e2 && isSimple e3

--- ### Define `cpsExp` - Overview
cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k fresh = (AppExp k (IntExp i), fresh)
cpsExp (VarExp v) k fresh = (AppExp k (VarExp v), fresh) 

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f1 e1) k fresh | isSimple e1 == True = (AppExp (AppExp f1 e1) k, fresh)
                              | otherwise = cpsExp e1 (LamExp v arg2) fresh2
                                                where (v, fresh2) = gensym fresh
                                                      arg2 = AppExp (AppExp f1 (VarExp v)) k

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k fresh = 
    let b1 = isSimple e1
        b2 = isSimple e2
    in case (b1, b2) of
        (True, True) -> (AppExp k (OpExp op e1 e2), fresh)
        (False, True) -> cpsExp e1 (LamExp v arg2) fresh2
                            where (v, fresh2) = gensym fresh
                                  arg2 = AppExp k (OpExp op (VarExp v) e2)
        (True, False) -> cpsExp e2 (LamExp v arg2) fresh2
                            where (v, fresh2) = gensym fresh
                                  arg2 = AppExp k (OpExp op e1 (VarExp v))
        (False, False) -> let (v1, fresh2) = gensym fresh
                              (v2, fresh3) = gensym fresh2
                              arg2 = AppExp k (OpExp op (VarExp v1) (VarExp v2))
                              (e2Cps, fresh4) = cpsExp e2 (LamExp v2 arg2) fresh3
                          in cpsExp e1 (LamExp v1 e2Cps) fresh4

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k fresh =
    let b1 = isSimple e1
    in case b1 of
        True -> let (e2Cps, fresh2) = cpsExp e2 k fresh
                    (e3Cps, fresh3) = cpsExp e3 k fresh
                in (IfExp e1 e2Cps e3Cps, fresh)
        False -> let (v, fresh1) = gensym fresh
                     (e2Cps, fresh2) = cpsExp e2 k fresh1
                     (e3Cps, fresh3) = cpsExp e3 k fresh1
                 in cpsExp e1 (LamExp v (IfExp (VarExp v) e2Cps e3Cps)) fresh1

--- ### Define `cpsDecl`
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) = Decl f (params ++ ["k"]) first
    where (first, _) = cpsExp body (VarExp "k") 1
 
