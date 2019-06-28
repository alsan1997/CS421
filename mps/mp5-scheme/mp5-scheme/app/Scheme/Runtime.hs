--partner: Rahul Sirasao

{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

-- TODO
-- You should replace the following line with your own implementation
liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp f = PrimFunc p where
  p [] = throwError $ UnexpectedArgs []
  p [x] = throwError $ UnexpectedArgs [x]
  p xx = Number . foldl1 f <$> mapM lowerInt xx
  
-- TODO
-- You should replace the following line with your own implementation
liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
  p [] = throwError $ UnexpectedArgs []
  p [x] = Number . f <$> lowerInt x
  p xx = throwError $ UnexpectedArgs xx

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean False] = return $ Boolean $ f False
  p [_] = return $ Boolean $ f True
  p v = throwError $ UnexpectedArgs v

-- TODO
liftCompOp :: (Int -> Int -> Bool) -> Val
-- You should replace the following line with your own implementation
liftCompOp f = PrimFunc p where
  p [] = return(Boolean True)
  p body = Boolean . myfold f <$> mapM lowerInt body

myfold f [] = True
myfold f [x] = True   
myfold f (x:y:xs) = case xs of
  [] -> f x y
  _  -> f x y && myfold f (y:xs)

--- ### Primtive operations

-- Primitive function `car`
-- TODO
car :: [Val] -> EvalState Val
car [] = throwError $ UnexpectedArgs []
car [List []] = throwError $ UnexpectedArgs [List []]
car [List(x:xs)] = return x
car [(DottedList (x:xs) y)] = return x

-- Primitive function `cdr`
-- TODO
cdr :: [Val] -> EvalState Val
cdr [] = throwError $ UnexpectedArgs []
cdr [List []] = throwError $ UnexpectedArgs [List []]
cdr [List(x:xs)] = return(List xs)
cdr [(DottedList (x:xs) y)] = return((DottedList xs y))

-- Primitive function `cons`
-- TODO
cons :: [Val] -> EvalState Val
cons [x] = return x
cons (x:xs) = (DottedList [x] <$> (cons xs))

list :: [Val] -> EvalState Val
list [] = return $ List []
list [x] = return $ List [x]
list vv = return $ List vv


-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
  append' (List []) x = return x
  append' (List xs) (List ys) = return $ List (xs ++ ys)
  append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
  append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
-- TODO
-- Examples:
--   (apply + '(1 2 3))  => 6
--   (apply car '((1 2 3)))  => 1
applyPrim :: [Val] -> EvalState Val
applyPrim [PrimFunc f, List(args)] = f args
applyPrim [f, List(args)] = apply f args

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6
evalPrim :: [Val] -> EvalState Val
evalPrim [] = eval Void
evalPrim [List args] = eval $ List args
evalPrim [arg] = eval arg

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
-- TODO
-- Examples:
--   (= 1 1) => #t
--   (= #f #t) => #f
--   (= #f #f) => #t
--   (= 'a 10) => Type error
--   (= 'a 'b) => Type error
equalSign :: [Val] -> EvalState Val
equalSign [] = return(Boolean True)
equalSign [x] = return(Boolean True)
equalSign vv =
  if not (isValid vv)
    then throwError $ TypeError (head vv)
    else return $ Boolean (eqSign vv) where
      eqSign [] = True
      eqSign [x] = True
      eqSign ((Boolean x):(Boolean y):xs) = ((x == y) && (eqSign ((Boolean y):xs)))
      eqSign ((Number x):(Number y):xs) = ((x == y) && (eqSign ((Number y):xs)))

isValid [] = True
isValid [x] = True
isValid (x:y:xs) =
  if (((typeName x == "Number") || (typeName x == "Boolean")) && ((typeName x) == (typeName y)))
    then True && (isValid (y:xs))
    else False

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
-- TODO
-- Examples:
--   (eq? 1 1) => #t
--   (eq? #f #t) => #f
--   (eq? #f #f) => #t
--   (eq? 'a 10) => #f
--   (eq? 'a 'a) => #t
eq :: [Val] -> EvalState Val
eq [] = return(Boolean True)
eq [x] = return(Boolean True)
eq vv = return . Boolean $ eq' vv where
  eq' [] = True
  eq' [x] = True
  eq' ((Symbol x):(Symbol y):xs) = ((x == y) && (eq' ((Symbol y):xs)))
  eq' ((Boolean x):(Boolean y):xs) = ((x == y) && (eq' ((Boolean y):xs)))
  eq' ((Number x):(Number y):xs) = ((x == y) && (eq' ((Number y):xs)))
  eq' _ = False

-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
-- TODO
isList :: [Val] -> EvalState Val
isList [x] =
  return . Boolean $ case flattenList x of
    List _ -> True
    _ -> False
isList xx = throwError $ UnexpectedArgs xx

-- Primitive function `symbol?` predicate
-- TODO
isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol _] = return(Boolean True)
isSymbol [_] = return(Boolean False)
isSymbol vv = throwError $ UnexpectedArgs vv

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
-- TODO
isPair :: [Val] -> EvalState Val
isPair [] = throwError $ UnexpectedArgs []
isPair args@[x, xs] = throwError $ UnexpectedArgs args
isPair [(Number x)] = return(Boolean False)
isPair [(DottedList xx y)] = return . Boolean $ ((1 + length xx) == 2)
isPair [(List [])] = return(Boolean False)
isPair [(List [x])] = return(Boolean False)
isPair [(List xx)] = return . Boolean $ (length xx == 2)
isPair [x] = throwError $ UnexpectedArgs [x]

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber [Number _] = return(Boolean True)
isNumber [_] = return(Boolean False)
isNumber xx = throwError $ UnexpectedArgs xx

-- Primitive function `boolean?` predicate
-- TODO
isBoolean :: [Val] -> EvalState Val
isBoolean [Boolean _] = return(Boolean True)
isBoolean [_] = return(Boolean False)
isBoolean xx = throwError $ UnexpectedArgs xx

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull :: [Val] -> EvalState Val
isNull [] = throwError $ UnexpectedArgs []
isNull [List []] = return (Boolean True)
isNull [(List xx)] = return (Boolean False)
isNull [x] = return(Boolean False)
isNull xx = throwError $ UnexpectedArgs xx

--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("cons", PrimFunc cons)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     -- TODO: Insert more runtime bindings here
                     , ("*", liftIntVargOp (*) 1)
                     , ("/", liftIntVargOp (div) 1)
                     , (">", liftCompOp (>))
                     , ("<", liftCompOp (<))
                     , (">=", liftCompOp (>=))
                     , ("<=", liftCompOp (<=))
                     , ("car", PrimFunc car)
                     , ("cdr", PrimFunc cdr)
                     , ("not", liftBoolUnaryOp not)
                     , ("eq?", PrimFunc eq)
                     , ("=", PrimFunc equalSign)
                     , ("modulo", liftIntBinOp mod)
                     , ("abs", liftIntUnaryOp abs)
                     , ("number?", PrimFunc isNumber)
                     , ("boolean?", PrimFunc isBoolean)
                     , ("null?", PrimFunc isNull)
                     , ("pair?", PrimFunc isPair)
                     , ("apply", PrimFunc applyPrim)
                     , ("eval", PrimFunc evalPrim)
                     , ("list", PrimFunc list)
                     ]
