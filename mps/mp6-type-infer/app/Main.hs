
module Main where

import Common
import Infer
import Parser

import Data.Map.Strict as H (Map, empty)

step :: TypeEnv -> IO TypeEnv
step env = do
  putStrLn "Enter an expression:"
  line <- getLine
  let d = parse (lexer line)
  putStrLn "Parsed as:"
  putStrLn ("  " ++ (stringOfDec d))
  case runFVState (inferDec env d) 0 of
    ValidRes (env', tau) _ -> do
      putStrLn "Inferred type:"
      putStrLn ("  " ++ (stringOfMonoTy (canonize tau) tau))
      return env'
    ErrorRes e -> do
      putStrLn "Error while type inferencing:"
      putStrLn ("  " ++ (stringOfError e))
      return env

repl :: TypeEnv -> IO ()
repl env = do
  env' <- step env
  repl env'

main :: IO ()
main = repl H.empty