module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Lib

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parse input of
                            (decl,remain) -> do printLn "Got it!"
                                                printLn . show $ decl
                                                printLn "Remaining input:"
                                                printLn . show $ remain
                                                printLn "That was fun.  Let's do it again."
                         repl


main :: IO ()
main = do putStrLn "Welcome to the LL Parser!"
          repl
          putStrLn "GoodBye!"
