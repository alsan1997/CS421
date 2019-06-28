import Data.List
import System.IO 

sayHello = do
 putStrLn "What's your name"
 name <- getLine
 putStrLn ("Hello " ++ name)

writeToFile = do
 theFile <- openFile ("tutorial_1.hs") WriteMode
 hPutStrLn theFile ("Random line of text")
 hClose theFile

readFromFile = do
 theFile2 <- openFile ("tutorial_1.hs") ReadMode
 contents <- hGetContents theFile2
 putStr contents
 hClose theFile2