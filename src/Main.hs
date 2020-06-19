import System.IO  
import Control.Monad
import Data.Char
import System.Environment

import Lexer (lexor)
import Parser (parse)
import Evaluator (evaluate)
import PPrinter (pprint)

doNormal :: String -> IO()
doNormal s = do
   handle <- openFile s ReadMode
   contents <- hGetContents handle
   outputFile <- openFile "assembly.s" WriteMode
   hPutStrLn outputFile (evaluate $ parse $ lexor contents)
   hClose outputFile
   hClose handle

doTest :: String -> IO()
doTest s = do
   handle <- openFile s ReadMode
   contents <- hGetContents handle

   print("-------------")
   putStrLn contents
   print("-------------")
   print (lexor contents)
   print("-------------")
   print (parse $ lexor contents)
   pprint (parse $ lexor contents)
   putStrLn (evaluate $ parse $ lexor contents)

   hClose handle


main :: IO()
main = do  
        args <- getArgs
        case args of 
           [] -> error "Please provide path to .c file"
           a : [] -> doNormal a
           a : b : [] -> doTest a