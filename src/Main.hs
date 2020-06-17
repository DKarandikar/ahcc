import System.IO  
import Control.Monad
import Data.Char
import System.Environment

import Lexer (lexor)
import Parser (parse)
import Evaluator (evaluate)

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

   print contents
   print (lexor contents)
   print (parse $ lexor contents)
   print (evaluate $ parse $ lexor contents)

   hClose handle


main :: IO()
main = do  
        args <- getArgs
        case args of 
           [] -> error "Please provide path to .c file"
           a : [] -> doNormal a
           a : b : [] -> doTest a