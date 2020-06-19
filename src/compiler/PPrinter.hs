module PPrinter (pprint) where

import System.IO

import Parser (Tree(..))

pprint :: Tree -> IO()
pprint tree = do
    print "-------------"
    putStr (strTree 0 tree)
    putStr "\n-------------\n"


getIndent :: Int -> String
getIndent n = concat $ replicate n "    "

strTree :: Int -> Tree -> String

strTree n (ProgNode tree) = strTree n tree
strTree n (FuncNode name tree) = "FUN " ++ name ++ ":\n" ++ strTree (n+1) tree
strTree n (ReturnNode tree) = (getIndent n) ++ "RETURN " ++ strTree n tree
strTree n (ConstNode num) = "INT<" ++ (show num) ++ ">"
strTree n (UnOpNode char tree) = "(" ++ [char] ++ (strTree n tree) ++ ")"
strTree n (BinOpNode char tree tree') = "(" ++ (strTree n tree) ++ [char] ++ (strTree n tree') ++ ")"