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
strTree n (ExprNode tree) = strTree n tree
strTree n (VarNode s) = s
strTree n (AssignNode s tree) = s ++ " = " ++ (strTree n tree)
strTree n (FuncNode name trees) = "FUN " ++ name ++ ":\n" ++ (concat [(getIndent (n+1)) ++ strTree (n+1) tree ++ "\n" | tree <- trees])
strTree n (ReturnNode tree) = "RETURN " ++ strTree n tree
strTree n (ConstNode num) = "<" ++ (show num) ++ ">"
strTree n (UnOpNode s tree) = "(" ++ s ++ (strTree n tree) ++ ")"
strTree n (BinOpNode s tree tree') = "(" ++ (strTree n tree) ++ s ++ (strTree n tree') ++ ")"
strTree n (DeclNode s mayTree) = 
    case mayTree of 
        Just tree -> "INT " ++ s ++ " = " ++ (strTree n tree)
        Nothing -> "INT " ++ s
