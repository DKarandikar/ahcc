module Evaluator(evaluate) where

import Parser (Tree(..))

evaluate :: Tree -> String
evaluate (FuncNode fname tree) = 
    let x = evaluate tree 
    in ".globl " ++ fname ++ "\n" ++ fname ++ ":" ++ "\n" ++ x

evaluate (ProgNode tree) =  evaluate tree 

evaluate (ReturnNode tree) =
    let x = evaluate tree 
    in "    mov   $" ++ x ++ ", %rax\n    ret"

evaluate (ConstNode x) = show x