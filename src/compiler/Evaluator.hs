module Evaluator(evaluate) where

import Parser (Tree(..))

evaluate :: Tree -> String
evaluate (FuncNode fname tree) = 
    let x = evaluate tree 
    in ".globl " ++ fname ++ "\n" ++ fname ++ ":" ++ "\n" ++ x

evaluate (ProgNode tree) =  evaluate tree 

evaluate (ReturnNode tree) =
    let x = evaluate tree 
    in x ++ "    ret"

evaluate (ConstNode x) =  "    mov   $" ++ (show x) ++ ", %rax\n"
evaluate (UnOpNode op tree)
    | op == '-' = evaluate tree ++ "    neg    %rax\n"
    | otherwise = error "WIP"