module Evaluator(evaluate) where

import Parser (Tree(..))

rax = "%rax"
rcx = "%rcx"

evaluate :: Tree -> String
evaluate (FuncNode fname tree) = 
    let x = evaluate tree 
    in ".globl " ++ fname ++ "\n" ++ fname ++ ":" ++ "\n" ++ x

evaluate (ProgNode tree) =  evaluate tree 

evaluate (ReturnNode tree) =
    let x = evaluate tree 
    in x ++ "    ret"

evaluate (ConstNode x) =  move ('$': (show x)) rax
evaluate (UnOpNode op tree)
    | op == "-" = concat [evaluate tree, doUnOp "neg" rax]
    | op == "~" = concat [evaluate tree, doUnOp "not" rax]
    | op == "!" = concat [evaluate tree, doBinOp "cmpq" "$0" rax, move "$0" rax, doUnOp "sete" "%al"]
    | otherwise = error "Invalid unary operation: " ++ op

evaluate (BinOpNode op tree tree')
    | op == "+" = concat [evaluate tree, push rax, evaluate tree', pop rcx, doBinOp "addq" rcx rax]
    | op == "-" = concat [evaluate tree, push rax, evaluate tree', pop rcx, doBinOp "subq" rax rcx, move rcx rax]
    | op == "/" = concat [evaluate tree, push rax, evaluate tree', move rax rcx, pop rax, doOp "cqto", doUnOp "idivq" rcx]
    | op == "*" = concat [evaluate tree, push rax, evaluate tree', pop rcx, doBinOp "imul" rcx rax]
    | otherwise  = error "Invalid binary operation: " ++ op
    

move :: String -> String -> String
move v1 v2 = concat ["    movq " ++ v1, ", ", v2, "\n"]

push :: String -> String
push v = concat ["    push ", v, "\n"]

pop :: String -> String
pop v = concat ["    pop ", v, "\n"]

doBinOp:: String -> String -> String -> String
doBinOp op v1 v2 = concat ["    ", op, " ", v1, ", ", v2, "\n"]

doUnOp:: String -> String -> String
doUnOp op v = concat ["    ", op, " ", v, "\n"]

doOp:: String -> String
doOp op = concat ["    ", op, "\n"]
