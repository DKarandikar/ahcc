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

evaluate (ConstNode x) =  "    movq   $" ++ (show x) ++ ", %rax\n"
evaluate (UnOpNode op tree)
    | op == "-" = evaluate tree ++ "    neg    %rax\n"
    | op == "~" = evaluate tree ++ "    not    %rax\n"
    | op == "!" = evaluate tree ++ "    cmpq   $0, %rax\n    movq   $0, %rax\n    sete    %al\n"
    | otherwise = error "Invalid unary operation: " ++ op


evaluate (BinOpNode op tree tree')
    | op == "+" = evaluate tree ++ "    push %rax\n" ++ (evaluate tree') ++ "    pop %rcx\n    addq %rcx, %rax\n"
    | op == "-" = evaluate tree ++ "    push %rax\n" ++ (evaluate tree') ++ "    pop %rcx\n    subq %rax, %rcx\n    movq %rcx, %rax\n"
    | op == "/" = evaluate tree ++ "    push %rax\n" ++ (evaluate tree') ++ "    movq %rax, %rcx\n    pop %rax\n    cqto\n    idivq %rcx\n"
    | op == "*" = evaluate tree ++ "    push %rax\n" ++ (evaluate tree') ++ "    pop %rcx\n    imul %rcx, %rax\n"
    | otherwise  = error "Invalid binary operation: " ++ op
    